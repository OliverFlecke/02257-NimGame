module StateStore 

open Heaps
open AsyncEventQueue 

type Player = Player1 | Player2

type Message =
    | Start of int
    | Reset
    | Action of HeapIndex * HeapCount

type View =
    | InitialView
    | PendingPlayerView
    | AiView
    | FinishedView
type State =
    {
        Heaps: Heaps
        Error: Error option
        Player: Player
        AiMode: bool
        View: View
    }

let events : AsyncEventQueue<Message> = AsyncEventQueue()

// AI action
let maxIndex seq =
    seq
    |> Seq.mapi (fun i x -> i, x)
    |> Seq.maxBy snd
    |> fst
let aiAction (heaps: Heaps) =
    let m = Seq.reduce (^^^) heaps
    let (index, count) =
        match m with
            | 0 -> (maxIndex heaps, 1)
            | _ -> let k = Seq.findIndex (fun a -> (m ^^^ a) < a) heaps
                   (k, heaps.[k] - (m ^^^ heaps.[k]))
    events.Post (Action (index, count))

/// Swap between player one and two
let swapPlayer = function
    | Player1 -> Player2
    | Player2 -> Player1

/// Initial state of the game 
let initialState = { Heaps = List.empty; Error = None; Player = Player1; AiMode = false; View = InitialView }

/// Reduces the state based on the message to a new state
let reducer state message : State =
    match message with
        | Start count           -> { initialState with Heaps = createHeap count; View = PendingPlayerView }
        | Action (index, count) ->
            match removeMatches index count state.Heaps with
                | Left errorMessage -> { state with Error = Some errorMessage }
                | Right heaps'      ->
                    let state' = { state with Heaps = heaps'; Error = None }
                    if gameOver state'.Heaps
                        then { state' with View = FinishedView }
                        else { state' with Player = swapPlayer state'.Player; View = PendingPlayerView }
        | Reset                 -> initialState

let middleware reducer state message =
    let state' = reducer state message
    if state'.AiMode && state'.Player = Player2 && state'.View = PendingPlayerView
        then aiAction state'.Heaps; { state' with View = AiView }
        else state'

/// Function to start and run the game 
let start (ui : State -> unit) =
    ui initialState
    let rec run state =
        async {
            let! msg = events.Receive ()
            let newState = middleware reducer state msg
            ui newState
            return! run newState
        }
    run initialState
