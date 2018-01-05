module StateStore

open Heaps
open AsyncEventQueue

type Player = Player1 | Player2
type Choice =  HeapIndex * HeapCount
type View =
    | InitialView
    | PendingPlayerView
    | AiView
    | FinishedView

type GameSettings =
    {
        AIEnabled: bool
        AIDifficulty: float
    }
type State =
    {
        Heaps: Heaps
        Error: Error option
        Player: Player
        Settings: GameSettings
        View: View
        PreviousState: State option
        LastChoice: Choice option
    }

type Message =
    | Start of int
    | Reset
    | Undo
    | ChangeSettings of GameSettings
    | Action of Choice
type Dispatch = Message -> unit

let random = System.Random ()

// AI action
let maxIndex seq =
    seq
    |> Seq.mapi (fun i x -> i, x)
    |> Seq.maxBy snd
    |> fst

let rec randomAction (heaps : Heaps) =
    let index = random.Next heaps.Length
    if heaps.[index] = 0
        then randomAction heaps
        else (index, (random.Next (heaps.[index] - 1) + 1))
let aiAction state dispatch =
    let rnd = random.NextDouble ()
    if rnd > state.Settings.AIDifficulty
        then
            let (index, count) = randomAction state.Heaps
            dispatch (Action (index, count))
        else
            let m = Seq.reduce (^^^) state.Heaps
            let (index, count) =
                match m with
                    | 0 -> (maxIndex state.Heaps, 1)
                    | _ -> let k = Seq.findIndex (fun a -> (m ^^^ a) < a) state.Heaps
                           (k, state.Heaps.[k] - (m ^^^ state.Heaps.[k]))
            dispatch (Action (index, count))


/// Swap between player one and two
let swapPlayer = function
    | Player1 -> Player2
    | Player2 -> Player1

/// Initial state of the game
let initialState =
    {
        Heaps = List.empty;
        Error = None;
        Player = Player1;
        Settings =
            {
                AIEnabled = true;
                AIDifficulty = 0.5
            };
        View = InitialView;
        PreviousState = None;
        LastChoice = None
    }

let rec undo state =
    match state.PreviousState with
        | Some state'   ->
            if state'.Settings.AIEnabled && state'.Player = Player2
                then undo state'
                else state'
        | None          -> failwith "There is no previus state"

/// Reduces the state based on the message to a new state
let update state message : State =
    match message with
        | Reset                     -> { state with View = InitialView;
                                                    Heaps = [];
                                                    Error = None;
                                                    PreviousState = None;
                                                    LastChoice = None }
        | ChangeSettings settings   -> { state with Settings = settings }
        | Start count               -> { state with Heaps = createHeap count;
                                                    View = PendingPlayerView }
        | Undo                      -> undo state
        | Action (index, count)     ->
            match removeMatches index count state.Heaps with
                | Left errorMessage -> { state with Error = Some errorMessage }
                | Right heaps'      ->
                    let state' = { state with Heaps = heaps';
                                              Error = None;
                                              PreviousState = Some state;
                                              LastChoice = Some (index, count) }
                    if gameOver state'.Heaps
                        then { state' with View = FinishedView; }
                        else { state' with Player = swapPlayer state'.Player;
                                           View = PendingPlayerView; }

let middleware dispatch update state message =
    let state' = update state message
    if state'.Settings.AIEnabled && state'.Player = Player2 && state'.View = PendingPlayerView
        then aiAction state' dispatch; { state' with View = AiView }
        else state'

let createDispatch (events : AsyncEventQueue<Message>) : Dispatch = events.Post

/// Function to start and run the game
let start (ui : State -> Dispatch -> unit) =
    let events : AsyncEventQueue<Message> = AsyncEventQueue()
    let dispatch = createDispatch events
    ui initialState dispatch
    let rec run state =
        async {
            let! msg = events.Receive ()
            let state' = middleware dispatch update state msg
            if state <> state' && state.Settings = state'.Settings
                then ui state' dispatch
                else System.Console.WriteLine ("No state changes\n" + string state + "\n" + string state'); ()
            return! run state'
        }
    run initialState