open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing
open System.Collections

type Heaps = int list
type HeapIndex = int
type HeapCount = int
type Player = Player1 | Player2

type Message =
    | Start of int
    | Reset
    | Action of HeapIndex * HeapCount

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b
type Error = string
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


type AsyncEventQueue<'T>() =
    let mutable cont = None
    let mutable history = []
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() =
        match queue.Count, cont with
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d ->
            cont <- None
            d (queue.Dequeue())
    let tryListen(d) =
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.History () = history
    member x.Post msg = queue.Enqueue msg; history <- msg :: history; tryTrigger()
    member x.Receive() =
        Async.FromContinuations (fun (cont,econt,ccont) ->
            tryListen cont)

let ev : AsyncEventQueue<Message> = AsyncEventQueue()
let rnd = System.Random ()

let createHeap n : Heaps = List.init n (fun _ -> rnd.Next(1, 20))

let removeMatches index count heaps : Either<Error, Heaps> =
    if index < 0 || index > (List.length heaps) - 1
        then Left "Index out of bounds"
        else if count < 1 || heaps.[index] = 0
            then Left "Invalid count"
            else
                let newHeaps = List.toArray heaps
                Array.set newHeaps index (max 0 (heaps.[index] - (max 0 count)))
                Right (Array.toList newHeaps)

let gameOver = List.forall ((=) 0)

let swapPlayer = function
    | Player1 -> Player2
    | Player2 -> Player1

// AI moves
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
    ev.Post (Action (index, count))

let heapsToString heaps = String.concat "   " (List.map string heaps)

// Nim Game GUI
let window = new Form(Text="Nim Game", Size=Size(500,500))

let pendingPlayerGUI state =
    window.Controls.Clear ()
    let playerLabel = new Label(Location=Point(20, 20), Text=((string state.Player) + "'s turn"))
    window.Controls.Add playerLabel

    // Heaps view
    let heapLabel = new Label(Location=Point(20, 50), Text=heapsToString state.Heaps, MinimumSize=Size(500, 20))
    window.Controls.Add heapLabel

    // Input boxes
    let indexLabel = new Label(Location=Point(20, 80), Text="Index")
    let indexBox   = new TextBox(Location=Point(20, 110), Size=Size(200,20))
    window.Controls.Add indexLabel
    window.Controls.Add indexBox

    let countLabel = new Label(Location=Point(20, 140), Text="Count")
    let countBox   = new TextBox(Location=Point(20, 170), Size=Size(200,20))
    window.Controls.Add countLabel
    window.Controls.Add countBox

    // Error handling
    match state.Error with
        | Some errorMessage ->
            let errorLabel = new Label(Location=Point(20, 200), Text=errorMessage)
            window.Controls.Add errorLabel
        | None              -> ()

    let submitButton = new Button(Location=Point(20, 250), Text="Submit")
    window.Controls.Add submitButton
    submitButton.Click.Add (fun _ -> ev.Post (Action ((int indexBox.Text) - 1, (int countBox.Text))))

let startGUI () =
    window.Controls.Clear ()
    let numberOfHeapsLabel = new Label(Location=Point(20, 20), Text="Enter the number of heaps to play:",
                                        MinimumSize=Size(300, 20))
    window.Controls.Add numberOfHeapsLabel
    let numberOfHeapsBox = new TextBox(Location=Point(20, 40), MinimumSize=Size(50, 20), Text="5")
    window.Controls.Add numberOfHeapsBox

    let startButton = new Button(Location=Point(20, 70), MinimumSize=Size(100,50), MaximumSize=Size(100,50), Text="Start")
    window.Controls.Add startButton
    startButton.Click.Add (fun _ -> ev.Post (Start (int numberOfHeapsBox.Text)))

let finishedGUI state =
    window.Controls.Clear ()
    let winnerLabel = new Label(Location=Point(20, 20), Text=((string state.Player) + " is the winner!"), MinimumSize=Size(300,20))
    window.Controls.Add winnerLabel

    let resetButton = new Button(Location=Point(20, 50), Text="Reset")
    window.Controls.Add resetButton
    resetButton.Click.Add (fun _ -> ev.Post Reset)

let nimGameGUI state =
    match state.View with
        | InitialView                   -> startGUI ()
        | PendingPlayerView | AiView    -> pendingPlayerGUI state
        | FinishedView                  -> finishedGUI state

// Command line interface for the game
let printGameState state =
    match state.Error with
        | Some errorMsg -> Console.WriteLine errorMsg
        | None                  -> ()
    Console.WriteLine ""
    Console.WriteLine (string state.Player + "'s turn.")
    Console.WriteLine ("Heaps: " + heapsToString state.Heaps)

let nimGameCommandLineInterface state =
    match state.View with
        | InitialView            ->
            Console.WriteLine "Welcome to Nim!"
            Console.WriteLine "Enter the number of heaps you want to play: "
            let numberOfHeaps = int (Console.ReadLine ())
            ev.Post (Start numberOfHeaps)
        | PendingPlayerView    ->
            printGameState state
            Console.WriteLine "Choose a heap index... "
            let index = int (Console.ReadLine ())
            Console.WriteLine "And the number of matches to remove..."
            let count = int (Console.ReadLine ())
            ev.Post (Action (index - 1, count))
        | AiView               ->
            printGameState state
            let (index, count) =
                match List.head (ev.History ()) with
                    | Action (index, count) -> (index + 1, count)
                    | _                     -> failwith "History is corrupt!"
            Console.WriteLine (string (index, count))
        | FinishedView         ->
            Console.WriteLine ("Game Over! The winner is " + string state.Player)
            Console.WriteLine "Want to play again? y/n"
            let response = Console.ReadLine ()
            if response = "y"
                then Console.WriteLine ""; ev.Post Reset
                else Environment.Exit 0

let initialState = { Heaps = List.empty; Error = None; Player = Player1; AiMode = false; View = InitialView }
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

// State atomata
let start (ui : State -> unit) =
    ui initialState
    let rec run state =
        async {
            let! msg = ev.Receive ()
            let newState = middleware reducer state msg
            ui newState
            return! run newState
        }
    run initialState

Async.StartImmediate (start nimGameGUI)
Application.Run(window)

// Async.StartImmediate (start nimGameUISetup)