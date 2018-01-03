open System
open System.Net
open System.Threading
open System.Windows.Forms
open System.Drawing
open System.Collections
open System

type Heaps = int list
type HeapIndex = int
type HeapCount = int
type Player = Player1 | Player2

type Message =
    | Start of int
    | Error
    | Reset
    | Action of HeapIndex * HeapCount

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b
type Error = string
type State =
    | StartState
    | PendingPlayerState
    | AiState
    | FinishedState
type Data =
    {
        Heaps: Heaps
        Error: Error option
        Player: Player
        AiMode: bool
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
        else if count < 1
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

let pendingPlayerGUI data =
    window.Controls.Clear ()
    let playerLabel = new Label(Location=Point(20, 20), Text=((string data.Player) + "'s turn"))
    window.Controls.Add playerLabel

    // Heaps view
    let heapLabel = new Label(Location=Point(20, 50), Text=heapsToString data.Heaps, MinimumSize=Size(500, 20))
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
    match data.Error with
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

let finishedGUI data =
    window.Controls.Clear ()
    let winnerLabel = new Label(Location=Point(20, 20), Text=((string data.Player) + " is the winner!"), MinimumSize=Size(300,20))
    window.Controls.Add winnerLabel

    let resetButton = new Button(Location=Point(20, 50), Text="Reset")
    window.Controls.Add resetButton
    resetButton.Click.Add (fun _ -> ev.Post Reset)

let nimGameGUI state data =
    match state with
        | StartState            -> startGUI ()
        | PendingPlayerState    -> pendingPlayerGUI data
        | AiState               -> ()
        | FinishedState         -> finishedGUI data

// Command line interface for the game
let printGameState data =
    match data.Error with
        | Some errorMsg -> Console.WriteLine errorMsg
        | None                  -> ()
    Console.WriteLine ""
    Console.WriteLine (string data.Player + "'s turn.")
    Console.WriteLine ("Heaps: " + heapsToString data.Heaps)

let nimGameCommandLineInterface state data =
    match state with
        | StartState            ->
            Console.WriteLine "Welcome to Nim!"
            Console.WriteLine "Enter the number of heaps you want to play: "
            let numberOfHeaps = int (Console.ReadLine ())
            ev.Post (Start numberOfHeaps)
        | PendingPlayerState   ->
            printGameState data
            Console.WriteLine "Choose a heap index... "
            let index = int (Console.ReadLine ())
            Console.WriteLine "And the number of matches to remove..."
            let count = int (Console.ReadLine ())
            ev.Post (Action (index - 1, count))
        | AiState              ->
            printGameState data
            let (index, count) =
                match List.head (ev.History ()) with
                    | Action (index, count) -> (index + 1, count)
                    | _                     -> failwith "History is corrupt!"
            Console.WriteLine (string (index, count))
        | FinishedState         ->
            Console.WriteLine ("Game Over! The winner is " + string data.Player)
            Console.WriteLine "Want to play again? y/n"
            let response = Console.ReadLine ()
            if response = "y"
                then ev.Post Reset
                else Environment.Exit 0

// State atomata
let rec start (ui : State -> Data -> unit) =
    async {
        ui StartState { Heaps = List.empty; Error = None; Player = Player1; AiMode = true }
        let! msg = ev.Receive ()
        match msg with
            | Start numberOfHeaps   ->
                return! pendingPlayer ui { Heaps = (createHeap numberOfHeaps); Error = None; Player = Player1; AiMode = false }
            | _                     -> failwith "Start error: unknown"
    }
and pendingPlayer ui data =
    async {
        match data.Player with
            | Player1 -> ui PendingPlayerState data
            | Player2 ->
                if data.AiMode
                    then aiAction data.Heaps; ui AiState data
                    else ui PendingPlayerState data
        let! msg = ev.Receive ()
        match msg with
            | Action (index, count) ->
                match removeMatches index count data.Heaps with
                    | Right newHeaps            ->
                        if gameOver newHeaps
                            then return! finished ui {data with Heaps = newHeaps; Error = None }
                            else return! pendingPlayer ui
                                    { data with Heaps = newHeaps; Error = None; Player = swapPlayer data.Player }
                    | Left errorMessage         -> return! pendingPlayer ui { data with Error = (Some errorMessage) }
            | _                     -> failwith "PendingPlayerAI error: unknown"
    }
and finished ui data =
    async {
        ui FinishedState data
        let! msg = ev.Receive ()
        match msg with
            | Reset -> return! start ui
            | _     -> failwith "Finished error: unknown"
    }

// Async.StartImmediate (start nimGameGUI)
// Application.Run(window)
Async.StartImmediate (start nimGameCommandLineInterface)