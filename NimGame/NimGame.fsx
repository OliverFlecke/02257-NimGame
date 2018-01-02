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
    | Start
    | Error
    | Reset
    | Action of HeapIndex * HeapCount 

type Either<'a, 'b> = 
    | Succes of 'a
    | Error of 'b

type AsyncEventQueue<'T>() = 
    let mutable cont = None 
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

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)

let rnd = System.Random ()

let ev : AsyncEventQueue<Message> = AsyncEventQueue()

let window = new Form(Text="Nim Game", Size=Size(500,500))

let createHeap n : Heaps = List.init n (fun _ -> rnd.Next(1, 20))

let removeMatches index count heaps : Either<Heaps, String> = 
    if count < 1 
        then Error "Invalid count"
        else
            let newHeaps = List.toArray heaps
            Array.set newHeaps index (max 0 (heaps.[index] - (max 0 count)))
            Succes (Array.toList newHeaps)
    
let gameOver = List.forall ((=) 0)

let swapPlayer = function 
    | Player1 -> Player2
    | Player2 -> Player1

let heapsToString heaps = String.concat "   " (List.map string heaps) 

let rec pendingPlayer heaps player error = 
    async {
        window.Controls.Clear ()
        let playerLabel = new Label(Location=Point(20, 20), Text=((string player) + "'s turn"))
        window.Controls.Add playerLabel

        // Heaps view
        let heapLabel = new Label(Location=Point(20, 50), Text=heapsToString heaps, MinimumSize=Size(500, 20))
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
        match error with 
            | Some errorMessage -> 
                let errorLabel = new Label(Location=Point(20, 200), Text=errorMessage)
                window.Controls.Add errorLabel
            | None              -> ()

        let submitButton = new Button(Location=Point(20, 250), Text="Submit")
        window.Controls.Add submitButton
        submitButton.Click.Add (fun _ -> ev.Post (Action ((int indexBox.Text), (int countBox.Text))))

        let! msg = ev.Receive ()
        match msg with 
            | Action (index, count) ->
                match removeMatches index count heaps with 
                    | Succes newHeaps   -> 
                        if gameOver newHeaps 
                            then return! finished player
                            else return! pendingPlayer newHeaps (swapPlayer player) None
                    | Error errorMsg    -> return! pendingPlayer heaps player error                 
            | _                     -> failwith "PendingPlayer Error:"
    }
and start () = 
    async {
        window.Controls.Clear ()
        let startButton = new Button(Location=Point(20, 50), MinimumSize=Size(100,50), MaximumSize=Size(100,50), Text="Start")
        window.Controls.Add startButton
        startButton.Click.Add (fun _ -> ev.Post Start)

        let numberOfHeapsBox = new TextBox(Location=Point(20, 20), MinimumSize=Size(50, 20), Text="5")
        window.Controls.Add numberOfHeapsBox

        let! msg = ev.Receive ()
        match msg with
            | Start     -> return! pendingPlayer (createHeap (int numberOfHeapsBox.Text)) Player1 None
            | _         -> failwith "Start: unexpected message"
    }
and finished player = 
    async {
        window.Controls.Clear () 

        let winnerLabel = new Label(Location=Point(20, 20), Text=((string player) + " is the winner!"))
        window.Controls.Add winnerLabel

        let resetButton = new Button(Location=Point(20, 50), Text="Reset")
        window.Controls.Add resetButton
        resetButton.Click.Add (fun _ -> ev.Post Reset)

        let! msg = ev.Receive ()
        match msg with 
            | Reset -> return! start ()
            | _     -> failwith "Finished error"
    }

// [<EntryPoint>] 
// let main argv =
Async.StartImmediate (start ())
Application.Run(window)
    // 0 // return an integer exit code
