module NimGameGUI

open System.Windows.Forms
open System.Drawing

open Heaps
open StateStore
open AsyncEventQueue
open System.Drawing
open System.Drawing

let window = new Form(Text="Nim Game", Size=Size(300,500), BackColor=Color.Blue)

let pendingPlayerGUI state =
    window.Controls.Clear ()
    let playerLabel = new Label(Location=Point(20, 20), Text=((string state.Player) + "'s turn"))
    window.Controls.Add playerLabel

    // Heaps view
    let heapLabel = new Label(Location=Point(20, 50), Text=heapsToString state.Heaps, MinimumSize=Size(280, 20))
    window.Controls.Add heapLabel

    // Input boxes
    let indexLabel = new Label(Location=Point(20, 80), Text="Index")
    let indexBox   = new TextBox(Location=Point(20, 110), Size=Size(280, 20))
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
    submitButton.Click.Add (fun _ -> events.Post (Action ((int indexBox.Text) - 1, (int countBox.Text))))

let startGUI () =
    window.Controls.Clear ()
    let numberOfHeapsLabel = new Label(Location=Point(20, 20), Text="Enter the number of heaps to play:",
                                        MinimumSize=Size(300, 20))
    window.Controls.Add numberOfHeapsLabel
    let numberOfHeapsBox = new TextBox(Location=Point(20, 40), MinimumSize=Size(240, 20), Text="5")
    window.Controls.Add numberOfHeapsBox

    let aiDifficultyLabel = new Label(Location=Point(20, 80), MinimumSize=Size(240, 20), Text="Ai Difficulty")
    window.Controls.Add aiDifficultyLabel
    let aiDifficultyBox = new TrackBar(Location=Point(20, 100), MinimumSize=Size(240, 20))
    aiDifficultyBox.SetRange (1, 10)
    window.Controls.Add aiDifficultyBox

    let startButton = new Button(Location=Point(20, 200), MinimumSize=Size(240,50), MaximumSize=Size(100,50), Text="Start")
    window.Controls.Add startButton
    startButton.Click.Add (fun _ -> events.Post (Start (int numberOfHeapsBox.Text)))

let finishedGUI state =
    window.Controls.Clear ()
    let winnerLabel = new Label(Location=Point(20, 20), Text=((string state.Player) + " is the winner!"), MinimumSize=Size(300,20))
    window.Controls.Add winnerLabel

    let resetButton = new Button(Location=Point(20, 50), Text="Reset")
    window.Controls.Add resetButton
    resetButton.Click.Add (fun _ -> events.Post Reset)

let nimGameGUI state =
    match state.View with
        | InitialView                   -> startGUI ()
        | PendingPlayerView | AiView    -> pendingPlayerGUI state
        | FinishedView                  -> finishedGUI state
