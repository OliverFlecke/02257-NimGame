module NimGameGUI

open System.Windows.Forms
open System.Drawing

open Heaps
open StateStore
open AsyncEventQueue
open System.Drawing

let window = new Form(Text="Nim Game",
                        Size=Size(300, 400),
                        BackColor=Color.LightGray)

let startGUI state =
    window.Controls.Clear ()

    // Input for the number of heaps
    let numberOfHeapsLabel = new Label(Location=Point(20, 20),
                                       Text="Enter the number of heaps to play:",
                                       MinimumSize=Size(300, 20))
    window.Controls.Add numberOfHeapsLabel
    let numberOfHeapsInput = new NumericUpDown(Location=Point(20, 40),
                                               MinimumSize=Size(240, 20),
                                               Minimum=1m,
                                               Maximum=10m,
                                               Value=5m)
    window.Controls.Add numberOfHeapsInput

    // Enable/disable Ai
    let aiEnabledCheckBox = new CheckBox(Location=Point(20, 80),
                                         MinimumSize=Size(100, 20),
                                         Text="AI Enabled",
                                         Checked=state.Settings.AIEnabled)
    window.Controls.Add aiEnabledCheckBox

    // Control the difficulty of the AI
    let aiDifficultyLabel = new Label(Location=Point(20, 110),
                                      MinimumSize=Size(240, 20),
                                      Text="Ai Difficulty")
    window.Controls.Add aiDifficultyLabel
    let aiDifficultyTrackBar = new TrackBar(Location=Point(20, 130),
                                            MinimumSize=Size(240, 20),
                                            Value=(int (state.Settings.AIDifficulty * 10.0)),
                                            Enabled=state.Settings.AIEnabled)
    aiDifficultyTrackBar.SetRange (0, 10)
    window.Controls.Add aiDifficultyTrackBar

    // Event handlers
    aiEnabledCheckBox.Click.Add (fun _ ->
        events.Post (ChangeSettings { AIEnabled = aiEnabledCheckBox.Checked;
                                      AIDifficulty = (float aiDifficultyTrackBar.Value) / 10.0 }))
    aiDifficultyTrackBar.Click.Add (fun _ ->
        events.Post (ChangeSettings { AIEnabled = aiEnabledCheckBox.Checked;
                                      AIDifficulty = (float aiDifficultyTrackBar.Value) / 10.0 }) )
    // Start game button
    let startButton = new Button(Location=Point(20, 200),
                                    Size=Size(240,30),
                                    Text="Start")
    window.Controls.Add startButton
    startButton.Click.Add (fun _ -> events.Post (Start (int numberOfHeapsInput.Value)))

let pendingPlayerGUI state =
    window.Controls.Clear ()
    let playerLabel = new Label(Location=Point(20, 20),
                                Size=Size(260, 30),
                                Font=new Font(FontFamily.GenericSansSerif, 16.0f),
                                Text=((string state.Player) + "'s turn"))
    window.Controls.Add playerLabel

    // Heaps view
    let heapLabel = new Label(Location=Point(20, 50),
                              Text=("Heaps: " + heapsToString state.Heaps),
                              MinimumSize=Size(280, 20))
    window.Controls.Add heapLabel

    // Input boxes
    let indexLabel = new Label(Location=Point(20, 80),
                               Text="Index",
                               Size=Size(40, 20))
    let indexInput   = new NumericUpDown(Location=Point(60, 80),
                                         Size=Size(70, 20),
                                         Minimum=1m,
                                         Maximum=(decimal state.Heaps.Length))
    window.Controls.Add indexLabel
    window.Controls.Add indexInput

    // Count box
    let countLabel = new Label(Location=Point(20, 110),
                               Text="Count",
                               Size=Size(40, 20))
    let countInput   = new NumericUpDown(Location=Point(60, 110),
                                         Size=Size(70, 20),
                                         Minimum=1m)
    window.Controls.Add countLabel
    window.Controls.Add countInput

    // Submit button
    let submitButton = new Button(Location=Point(20, 150),
                                  Text="Submit",
                                  Size=Size(240, 30))
    submitButton.Click.Add (fun _ -> events.Post (Action ((int indexInput.Value) - 1, (int countInput.Value))))
    window.Controls.Add submitButton

    // Undo button
    let undoButton = new Button(Location=Point(20, 190),
                                Text="Undo",
                                Size=Size(240, 30),
                                Enabled=(state.PreviousState.IsSome))
    undoButton.Click.Add (fun _ -> events.Post Undo)
    window.Controls.Add undoButton

    // Reset button
    let resetButton = new Button(Location=Point(20, 230),
                                 Text="Reset",
                                 Size=Size(240, 30))
    resetButton.Click.Add (fun _ -> events.Post Reset)
    window.Controls.Add resetButton

    match state.LastChoice with
        | Some (index, count)  ->
            let messageLabel = new Label(Location=Point(20,270),
                                         Text=("Last action: " + string count + " from heap " + string (index + 1)),
                                         Size=Size(240, 20))
            window.Controls.Add messageLabel
        | None                          -> ()
    // Error handling
    match state.Error with
        | Some errorMessage ->
            let errorLabel = new Label(Location=Point(20, 300),
                                       Text=errorMessage,
                                       Size=Size(240, 20))
            window.Controls.Add errorLabel
        | None              -> ()

let finishedGUI state =
    window.Controls.Clear ()
    let winnerLabel = new Label(Location=Point(20, 20),
                                Text=((string state.Player) + " is the winner!"),
                                MinimumSize=Size(300,20))
    window.Controls.Add winnerLabel

    let resetButton = new Button(Location=Point(20, 50), Text="Reset")
    window.Controls.Add resetButton
    resetButton.Click.Add (fun _ -> events.Post Reset)

let nimGameGUI state =
    match state.View with
        | InitialView                   -> startGUI state
        | PendingPlayerView | AiView    -> pendingPlayerGUI state
        | FinishedView                  -> finishedGUI state