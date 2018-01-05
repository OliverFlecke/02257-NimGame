open StateStore
open NimGameCmdUI
open NimGameGUI
open System.Windows.Forms

[<EntryPoint>]
let main args =
    if args.Length > 0 && args.[0] = "-cli" 
        then Async.StartImmediate (start nimGameCommandLineUI)
        else Async.StartImmediate (start nimGameGUI)
             Application.Run(window)
    0
