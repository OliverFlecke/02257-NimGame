#load "Heaps.fs"
#load "AsyncEventQueue.fs"
#load "StateStore.fs"
#load "NimGameGUI.fs"
#load "NimGameCmdUI.fs"

open StateStore
open NimGameCmdUI
open NimGameGUI
open System.Windows.Forms

// [<EntryPoint>]
let main args = 
    // Async.StartImmediate (start nimGameCommandLineUI)
    0

Async.StartImmediate (start nimGameGUI)
Application.Run(window)