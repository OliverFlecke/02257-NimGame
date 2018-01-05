module NimGameCmdUI

open System
open Heaps
open StateStore

// Command line interface for the game
let printGameState state =
    match state.Error with
        | Some errorMsg -> System.Console.WriteLine errorMsg
        | None                  -> ()
    System.Console.WriteLine ""
    System.Console.WriteLine (string state.Player + "'s turn.")
    System.Console.WriteLine ("Heaps: " + heapsToString state.Heaps)

let nimGameCommandLineUI state dispatch =
    match state.View with
        | InitialView            ->
            System.Console.WriteLine "Welcome to Nim!"
            System.Console.WriteLine "Enter the number of heaps you want to play: "
            let numberOfHeaps = int (System.Console.ReadLine ())
            dispatch (Start numberOfHeaps)
        | PendingPlayerView    ->
            printGameState state
            System.Console.WriteLine "Choose a heap index... "
            let index = int (System.Console.ReadLine ())
            System.Console.WriteLine "And the number of matches to remove..."
            let count = int (System.Console.ReadLine ())
            dispatch (Action (index - 1, count))
        | AiView               ->
            printGameState state
            let (index, count) =
                match state.LastChoice with
                    | Some (index, count)   -> (index + 1, count)
                    | None                  -> failwith "History is corrupt!"
            System.Console.WriteLine (string (index, count))
        | FinishedView         ->
            System.Console.WriteLine ("Game Over! The winner is " + string state.Player)
            System.Console.WriteLine "Want to play again? y/n"
            let response = System.Console.ReadLine ()
            if response = "y"
                then System.Console.WriteLine ""; dispatch Reset
                else Environment.Exit 0