module AsyncEventQueue
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