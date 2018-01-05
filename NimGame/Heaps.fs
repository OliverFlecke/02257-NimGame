module Heaps

type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

type Error = string

type HeapIndex = int
type HeapCount = int
type Heaps = HeapCount list

let rnd = System.Random ()

/// Creates a default heap of size n with a random amount of matches in each (between 1 and 20)
let createHeap n : Heaps = List.init n (fun _ -> rnd.Next(1, 20))

/// Converts a heap into a string representation
let heapsToString (heaps : Heaps) = String.concat "   " (List.map string heaps)

/// Remove 'count' number of matches from number 'index' heap in 'heaps'
let removeMatches index count heaps : Either<Error, Heaps> =
    if index < 0 || index > (List.length heaps) - 1
        then Left "Index out of bounds"
        else if count < 1 || heaps.[index] = 0
            then Left "Invalid count"
            else
                let newHeaps = List.toArray heaps
                Array.set newHeaps index (max 0 (heaps.[index] - (max 0 count)))
                Right (Array.toList newHeaps)

/// Test if the game is over
let gameOver = List.forall ((=) 0)