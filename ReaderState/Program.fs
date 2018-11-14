// Learn more about F# at http://fsharp.org

open System
open Reader
open State

let first num =
    Reader (fun x -> x * num)

let second (num: int) =
    let makeStr a = a.ToString()
    Reader (fun x -> x + num |> makeStr)

let third (s: string) =
    Reader (fun x -> printfn "String is: %s. Num is: %i" s x)

let s1 num =
    State (fun s -> (num, s + num))

let s2 num =
    State (fun s-> (num.ToString(), s + num))

let s3 str =
    State (fun s -> (printfn "State str is %s and state is %i" str s, s))

[<EntryPoint>]
let main argv =
    let r = reader {
        let! a = first 10
        let! b = second a
        do! third b
    }

    runReader r 10

    let s = state {
        let! a = s1 100
        let! b = s2 200
        do! s3 b
    }

    let (_, finalState) = runState s 40
    printfn "Final state is %i" finalState

    Console.ReadKey() |> ignore
    0 // return an integer exit code
