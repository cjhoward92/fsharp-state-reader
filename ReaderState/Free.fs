module Free

open Reader
open State

type ReaderState<'env, 'state, 'a> =
    ReaderState of Reader<'env, State<'a, 'state>>

let runReaderT (ReaderState r) e = runReader r e

let private mapR f r = reader {
    let! a = r
    return State.map f a
}

let map f (ReaderState r) = mapR f r |> ReaderState

let bindR f r = reader {
    let! a = r
    return State.bind f a
}

let bind f (ReaderState r) = bindR f r |> ReaderState

let private applyR rf r = reader {
    let! a = r
    let! fn = rf
    return State.apply fn a
}

let apply (ReaderState fn) (ReaderState r) = applyR fn r |> ReaderState

// Keeping this because I like it
// Alternate apply impl
// let flip fn x = fun y -> fn y x
// State.apply
// |> flip Reader.map fn
// |> flip Reader.apply r
// |> ReaderState

let retn x = State.retn >> Reader.retn >> ReaderState <| x

let liftR r = r |> ReaderState
let liftS s = s |> Reader.retn |> ReaderState

type ReaderStateBuilder() =
    member this.Bind(m,f) = bind f m
    member this.Return(x) = retn x
    member this.ReturnFrom(m) = bind (State.retn) m
    member this.Zero() = retn ()

let readerState = ReaderStateBuilder()
