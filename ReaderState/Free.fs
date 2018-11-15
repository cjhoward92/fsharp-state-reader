module Free

open Reader
open State

type ReaderState<'env, 'state, 'a> =
    ReaderState of Reader<'env, State<'a, 'state>>

let flip fn x = fun y -> fn y x

let extract (m: ReaderState<'env, 'state, 'a>) (e: 'env) (s: 'state): 'a * 'state =
    let (ReaderState r) = m
    runReader r e |> flip runState s

let runReaderT m e =
    let (ReaderState r) = m
    runReader r e

let map (f: State<'a, 'state> -> State<'b, 'state>) (m: ReaderState<'env, 'state, 'a>) =
    let (ReaderState r) = m
    Reader.map f r |> ReaderState

let bind (f: 'a -> ReaderState<'env, 'state, 'b>) (m: ReaderState<'env, 'state, 'a>) =
    let inner (env: 'env) =
        let result = runReaderT m env
        State (fun s ->
            let (a, newState) = runState result s
            extract (f a) env newState
        )
    Reader inner |> ReaderState

let apply (rF: ReaderState<'env, 'state, ('a -> 'b)>) (m: ReaderState<'env, 'state, 'a>) =
    let (ReaderState r) = m
    let (ReaderState fn) = rF

    State.apply
    |> flip Reader.map fn
    |> flip Reader.apply r
    |> ReaderState