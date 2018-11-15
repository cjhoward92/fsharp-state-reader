module State

type State<'a, 'state> =
    State of ('state -> 'a * 'state)

let retn x = State (fun s -> (x, s))

let runState monad state =
    let (State fn) = monad
    fn state

let bind f m =
    let inner state =
        let (result, newState) = runState m state
        runState (f result) newState
    State inner

let map f m =
    let inner state =
        let (result, _) = runState m state
        f result
    State inner

let apply (sFn: State<('a -> 'b), 'state>) (m: State<'a, 'state>) =
    let inner state =
        let (fn, newState) = runState sFn state
        let (result, newerState) = runState m newState
        (fn result, newerState)
    State inner

let zero () = retn ()

let delay f =
    f

let run f =
    f()

let traverse f list =
    let cons head tail = head :: tail
    let (<*>) = apply
    let initState = retn []
    let folder h t = retn cons <*> (f h) <*> t
    List.foldBack folder list initState

type StateBuilder() =
    member x.Bind(m,f) = bind f m
    member x.Return(a) = retn a
    member x.ReturnFrom(m) = bind (retn) m
    member x.Zero() = zero()
    //member x.Delay(f) = delay f
    //member x.Run(f) = run f
    member x.For(m,f) = bind f m

let state = StateBuilder()