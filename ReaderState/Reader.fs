module Reader

type Reader<'env, 'a> =
    Reader of ('env -> 'a)

// Reader<'env, 'a> -> 'env -> 'a
let runReader reader env =
    let (Reader fn) = reader
    fn env

// ('a -> Reader<'env, 'b>) -> Reader<'env, 'a> -> Reader<'env, 'b>
let bind (f: 'a -> Reader<'env, 'b>) (m: Reader<'env, 'a>) =
    let inner env =
        let result = runReader m env
        runReader (f result) env
    Reader inner

// ('a -> 'b) -> Reader<'env, 'a> -> Reader<'env, 'b>
let map f m =
    let inner env =
        let result = runReader m env
        f result
    Reader inner

// 'a -> Reader<'env, 'a>
let retn x = Reader (fun _ -> x)

// Reader<'env, ('a -> 'b)> -> Reader<'env, 'a> -> Reader<'env, 'b>
let apply rFn m =
    let inner env =
        let fn = runReader rFn env
        let result = runReader m env
        fn result
    Reader inner

let zero () = retn ()

let delay (f: unit -> Reader<'env, unit>) =
    printfn "We are in the delay"
    f

let run comp =
    printfn "We are in the run"
    comp()

let traverse f list =
    let cons head tail = head :: tail
    let (<*>) = apply
    let initReader = retn []
    let folder h t = retn cons <*> (f h) <*> t
    List.foldBack folder list initReader

type ReaderBuilder() =
    member x.Bind(m,f) = bind f m
    member x.Return(a) = retn a
    member x.ReturnFrom(m) = bind (retn) m
    member x.Zero() = zero()
    //member x.Delay(f) = delay f
    //member x.Run(f) = run f
    member x.For(m,f) = bind f m

let reader = ReaderBuilder()