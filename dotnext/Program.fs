type Action =
    | Atom of (unit -> unit)
    | Fork of Action list

type GoroutineBuilder() =
    member _.Yield(x) = [Atom x]

    member _.YieldFrom(xs: Action list) = [Fork xs]
    
    member _.Delay(f) = f

    member _.Combine(as1: Action list, f) = List.concat (seq { yield as1; yield f() })
    
    member _.Run(f) = f()

let goroutine = GoroutineBuilder()

let print v =
    (fun () -> printfn "%A" v)

let actions = goroutine {
    yield print "0.1"
    yield print "0.2"
    
    yield! goroutine {
        yield print "1.1"
        yield print "1.2"
        yield print "1.3"
    }
    
    yield print "0.3"

    yield! goroutine {
        yield print "2.1"
        yield print "2.2"
        yield print "2.3"
        yield print "2.4"
        yield print "2.5"
    }

    yield print "0.4"
    yield print "0.5"
}

let rec run_first_actions (processes: Action list list) =
    match processes with
    | p::ps ->
        match p with
        | Atom action::actions ->
            action()
            actions::run_first_actions ps
        | Fork actions2::actions1 ->
            actions1::actions2::run_first_actions ps
        | [] -> run_first_actions ps
    | [] -> []

let rec run_step (processes: Action list list) =
    let next_processes = run_first_actions processes
    if not (List.isEmpty next_processes)
    then run_step next_processes


let run actions = run_step [actions]

run actions