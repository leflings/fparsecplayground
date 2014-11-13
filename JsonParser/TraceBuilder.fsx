type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) = 
        printfn "Returning an option (%A) directly" m
        m
    member this.Zero() = 
        printfn "Zero"
        None
    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x
    member this.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m

// make an instance of the workflow 
let trace = new TraceBuilder()

trace { 
    return 1
    } |> printfn "Result 1: %A" 

trace { 
    return! Some 2
    } |> printfn "Result 2: %A" 

trace { 
    let! x = Some 1
    let! y = Some 2
    return x + y
    } |> printfn "Result 3: %A" 

trace { 
    let! x = None
    let! y = Some 1
    return x + y
    } |> printfn "Result 4: %A" 
trace { 
    do! Some (printfn "...expression that returns unit")
    do! Some (printfn "...another expression that returns unit")
    let! x = Some (1)
    return x
    } |> printfn "Result from do: %A"
trace { 
    printfn "hello world"
    } |> printfn "Result for simple expression: %A" 

trace { 
    if false then return 1
    } |> printfn "Result for if without else: %A" 