module Monads

[<CompiledName("IsNull")>]
let inline isNull<'T when 'T : not struct> (arg : 'T) =
    // OPTIMIZE :   Implement with inline IL (ldnull, ldarg.0, ceq). We can't use LanguagePrimitives.PhysicalEquality because it
    //              requires the 'null' constraint which we don't want to require for this function.
    System.Object.ReferenceEquals (null, arg)

[<Sealed>]
type MaybeBuilder () =
    // 'T -> M<'T>
    member inline __.Return value : 'T option =
        Some value

    // M<'T> -> M<'T>
    member inline __.ReturnFrom value : 'T option =
        value

    // unit -> M<'T>
    member inline __.Zero () : unit option =
        Some ()     // TODO : Should this be None?

    // (unit -> M<'T>) -> M<'T>
    member __.Delay (f : unit -> 'T option) : 'T option =
        f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    member inline __.Combine (r1, r2 : 'T option) : 'T option =
        match r1 with
        | None ->
            None
        | Some () ->
            r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    member inline __.Bind (value, f : 'T -> 'U option) : 'U option =
        Option.bind f value

//    // M<'T> * (exn -> M<'T>) -> M<'T>
//    member this.TryWith (body, handler) : _ option =
//        fun value ->
//            try body value
//            with ex ->
//                handler ex
//
//    // M<'T> * (unit -> unit) -> M<'T>
//    member this.TryFinally (body, handler) : _ option =
//        fun value ->
//            try body value
//            finally
//                handler ()

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    member this.Using (resource : ('T :> System.IDisposable), body : _ -> _ option) : _ option =
        try body resource
        finally
            if not <| isNull (box resource) then
                resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    member this.While (guard, body : _ option) : _ option =
        if guard () then
            // OPTIMIZE : This could be simplified so we don't need to make calls to Bind and While.
            this.Bind (body, (fun () -> this.While (guard, body)))
        else
            this.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    member this.For (sequence : seq<_>, body : 'T -> unit option) : _ option =
        // OPTIMIZE : This could be simplified so we don't need to make calls to Using, While, Delay.
        this.Using (sequence.GetEnumerator (), fun enum ->
            this.While (
                enum.MoveNext,
                this.Delay (fun () ->
                    body enum.Current)))
