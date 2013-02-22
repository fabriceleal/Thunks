open System

type Thunk = 
    | Thunk of (obj list -> Thunk) * obj list
    | ThunkValue of obj

let rec evalThunk thk = 
    match thk with
    | ThunkValue(v) -> v
    | Thunk(func, args) ->         
        func args |> evalThunk;;

// Thunk-style sum
let rec sumThunk (args : obj list) : Thunk =
    let n, cont =   args.[0] :?> int,
                    args.[1] :?> obj list -> Thunk;
    if n = 1 then
        // Final thunk, 
        // thunk the return value within the continuation
        Thunk(cont, [1])
    else
        // new continuation. 
        // This thunks a call to the supplied continuation
        // updating the return value
        let new_cont (w_args : obj list) =
            match w_args with
            | h :: _ ->
                let hint = h :?> int
                Thunk(cont, [hint + n])
            | _ -> failwith "unexpected!";
        // Thunk a recursive call
        // updateing the arguments and with a new continuation
        Thunk(sumThunk, [n - 1; new_cont]);;

let thunkObj (m : obj list) =
    match m with
    | x :: _ -> ThunkValue(x)
    | _ -> failwith "unexpected!";;

[<EntryPoint>]
let main argv = 
    
    sumThunk [1; thunkObj] |> evalThunk |> Console.WriteLine
    sumThunk [2; thunkObj] |> evalThunk |> Console.WriteLine
    sumThunk [3; thunkObj] |> evalThunk |> Console.WriteLine

    Console.ReadKey() |> ignore
    0 // return an integer exit code
