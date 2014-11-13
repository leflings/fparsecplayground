let strToInt str =
    match System.Int32.TryParse(str) with
    | true, n -> Some n
    | false, _ -> None

type YourWorkflow() =
    member x.Bind(m,f) = Option.bind f m
    member x.Return(f) = Some f

let yourWorkflow = YourWorkflow()

let stringAddWorkflow x y z = 
    yourWorkflow 
        {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }    

// test
let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

let strAdd str i =
    yourWorkflow {
        let! n = strToInt str
        return i + n
    }
let (>>=) m f = Option.bind f m
//And then with these functions, you should be able to write code like this:

let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let bad = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"