open ParserUtils

[<EntryPoint>]
let main argv =
  if argv.Length = 0
    then
      System.Console.WriteLine("specify path for test file")
      -1
    else
      let s = argv.[0]
      Draw.drawProgram s false
      System.Console.WriteLine("Program parsed and drawn")
      0
