// Learn more about F# at http://fsharp.org

open System
open GameModule

[<EntryPoint>]
let main argv =
    playGame Console.ReadLine Console.Write
    0 // return an integer exit code
