// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions

open Microsoft.FSharp.Collections

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec
open FParsec.CharParsers

#load "FParsecLines.fs"
open  LineParser.FParsecLines


let inputPath = @"G:\work\pdftotext\pdftotext-table.txt"

let test01 () = 
    File.ReadAllLines(inputPath)

let test02 () = 
    File.ReadLines(inputPath) |> Seq.map (fun s -> if s.Length > 10 then s.[0..10] else s)

let test03 () = 
    let en = File.ReadLines(inputPath).GetEnumerator()
    if en.MoveNext() then
        en.Current
    else
        failwith "No content"



/// This is a Seq generator that filters.
let readFilterLines (path:string) (test:string -> bool) : IEnumerable<string> = 
    let rator = File.ReadLines(path).GetEnumerator()
    let rec work () = 
        match rator.MoveNext() with
        | false -> Seq.empty
        | true -> 
            let ss = rator.Current
            if test ss then 
                seq { yield ss; yield! work () }
            else
                seq { yield! work () }
    work ()

let test04 () = 
    let test s = Regex.IsMatch(s,"DAZ")
    readFilterLines inputPath test

let test05 () = 
    test04 () |> Seq.length

let inline readLine1 parser = 
    let source = "Hello world!"
    match runParserOnString parser () "NONE" source with
    | Success(ans,_,_) -> ans
    | Failure(_,_,_) -> failwithf "Parsing failed on signature: '%s'" source

let test06 () = 
    parseLines (many anyChar) () inputPath

let formFeed : Parser<unit,unit> = pchar (char 12) |>> ignore


let dazLiteral : Parser<string,unit> = optional formFeed >>. pstring "DAZ"

let pint : Parser<int,unit>  = 
    puint32 |>> int

let dazNumber : Parser<int,unit> = spaces >>. pint

let receivingSTW : Parser<string,unit> = 
    charsTillString "SAI0" false 200 |>> fun s -> s.Trim()

let saiReference : Parser<string,unit> = 
    parse { 
        do! spaces 
        let! _ = pstring "SAI" 
        let! nums = many1Satisfy (isDigit)
        return "SAI" + nums
    }

type LinePayload = Map<string,string>

let atLeastSpaces (atLeast:int) : Parser<unit,unit> = 
    manyMinMaxSatisfy atLeast 200 (fun c -> c=' ') |>> ignore

let commonName : Parser<string,unit> = 
    spaces >>. manyCharsTill anyChar (atLeastSpaces 3) |>> (fun s -> s.Trim())

let payloadLine1 : Parser<LinePayload,unit> = 
    parse {
        let! daz = dazNumber
        let! stw = receivingSTW
        let! sai = saiReference
        let! name = commonName

        return Map.ofList [ ("DAZ", daz.ToString())
                          ; ("STW", stw)
                          ; ("SAI", sai)
                          ; ("NAME", name)
                          ]
    }

let test07 () = 
    parseLines dazLiteral () inputPath


let test07b () = 
    parseLines dazNumber () inputPath |> Seq.length


let test08 () = 
    parseLines payloadLine1 () inputPath

//type Payload = 
//    { Daz: int 
//      ReceivingStw: string
//      SaiNumber: string
//    }

let sreverse (s:string) = new string(s.ToCharArray() |> Array.rev)

//let reverseParse (parser:Parser<'a,'u>) : Parser<'a,'u> = 
//    parse { 
//        let! input = regex ".*" |>> sreverse
//        let! state = 
//        printfn "%s" input
//        match runParserOnString parser input with
//        | Success(ans, _, _) -> return ans
//        | Failure(errMsg, _, _) -> fail errMsg
//        }

//let dummy1 () : int= 
//    let source = "Wasted text going towards an integer 901"
//    match runParserOnString (reverseParse pint) () "NONE" source with
//    | Success(ans,_,_) -> ans
//    | Failure(_,_,_) -> failwithf "Parsing failed on signature: '%s'" source





