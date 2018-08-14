// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

open System.Text
open System.Text.RegularExpressions

#I @"..\packages\FParsec.1.0.4-RC3\lib\portable-net45+win8+wp8+wpa81"
#r "FParsec"
#r "FParsecCS"
open FParsec


#load @"LineParser\Parser.fs"
open LineParser.Parser

let inputFile = @"G:\work\Projects\rtu\AR-asset-expired-2011\structured-text.txt"

/// TODO runLineParser needs a better (safer) ``split to lines`` than this...
let temp01 () = 
    let s1 = "one\ntwo\nthree"
    let lines = s1.Split('\n')
    lines.[0]

let temp02 () = 
    let src = @"number=123"
    let pat = @"(?<label>\w+)=(?<val>\d+)"
    printfn "%s:=%s" (Regex.Match(src,pat).Groups.Item("label").Value) (Regex.Match(src,pat).Groups.Item("val").Value)

let temp02b () = 
    let src = @"number=123"
    let pat = @"(?<label>\w+)=(?<val>\d+)"
    printfn "'%s'" (Regex.Match(src,pat).Groups.Item("notfound").Value) 










let test01 () = 
    let parser = tupleM2 textline textline
    runLineParserFile inputFile parser

let test02 () = 
    let parser = 
        let bodyStart = rmatch1 "^Body:.*"
        skipManyTill textline (lookahead bodyStart) >>>. textline
    runLineParserFile inputFile parser

/// Should fail...
let test02a () = 
    let parser = 
        let bodyStart = rmatch1 "^Body:.*"
        bodyStart
    runLineParserFile inputFile parser

// Look for "form-feed" special char

let formFeed : LineParser<unit> = 
    rmatch1 @"^\f" |>>> fun _ -> ()

//let bodyTag : LineParser<string> = 
//    rmatch1 @"^\f" |>>> fun _ -> ()


let test03 () = 
    let parser = 
        skipManyTill textline (lookahead formFeed) >>>. formFeed
    runLineParserFile inputFile parser

