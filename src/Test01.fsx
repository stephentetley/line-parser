// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


#load @"LineParser\Parser.fs"
open LineParser.Parser

let inputFile = @"G:\work\Projects\rtu\AR-asset-expired-2011\structured-text.txt"


let temp01 () = 
    let s1 = "one\ntwo\nthree"
    let lines = s1.Split('\n')
    lines.[0]

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
