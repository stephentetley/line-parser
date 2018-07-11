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
    let action = tupleM2 textline textline
    runLineParserFile inputFile action

