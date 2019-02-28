// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

open System.Text
open System.Text.RegularExpressions

#load @"..\src\LineParser\Parser.fs"
open LineParser.Parser

let inputFile = @"G:\work\pdftotext\pdftotext-layout.txt"


let test01 () = 
    let parser = textline
    runLineParserFile inputFile parser
