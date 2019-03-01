// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace LineParser

module FParsecLines =
    
    open System.Collections.Generic
    open System.IO
    open FParsec

   

    //let inline readLine1 (handle: (lineReader:Parser<'ans,unit>) = 
    //    let source = 
    //    match runParserOnString pSignature () "NONE" source with
    //    | Success(ans,_,_) -> ans
    //    | Failure(_,_,_) -> failwithf "Parsing failed on signature: '%s'" source


    let parseLines (parseLine:Parser<'a,'UserState>)
                    (initialState:'UserState)
                    (path:string) : IEnumerable<'a> = 
        let rator = File.ReadLines(path).GetEnumerator()
        let rec work (st:'UserState) :seq<'a> = 
            match rator.MoveNext() with
            | false -> Seq.empty
            | true -> 
                let line1 :string = rator.Current
                match runParserOnString parseLine st "NONE" line1 with
                | Success(ans,st1,_) -> 
                    seq { yield ans; yield! work st1 }
                | Failure(_,_,st1) -> 
                    seq { yield! work st1 }                    
        work initialState
