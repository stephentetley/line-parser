// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause


module LineParser.Parser

open System.Text
open System.Text.RegularExpressions

/// We expect to delegate to FParsec or Regex for character level parsers
/// numbers, strings, dates, times, etc.
/// Naming should avoid needless clashes with FParsec

type LineNumber = int
type Index = int


type ErrorItem = { Line: LineNumber; ErrMessage: string }

type ParseError = ParseError of ErrorItem * list<ParseError>

let getErrorLog (err:ParseError) : string = 
    let item (err:ErrorItem) : string = sprintf "%i: %s" err.Line err.ErrMessage
    let writeLine (depth:int) (str:string) (sb:StringBuilder) : StringBuilder = 
        let line = sprintf "%s %s" (String.replicate depth "*") str
        sb.AppendLine(line)
    let rec work (e1:ParseError) (depth:int) (sb:StringBuilder) : StringBuilder  = 
        match e1 with
        | ParseError (s,[]) -> writeLine depth (item s) sb
        | ParseError (s,xs) ->
            let sb1 = writeLine depth (item s) sb
            List.fold (fun buf branch -> work branch (depth+1) buf) sb1 xs
    work err 0 (new StringBuilder()) |> fun sb -> sb.ToString()


/// Create a fresh ParseError
let private parseError (lineNum:LineNumber) (errMsg:string) : ParseError = 
    ParseError({ Line=lineNum+1; ErrMessage=errMsg }, [])

let concatParseErrors (lineNum:LineNumber) (errMsg:string) 
                        (failures:ParseError list) : ParseError = 
    ParseError({ Line=lineNum+1; ErrMessage=errMsg },failures)


type Result<'a> = 
    | Err of ParseError
    | Ok of LineNumber * 'a

type LineParser<'a> = 
    LineParser of (string [] -> Index -> Result<'a>)

let inline private apply1 (ma: LineParser<'a>) 
                            (lines: string [])
                            (pos: LineNumber) : Result<'a>= 
    let (LineParser f) = ma in f lines pos

let inline lpreturn (x:'a) : LineParser<'a> = 
    LineParser <| fun _ pos -> Ok (pos,x)


let inline private bindM (ma: LineParser<'a>) 
                            (f: 'a -> LineParser<'b>) : LineParser<'b> =
    LineParser <| fun lines pos -> 
        match apply1 ma lines pos with
        | Err stk -> Err stk
        | Ok (pos1,a) -> apply1 (f a) lines pos1


let inline lpzero () : LineParser<'a> = 
    LineParser <| fun _ pos -> Err (parseError pos "pzero")


let inline private combineM (ma:LineParser<unit>) 
                                (mb:LineParser<unit>) : LineParser<unit> = 
    LineParser <| fun lines pos -> 
        match apply1 ma lines pos with
        | Err stk -> Err stk
        | Ok (pos1,a) -> apply1 mb lines pos1


let inline private  delayM (fn:unit -> LineParser<'a>) : LineParser<'a> = 
    bindM (lpreturn ()) fn 



type LineParserBuilder () = 
    member self.Return x            = lpreturn x
    member self.Bind (p,f)          = bindM p f
    member self.Zero ()             = lpzero ()
    member self.Combine (ma,mb)     = combineM ma mb
    member self.Delay fn            = delayM fn


// Prefer "parse" to "parser" for the _Builder instance

let (parseLines:LineParserBuilder) = new LineParserBuilder ()


let (>>>=) (ma:LineParser<'a>) (fn:'a -> LineParser<'b>) : LineParser<'b> = 
    bindM ma fn


// *************************************
// Errors

let throwError (msg:string) : LineParser<'a> = 
    LineParser <| fun _ pos -> Err (parseError pos msg)

let swapError (msg:string) (ma:LineParser<'a>) : LineParser<'a> = 
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err (ParseError (_,stk)) -> Err (concatParseErrors pos msg stk)
        | Ok (pos1,a) -> Ok (pos1,a)

let (<&?>) (ma:LineParser<'a>) (msg:string) : LineParser<'a> = 
    swapError msg ma

let (<?&>) (msg:string) (ma:LineParser<'a>) : LineParser<'a> = 
    swapError msg ma

// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:LineParser<'a>) : LineParser<'b> = 
    LineParser <| fun tables ix -> 
       match apply1 ma tables ix with
       | Err msg -> Err msg
       | Ok (ix1,a) -> Ok (ix1, fn a)

// This is the nub of embedding FParsec - name clashes.
// We avoid them by using longer names in DocSoup.

/// Operator for fmap.
let (|>>>) (ma:LineParser<'a>) (fn:'a -> 'b) : LineParser<'b> = 
    fmapM fn ma

/// Flipped fmap.
let (<<<|) (fn:'a -> 'b) (ma:LineParser<'a>) : LineParser<'b> = 
    fmapM fn ma

// liftM (which is fmap)
let liftM (fn:'a -> 'x) (ma:LineParser<'a>) : LineParser<'x> = 
    fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'x) 
            (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'x> = 
    parseLines { 
        let! a = ma
        let! b = mb
        return (fn a b)
    }

let liftM3 (fn:'a -> 'b -> 'c -> 'x) 
            (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) : LineParser<'x> = 
    parseLines { 
        let! a = ma
        let! b = mb
        let! c = mc
        return (fn a b c)
    }

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) 
            (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) : LineParser<'x> = 
    parseLines { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        return (fn a b c d)
    }


let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x) 
            (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) : LineParser<'x> = 
    parseLines { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        let! e = me
        return (fn a b c d e)
    }

let liftM6 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x) 
            (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) (mf:LineParser<'f>) : LineParser<'x> = 
    parseLines { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        let! e = me
        let! f = mf
        return (fn a b c d e f)
    }

let tupleM2 (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) : LineParser<'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) : LineParser<'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) : LineParser<'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

let tupleM6 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) (mf:LineParser<'f>) : LineParser<'a * 'b * 'c * 'd * 'e * 'f> = 
    liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) ma mb mc md me mf

let pipeM2 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (fn:'a -> 'b -> 'x) : LineParser<'x> = 
    liftM2 fn ma mb

let pipeM3 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) 
            (fn:'a -> 'b -> 'c -> 'x): LineParser<'x> = 
    liftM3 fn ma mb mc

let pipeM4 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (fn:'a -> 'b -> 'c -> 'd -> 'x) : LineParser<'x> = 
    liftM4 fn ma mb mc md

let pipeM5 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) 
            (fn:'a -> 'b -> 'c -> 'd -> 'e ->'x): LineParser<'x> = 
    liftM5 fn ma mb mc md me

let pipeM6 (ma:LineParser<'a>) (mb:LineParser<'b>) 
            (mc:LineParser<'c>) (md:LineParser<'d>) 
            (me:LineParser<'e>) (mf:LineParser<'f>) 
            (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'x): LineParser<'x> = 
    liftM6 fn ma mb mc md me mf

/// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
let alt (ma:LineParser<'a>) (mb:LineParser<'a>) : LineParser<'a> = 
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err stk1 -> 
            match apply1 mb lines pos with
            | Err stk2 -> Err (concatParseErrors pos "alt" [stk1;stk2])
            | Ok (pos1,b) -> Ok (pos1,b)
        | Ok (pos1,a) -> Ok (pos1,a)

let (<||>) (ma:LineParser<'a>) (mb:LineParser<'a>) : LineParser<'a> = 
    alt ma mb <&?> "(<||>)"


// Haskell Applicative's (<*>)
let apM (mf:LineParser<'a ->'b>) (ma:LineParser<'a>) : LineParser<'b> = 
    parseLines { 
        let! fn = mf
        let! a = ma
        return (fn a) 
    }

let (<**>) (ma:LineParser<'a -> 'b>) (mb:LineParser<'a>) : LineParser<'b> = 
    apM ma mb

let (<&&>) (fn:'a -> 'b) (ma:LineParser<'a>) :LineParser<'b> = 
    fmapM fn ma


/// Perform two actions in sequence. Ignore the results of the second action if both succeed.
let seqL (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'a> = 
    parseLines { 
        let! a = ma
        let! b = mb
        return a
    }

/// Perform two actions in sequence. Ignore the results of the first action if both succeed.
let seqR (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'b> = 
    parseLines { 
        let! a = ma
        let! b = mb
        return b
    }

let (.>>>) (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'a> = 
    seqL ma mb

let (>>>.) (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'b> = 
    seqR ma mb


let mapM (p: 'a -> LineParser<'b>) (source:'a list) : LineParser<'b list> = 
    LineParser <| fun lines pos0 -> 
        let rec work pos ac ys = 
            match ys with
            | [] -> Ok (pos, List.rev ac)
            | z :: zs -> 
                match apply1 (p z) lines pos with
                | Err stk -> Err stk
                | Ok (pos1,ans) -> work pos1 (ans::ac) zs
        work pos0  [] source

let forM (source:'a list) (p: 'a -> LineParser<'b>) : LineParser<'b list> = 
    mapM p source

let optionToFailure (ma:LineParser<option<'a>>) 
                    (errMsg:string) : LineParser<'a> = 
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err stk -> Err stk
        | Ok (_,None) -> Err (parseError pos errMsg)
        | Ok (pos1, Some a) -> Ok (pos1,a)





/// Optionally parses. When the parser fails return None and don't move the cursor position.
let optional (ma:LineParser<'a>) : LineParser<'a option> = 
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err _ -> Ok (pos,None)
        | Ok (pos1,a) -> Ok (pos1,Some a)

let optionalz (ma:LineParser<'a>) : LineParser<unit> = 
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err _ -> Ok (pos, ())
        | Ok (pos1,_) -> Ok (pos1, ())

// *************************************
// Run functions

/// TODO - want a safer string split...
let runLineParser (input:string) (ma:LineParser<'a>) : Result<'a>= 
    match ma with 
    | LineParser fn ->  fn (input.Split('\n')) 0

let runLineParserFile (inputPath:string) (ma:LineParser<'a>) : Result<'a>= 
    let input : string [] = System.IO.File.ReadAllLines(inputPath)
    match ma with 
    | LineParser fn ->  fn input 0



// *************************************
// Single line parsers

let textline : LineParser<string> = 
    LineParser <| fun lines pos ->
        try
            let s = lines.[pos]
            Ok (pos+1, s)
        with
        | _ -> Err (parseError pos "textline failed - out of input")

let rmatch1 (pattern:string) : LineParser<string> = 
    let action : LineParser<string> = 
        textline >>>= fun input -> 
        match Regex.Matches(input, ".*") |> Seq.cast<Match> |> Seq.toList with
        | (m1::_) -> lpreturn m1.Value
        | _ -> throwError "rmatch - no match"
    action <&?> "rmatch"


// *************************************
// Parser combinators

// We don't care that these textual names clash with FParsec.
// The (textual) names for common parser combinators are so canonical that 
// changing them would hamper readabilty. Textual names can be used qualified.

/// End of document?
let eof : LineParser<unit> =
    LineParser <| fun lines pos ->
        if pos >= lines.Length then 
            Ok (pos, ())
        else
            Err (parseError pos "eof (not-at-end)")

/// Parses p without consuming input
let lookahead (ma:LineParser<'a>) : LineParser<'a> =
    LineParser <| fun lines pos ->
        match apply1 ma lines pos with
        | Err msg -> Err msg
        | Ok (_,a) -> Ok (pos,a)


let between (popen:LineParser<_>) (pclose:LineParser<_>) 
            (ma:LineParser<'a>) : LineParser<'a> =
    parseLines { 
        let! _ = popen
        let! ans = ma
        let! _ = pclose
        return ans 
    }
