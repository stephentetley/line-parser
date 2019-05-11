// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

namespace LineParser

module Parser = 


    open System
    open System.Text
    open System.Text.RegularExpressions


    // TODO 
    // Having Perm parsers and "Bounded Seas" would be very convenient.


    type LineNumber = int
    type ErrMsg = string


    type ParserState = LineNumber


    type Answer<'a> = Result<'a * ParserState, ErrMsg>

    type LineParser<'a> = 
        LineParser of (RegexOptions -> string [] -> ParserState -> Answer<'a>)

    let inline private apply1 (ma : LineParser<'a>) 
                              (opts : RegexOptions)
                              (body : string [])
                              (state : ParserState) : Answer<'a>= 
        let (LineParser f) = ma in f opts body state

    let inline mreturn (x:'a) : LineParser<'a> = 
        LineParser <| fun _ _ st  -> Ok (x, st)


    let inline private bindM (ma : LineParser<'a>) 
                             (f : 'a -> LineParser<'b>) : LineParser<'b> =
        LineParser <| fun opts body st -> 
            match apply1 ma opts body st with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (f a) opts body st1


    let inline private zeroM () : LineParser<'a> = 
        LineParser <| fun _ _ _ -> Error "zeroM"


    let inline private combineM (ma:LineParser<'z>) 
                                    (mb:LineParser<'a>) : LineParser<'a> = 
        LineParser <| fun opts body st -> 
            match apply1 ma opts body st with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 mb opts body st1


    let inline private  delayM (fn:unit -> LineParser<'a>) : LineParser<'a> = 
        bindM (mreturn ()) fn 



    type LineParserBuilder () = 
        member self.Return x            = mreturn x
        member self.Bind (p,f)          = bindM p f
        member self.Zero ()             = zeroM ()
        member self.Combine (ma,mb)     = combineM ma mb
        member self.Delay fn            = delayM fn


    // Prefer "parse" to "parser" for the _Builder instance

    let (lineParser:LineParserBuilder) = new LineParserBuilder ()


    let ( >>= ) (ma:LineParser<'a>) (fn:'a -> LineParser<'b>) : LineParser<'b> = 
        bindM ma fn


    // *************************************
    // Errors

    let parseError (msg:string) : LineParser<'a> = 
        LineParser <| fun _ _ _ -> Error msg

    let swapError (msg:string) (parser:LineParser<'a>) : LineParser<'a> = 
        LineParser <| fun opts body st ->
            match apply1 parser opts body st with
            | Error _ -> Error msg
            | Ok (pos1,a) -> Ok (pos1,a)

    let ( <?> ) (parser:LineParser<'a>) (msg:string) : LineParser<'a> = 
        swapError msg parser



    // Common monadic operations
    let fmapM (fn:'a -> 'b) (parser:LineParser<'a>) : LineParser<'b> = 
        LineParser <| fun opts body st -> 
           match apply1 parser opts body st with
           | Error msg -> Error msg
           | Ok (a, st1) -> Ok (fn a, st1)



    /// Operator for fmap.
    let ( |>> ) (parser:LineParser<'a>) (fn:'a -> 'b) : LineParser<'b> = 
        fmapM fn parser

    /// Flipped fmap.
    let ( <<| ) (fn:'a -> 'b) (parser:LineParser<'a>) : LineParser<'b> = 
        fmapM fn parser

    let ignoreM (parser:LineParser<'a>) : LineParser<unit> = 
        parser |>> ignore

    // ****************************************************
    // Monadic operations

    let getRegexOptions () : LineParser<RegexOptions> = 
        LineParser <| fun opts _ st -> Ok (opts, st)

    let localOptions (modify : RegexOptions -> RegexOptions) (ma: LineParser<'a>) : LineParser<'a> = 
        LineParser <| fun opts body st -> 
            apply1 ma (modify opts) body st

    let ignoreCase (ma : LineParser<'a>) : LineParser<'a> = 
        localOptions (fun opts -> RegexOptions.IgnoreCase ||| opts) ma

    // liftM (which is fmap)
    let liftM (fn:'a -> 'x) (ma:LineParser<'a>) : LineParser<'x> = 
        fmapM fn ma

    let liftM2 (fn:'a -> 'b -> 'x) 
                (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'x> = 
        lineParser { 
            let! a = ma
            let! b = mb
            return (fn a b)
        }

    let liftM3 (fn:'a -> 'b -> 'c -> 'x) 
                (ma:LineParser<'a>) (mb:LineParser<'b>) 
                (mc:LineParser<'c>) : LineParser<'x> = 
        lineParser { 
            let! a = ma
            let! b = mb
            let! c = mc
            return (fn a b c)
        }

    let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) 
                (ma:LineParser<'a>) (mb:LineParser<'b>) 
                (mc:LineParser<'c>) (md:LineParser<'d>) : LineParser<'x> = 
        lineParser { 
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
        lineParser { 
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
        lineParser { 
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
        LineParser <| fun opts body st ->
            match apply1 ma opts body st with
            | Error err1 -> 
                match apply1 mb opts body st with
                | Error err2 -> Error (sprintf "alt (%s, %s)" err1 err2)
                | Ok (b, st1) -> Ok (b, st1)
            | Ok (a, st1) -> Ok (a, st1)

    let ( <|> ) (ma:LineParser<'a>) (mb:LineParser<'a>) : LineParser<'a> = 
        alt ma mb <?> "(<|>)"


    // Haskell Applicative's (<*>)
    let apM (mf:LineParser<'a ->'b>) (ma:LineParser<'a>) : LineParser<'b> = 
        lineParser { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    let ( <*> ) (ma:LineParser<'a -> 'b>) (mb:LineParser<'a>) : LineParser<'b> = 
        apM ma mb

    let ( <&> ) (ma:LineParser<'a>) (fn:'a -> 'b)  :LineParser<'b> = 
        fmapM fn ma


    /// Perform two actions in sequence. Ignore the results of the second action if both succeed.
    let seqL (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'a> = 
        lineParser { 
            let! a = ma
            let! b = mb
            return a
        }

    /// Perform two actions in sequence. Ignore the results of the first action if both succeed.
    let seqR (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'b> = 
        lineParser { 
            let! a = ma
            let! b = mb
            return b
        }

    let ( .>> ) (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'a> = 
        seqL ma mb

    let ( >>. ) (ma:LineParser<'a>) (mb:LineParser<'b>) : LineParser<'b> = 
        seqR ma mb

    /// In CPS
    let mapM (p: 'a -> LineParser<'b>) (source:'a list) : LineParser<'b list> = 
        LineParser <| fun opts body state0 -> 
            let rec work ys st fk sk = 
                match ys with
                | [] -> sk ([],st)
                | z :: zs -> 
                    match apply1 (p z) opts body st with
                    | Error msg -> fk msg
                    | Ok (a, st1) -> work zs st1 fk (fun (acc,st2) -> sk (a::acc, st2))
            work source state0 (fun msg -> Error msg) (fun x -> Ok x)

    let forM (source :'a list) (parser : 'a -> LineParser<'b>) : LineParser<'b list> = 
        mapM parser source

    let option (defaultValue : 'a) (parser : LineParser<'a>) : LineParser<'a> = 
        parser <|> mreturn defaultValue

    /// Optionally parses. When the parser fails return None and don't move the cursor position.
    let optionMaybe (parser : LineParser<'a>) : LineParser<'a option> = 
        LineParser <| fun opts body st ->
            match apply1 parser opts body st with
            | Error _ -> Ok (None, st)
            | Ok (a, st1) -> Ok (Some a, st1)

    let optional (parser : LineParser<'a>) : LineParser<unit> = 
        LineParser <| fun opts body st ->
            match apply1 parser opts body st with
            | Error _ -> Ok ((), st)
            | Ok (_, st1) -> Ok ((), st1)

    // *************************************
    // Run functions

    /// Caution is the split good enough?
    let runLineParser (input:string) (ma:LineParser<'a>) : Answer<'a>= 
        let opts = RegexOptions.None
        let lines = input.Split(separator = [| "\r\n" ; "\r" ; "\n" |], options = StringSplitOptions.None)
        match ma with 
        | LineParser fn ->  fn opts lines 0

    let runLineParserFile (inputPath:string) (ma:LineParser<'a>) : Answer<'a>= 
        let opts = RegexOptions.None
        let lines : string [] = System.IO.File.ReadAllLines(inputPath)
        match ma with 
        | LineParser fn ->  fn opts lines 0



    // *************************************
    // Single line parsers

    let line : LineParser<string> = 
        LineParser <| fun _ body st ->
            try 
                let ans = body.[st] in Ok (ans, st+1)
            with
            | _ -> Error "line - out of input"
            


    let skipline : LineParser<unit> = 
        ignoreM line


//let rmatch1 (pattern:string) : LineParser<string> = 
//    let action = 
//        textline >>= fun input -> 
//        match Regex.Matches(input, pattern) |> Seq.cast<Match> |> Seq.toList with
//        | (m1::_) -> mreturn m1.Value
//        | _ -> throwError "rmatch1 - no match"
//    action <&?> "rmatch1"

//let rgroups (pattern:string) : LineParser<GroupCollection>= 
//    let parse1 = 
//        textline >>= fun input -> 
//        let m1 = Regex.Match(input, pattern) 
//        if m1.Success then
//            mreturn m1.Groups
//        else
//            throwError "groups - no match"
//    parse1 <&?> "rgroups"



    // *************************************
    // Parser combinators



    /// End of document?
    let eof : LineParser<unit> =
        LineParser <| fun _ body st ->
            if st >= body.Length then Ok ((), st) else Error "eof - not at end"

    /// Parses p without consuming input
    let lookahead (parser:LineParser<'a>) : LineParser<'a> =
        LineParser <| fun opts body st ->
            match apply1 parser opts body st with
            | Error msg -> Error msg
            | Ok (a, _) -> Ok (a, st)


    let between (popen:LineParser<_>) (pclose:LineParser<_>) 
                (parser:LineParser<'a>) : LineParser<'a> =
        lineParser { 
            let! _ = popen
            let! ans = parser
            let! _ = pclose
            return ans 
        }


    let many (parser:LineParser<'a>) : LineParser<'a list> = 
        LineParser <| fun opts body state0 ->
            let rec work st cont = 
                match apply1 parser opts body st with
                | Error _ -> cont ([], st)
                | Ok (a, st1) -> work st1 (fun (ac,st2) -> cont (a::ac, st2))
            work state0 (fun x -> Ok x)


    let many1 (parser:LineParser<'a>) : LineParser<'a list> = 
        lineParser { 
            let! one = parser
            let! more = many parser
            return (one::more) 
        } 

    let skipMany (parser:LineParser<'a>) : LineParser<unit> = 
        LineParser <| fun opts body state0 ->
            let rec work st cont = 
                match apply1 parser opts body st with
                | Error _ -> cont st
                | Ok (_, st1) -> work st1 cont
            work state0 (fun st -> Ok ((), st))

    let skipMany1 (parser:LineParser<'a>) : LineParser<unit> = 
        lineParser { 
            let! _ = parser
            let! _ = skipMany parser
            return () 
        } 




    let sepBy1 (parser:LineParser<'a>) 
                (sep:LineParser<_>) : LineParser<'a list> = 
        lineParser { 
            let! one = parser
            let! more = many (sep >>. parser) 
            return (one::more)
        }

    let sepBy (parser:LineParser<'a>) 
                (sep:LineParser<_>) : LineParser<'a list> = 
        sepBy1 parser sep <|> mreturn []


    let skipSepBy1 (parser:LineParser<'a>) (sep:LineParser<_>) : LineParser<unit> = 
        lineParser { 
            let! _ = parser
            let! _ = skipMany (sep >>. parser) 
            return ()
        }

    let skipSepBy (parser:LineParser<'a>) 
                  (sep:LineParser<_>) : LineParser<unit> = 
        skipSepBy1 parser sep <|> mreturn ()



    let sepEndBy1 (parser:LineParser<'a>) (sep:LineParser<_>) : LineParser<'a list> = 
        lineParser { 
            let! one = parser
            let! more = many (sep >>. parser) 
            let! _ = optional sep
            return (one::more)
        }

    let sepEndBy (parser:LineParser<'a>) (sep:LineParser<_>) : LineParser<'a list> = 
        sepEndBy1 parser sep <|> mreturn []


    let skipSepEndBy1 (parser:LineParser<'a>) (sep:LineParser<_>) : LineParser<unit> = 
        lineParser { 
            let! _ = parser
            let! _ = skipMany (sep >>. parser) 
            let! _ = optional sep
            return ()
        }

    let skipSepEndBy (parser:LineParser<'a>) (sep:LineParser<_>) : LineParser<unit> = 
        skipSepEndBy1 parser sep <|> mreturn ()



    let manyTill (parser:LineParser<'a>) 
                    (terminator:LineParser<_>) : LineParser<'a list> = 
        LineParser <| fun opts body state0 ->
            let rec work st fk sk = 
                match apply1 terminator opts body st with
                | Error msg -> 
                    match apply1 parser opts body st with
                    | Error msg -> fk msg
                    | Ok (a, st1) -> work st1 fk (fun (ac, st2) -> sk (a::ac, st2))
                | Ok (_, st2) -> sk ([], st2)
            work state0 (fun msg -> Error msg) (fun ans -> Ok ans)

    let many1Till (parser:LineParser<'a>) 
                    (terminator:LineParser<_>) : LineParser<'a list> = 
         lineParser { 
            let! one = parser 
            let! more = manyTill parser terminator
            return (one::more)
        }


    let skipManyTill (parser:LineParser<'a>) (terminator:LineParser<_>) : LineParser<unit> = 
        LineParser <| fun opts body state0 ->
            let rec work st fk sk = 
                match apply1 terminator opts body st with
                | Error _ -> 
                    match apply1 parser opts body st with
                    | Error msg -> fk (sprintf "skipManyTill (%s)" msg)
                    | Ok (_, st1) -> work st1 fk sk
                | Ok (_, st1) -> sk st1
            work state0 (fun msg -> Error msg) (fun st -> Ok ((), st))

    let skipMany1Till (ma:LineParser<'a>) (terminator:LineParser<_>) : LineParser<unit> = 
        ma >>. (skipManyTill ma terminator)    



    // ****************************************************
    // Regex matching

    /// Current line 
    let isMatch (pattern:string) : LineParser<bool> = 
        lineParser { 
            let! input = line 
            let! regexOpts =  getRegexOptions ()
            return Regex.IsMatch( input = input
                                , pattern = pattern
                                , options = regexOpts )
        }

    /// Current line 
    let isNotMatch (pattern:string) : LineParser<bool> = 
        isMatch pattern |>> not


    /// Current line 
    let regexMatch (pattern:string) : LineParser<RegularExpressions.Match> =
        lineParser { 
            let! input = line 
            let! regexOpts =  getRegexOptions ()
            return Regex.Match( input = input
                            , pattern = pattern
                            , options = regexOpts )
        }

    /// Current line 
    let matchValue (pattern:string) : LineParser<string> =
        regexMatch pattern >>= fun matchObj -> 
        if matchObj.Success then
            mreturn matchObj.Value
        else
            parseError "no match"