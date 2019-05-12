// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace LineParser

module Regex = 


    open System.Text
    open System.Text.RegularExpressions

    open LineParser.Parser

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

    let matchGroups (pattern:string) : LineParser<RegularExpressions.GroupCollection> =
        regexMatch pattern >>= fun matchObj -> 
        if matchObj.Success then
            mreturn matchObj.Groups
        else
            parseError "no match"

    let private isNumber (str:string) : bool = 
        let pattern = "^\d+$"
        Regex.IsMatch(input = str, pattern = pattern)
     
    /// This only returns user named matches, not the 'internal' ones given 
    /// numeric names by .Net's regex library.
    let matchNamedMatches (pattern:string) : LineParser<Map<string, string>> =
        regexMatch pattern >>= fun matchObj -> 
        if matchObj.Success then
            let nameValues = matchObj.Groups |> Seq.cast<Group> 
            let matches = 
                Seq.fold (fun acc (grp:Group) -> 
                            if isNumber grp.Name then acc else Map.add grp.Name grp.Value acc)
                        Map.empty
                        nameValues
            mreturn matches
        else
            parseError "no match"

    let anyMatch (patterns:string []) : LineParser<bool> = 
        let (predicates : LineParser<bool> list) = 
            patterns |> Array.toList |> List.map isMatch
        anyM predicates


    let allMatch (patterns:string []) : LineParser<bool> = 
        let (predicates : LineParser<bool> list) = 
            patterns |> Array.toList |> List.map isMatch
        allM predicates

    let matchStart (pattern:string) : LineParser<int> =
        regexMatch pattern >>= fun matchObj -> 
        if matchObj.Success then
            mreturn matchObj.Index
        else
        parseError "no match"


    let matchEnd (pattern:string) : LineParser<int> =
        regexMatch pattern >>= fun matchObj -> 
        if matchObj.Success then
            let start = matchObj.Index
            mreturn (start + matchObj.Length)
        else
            parseError "no match"

    let private contentsFrom (startIndex: int) : LineParser<string> = 
        lineParser { 
            let! str = line
            return! liftOperation "contentsFrom" (fun _ ->  str.Substring(startIndex= startIndex))
        }

    let private contentsTo (endIndex: int) : LineParser<string> = 
        lineParser { 
            let! str = line
            return! liftOperation "contentTo" (fun _ -> str.Substring(startIndex = 0, length = endIndex))
        }


    let leftOfMatch (pattern:string) : LineParser<string> =
        matchStart pattern >>= fun ix -> 
        contentsTo ix
         

    let rightOfMatch (pattern:string) : LineParser<string> =
        matchEnd pattern >>= fun ix -> 
        contentsFrom ix