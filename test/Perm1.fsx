// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"

// Experiments towards Perm parsing


#load @"..\src\LineParser\Parser.fs"
open LineParser.Parser

type Perms<'a> = 
    | Choice of Branch<'a> list
    | Empty of 'a
and Branch<'a> = 
    | Branch of (Perms<obj -> 'a>) * LineParser<obj>


// Doesn't work - also the Haskell version relies on laziness...
let rec perms (ps:Perms<'a>) : LineParser<'a> = 
    match ps with
    | Empty a -> mreturn a
    | Choice chs -> 
        let parseBranch (branch:Branch<'a>) : LineParser<'a> = 
            match branch with
            | Branch (transf,px) -> 
                parseLines { 
                    let! (a:obj) = px
                    let! (fn:obj -> 'a) = perms transf 
                    let ans = fn a
                    return ans
                }
        List.foldBack (fun p q -> p <||> q)  (List.map parseBranch chs) (LineParser.Zero ())


