// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#r "netstandard"

// Experiments towards Perm parsing


#load @"..\src\LineParser\Parser.fs"
open LineParser.Parser


// Dynamic types to the rescue
type Perms = Map<string,LineParser<obj>>
type Answers = Map<string,obj>



let getString (key:string) (omap:Perms) : LineParser<string> = 
    match Map.tryFind key omap with
    | Some p -> p |>> (fun o -> o :?> string)
    | None -> throwError "could not find"



let permsAdd (label:string) (parser:LineParser<'a>) (perms:Perms) : Perms = 
    let p2 = parser |>> (fun s -> s :> obj)
    Map.add label p2 perms

type PermExtractor<'a> = PermExtractor of (Answers -> 'a)

type Label = string

type PermStep = 
    | Mandatory of Label * LineParser<obj>
    | Optional of Label * obj * LineParser<obj>

type PermParser<'a> = 
    | PermParser of PermStep list
    member v.GetParsers = match v with | PermParser(xs) -> xs






let emptyPerms : PermParser<'a> = 
    PermParser []


/// Note - the PermParser type parameters are just phantoms.
let addPerm (perms:PermParser<'a>) (p1:LineParser<'b>) (label:string)  : PermParser<'c> = 
    let dynP = Mandatory (label, p1 |>> (fun s -> s :> obj))
    PermParser(dynP :: perms.GetParsers)

let addOptional (perms:PermParser<'a>) (p1:LineParser<'b>) (label:string) (defaultValue:'b) : PermParser<'c> = 
    let dynP: LineParser<obj> = p1 |>> (fun s -> s :> obj)
    let optP = Optional (label, defaultValue :> obj, dynP)
    PermParser(optP :: perms.GetParsers)


let private answersConcat (a1:Answers) (a2:Answers) : Answers = 
    List.fold (fun ac (k,v) -> Map.add k v ac) a1 (Map.toList a2)





let parsingPhase (perms:PermParser<'a>) : LineParser<Answers> = 
    let trailingOptionals (steps:PermStep list) : option<Answers> = 
        let rec work (xs:PermStep list) (ac:Answers)   = 
            match xs with 
            | Optional(l,v,_) :: ys -> work ys (Map.add l v ac) 
            | Mandatory _ :: _ -> None
            | [] -> Some ac
        work steps Map.empty

    let rec productive1 (steps:PermStep list) (tries:PermStep list) : 
                            LineParser<option<PermStep list * Label *obj>> = 
        match steps with
        | [] -> mreturn None
        | p1 :: ps -> 
            match p1 with
            | Mandatory (l,p) -> 
                optional p >>= fun ans ->
                    match ans with
                    | None -> productive1 ps (p1::tries)
                    | Some a -> mreturn <| Some ((List.rev tries) @ ps,l , a)
            | Optional(l,_,p) -> 
                optional p >>= fun ans -> 
                    match ans with
                    | None -> productive1 ps (p1::tries)
                    | Some a -> mreturn <| Some ((List.rev tries) @ ps, l, a)
    
    let rec work (steps:PermStep list) (ac:Answers) : LineParser<Answers> = 
        match steps with
        | [] -> mreturn ac
        | _ -> 
            productive1 steps [] >>= fun ans -> 
                match ans with
                | None -> 
                    // Nothing has succeeded but the input might have been
                    // all optional in the first place
                    match trailingOptionals steps with
                    | Some vals -> mreturn vals
                    | None -> throwError "perms - not productive"
                | Some(steps1,lbl,ans) -> 
                    match trailingOptionals steps1 with
                    | Some vals -> 
                        let a1 = Map.add lbl ans ac in mreturn (answersConcat a1 vals)
                    | None -> work steps1  (Map.add lbl ans ac)
    work perms.GetParsers Map.empty



let extractingPhase (extractors:PermExtractor<'a>) (answers:Answers): LineParser<'a> = 
    throwError "todo"

let perms (perms:PermParser<'a>) (extractor:PermExtractor<'a>) : LineParser<'a> = 
    parsingPhase perms >>= extractingPhase extractor


