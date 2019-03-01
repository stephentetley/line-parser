// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Text.Encoding.dll"
#r "system.Xml.Linq.dll"

open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions

open Microsoft.FSharp.Collections


// Use FSharp.Data for CSV output
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.0.0\lib\netstandard2.0"
#r @"FSharp.Data.dll"
open FSharp.Data


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec
open FParsec.CharParsers

#load "FParsecLines.fs"
open  LineParser.FParsecLines


let wordExport = @"G:\work\pdftotext\wordexport.txt"


let runFParsec parser path = 
    match runParserOnFile parser () path Encoding.Default with
    | Success(ans,_,_) -> ans
    | Failure(errMsg,_,_) -> failwith errMsg


let lineContents (trim:bool) : Parser<string, unit> = 
    let work () = restOfLine true |>> (fun s -> if trim then s.Trim() else s)
    work ()

let lineBound (p1:Parser<'a,'u>) : Parser<'a,'u> = 
    p1 .>> restOfLine true

let pline s = lineBound (pstring s)

let skipLine () : Parser<unit,'u> = 
    restOfLine true |>> ignore


/// TODO not actually in CPS due to the (>>=)
let ntimes (count:int) (p1:Parser<'a,'u>) : Parser<'a list,'u> = 
    let rec work (acc:'a list) 
                 (i:int) 
                 (cont:'a list -> Parser<'a list,'u>) = 
        if i <= 0 then 
            cont (List.rev acc)
        else
            p1 >>= fun x -> 
            work (x::acc) (i-1) cont
    work [] count preturn

/// TODO not actually in CPS due to the (>>=)
let sequence (parsers:Parser<'a,'u> list) : Parser<'a list,'u> = 
    let rec work (acc:'a list) 
                 (ps:Parser<'a,'u> list) 
                 (cont:'a list -> Parser<'a list,'u>) = 
        match ps with
        | [] -> cont (List.rev acc)
        | p1 :: rest -> 
            p1 >>= fun x -> 
            work (x::acc) rest cont
    work [] parsers preturn
    
let skipLines (count:int) : Parser<unit,'u> = 
    ntimes count (skipLine ()) |>> ignore

let pageSeparator () = pline "CatchmentZoning2018_v08.xlsx"

let headers () = 
    let headers = 
        ["DAZ"; "Receiving STW"; "SAI Reference"; "Common name"; "status";
            "Site address"; "postcode"; "type"; "2018 zone"]
    
    sequence (List.map pline headers)


let test01 () = 
    runFParsec (lineContents true) wordExport



type Payload = 
    { Daz: int 
      ReceivingStw: string
      SaiNumber: string
      CommonName: string
      Status: string
      Address: string
      Postcode: string
      AssetType: string
      Zone2018: string
    }

let pint : Parser<int,unit>  = 
    puint32 |>> int

let parsePayload : Parser<Payload, unit> = 
    parse {
        let! daz = lineBound (spaces >>. pint)
        let! stw = lineContents true
        let! sai = lineContents true
        let! name = lineContents true
        let! status = lineContents true
        let! addr = lineContents true
        let! postcode = lineContents true
        let! atype = lineContents true
        let! zone = lineContents true

        return { 
            Daz = daz
            ReceivingStw = stw
            SaiNumber = sai
            CommonName = name
            Status =status
            Address = addr
            Postcode = postcode
            AssetType = atype
            Zone2018 = zone
        }
    }

let test02 () = 
    runFParsec (ntimes 9 (lineContents true)) wordExport

let test03 () = 
    runFParsec (headers () >>. many parsePayload) wordExport

let parsePage () : Parser<Payload list,unit> = 
    headers () >>. manyTill parsePayload (pageSeparator ())


type OutputTable = 
    CsvProvider< Sample = "DAZ(int),Stw(string),SaiRef(string),Common Name(string),Status(string),Address(string),Postcode(string),AssetType(string),Zone 2018(string)",
                 Schema = "DAZ(int),Stw(string),SaiRef(string),Common Name(string),Status(string),Address(string),Postcode(string),AssetType(string),Zone 2018(string)",
                 HasHeaders = true >

type OutputRow = OutputTable.Row

let conv1 (row:Payload) : OutputRow = 
    new OutputRow ( daz = row.Daz
                  , stw = row.ReceivingStw
                  , saiRef = row.SaiNumber
                  , commonName = row.CommonName
                  , status = row.Status
                  , address = row.Address
                  , postcode = row.Postcode
                  , assetType = row.AssetType
                  , zone2018 = row.Zone2018 )


let main () = 
    let outfile = @"G:\work\pdftotext\hazardous-areas.csv"
    let payload = runFParsec (many (parsePage ()) |>> List.concat) wordExport
    let csvrows : seq<OutputRow> = payload |> List.map conv1 |> Seq.ofList
    let table = new OutputTable(csvrows)
    use sw = new StreamWriter(path=outfile, append=false)
    table.Save(writer = sw, separator = ',', quote = '"')

