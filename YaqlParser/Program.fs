// Learn more about F# at http://fsharp.org

open System
open FParsec

let betweenChars l r = between (pchar l) (pchar r)
let spacesAround parser = spaces >>. parser .>> spaces
let spacesAroundChar = pchar >> spacesAround
let ident =
    let isIdentStart x = isAsciiLetter x || x = '_'
    let isIdentCont x = isAsciiLetter x || isDigit x || x = '_'
    identifier (IdentifierOptions(isIdentStart, isIdentCont))

type Argument = { name: string; value: ArgumentValue }
and ArgumentValue =
    | Variable of name: string
    | Boolean of value: bool
    | Int32 of value: int32
    | Double of value: double
    | String of value: string

type Property = { name: string; aliasedName: string option; arguments: Argument list option; select: Property list option }

let (property: Parser<Property, unit>, propertyImpl) = createParserForwardedToRef()
do propertyImpl :=
    let argList =
        // TODO LOL
        let boolValue = choice [
            pstring "true" >>. preturn (Boolean true)
            pstring "false" >>. preturn (Boolean false)
        ]
        let numericValue =
            let int32Value = pint32 .>> notFollowedByString "." |>> Int32
            let doubleValue = pfloat |>> Double
            (attempt int32Value) <|> doubleValue            
        let variableValue = pchar '$' >>. ident |>> Variable
        let stringValue =
            let notControlChar c = c <> '"' && c <> '\\'
            let controlSeq = choice [
                attempt (pchar '\\')
                attempt (pchar '"')
                attempt (pchar 'n') >>. preturn '\n'
                attempt (pchar 'r') >>. preturn '\r'
            ]
            let stringEnt = choice [
                attempt (satisfy notControlChar)
                attempt (pchar '\\' >>. controlSeq)
            ]
            betweenChars '"' '"' (many stringEnt) |>> string |>> String
        let argValue = choice [
            attempt boolValue
            attempt numericValue
            attempt variableValue
            attempt stringValue
        ] 
        let arg =
            ident .>>
            spacesAroundChar ':' .>>.
            argValue |>> fun (name, value) -> { name = name; value = value }
            
        let args = spacesAround (sepBy arg (spacesAroundChar ','))
        spacesAround (betweenChars '(' ')' args)
        
    let property' =    
        ident .>>. 
        opt (attempt (spacesAroundChar ':' >>. ident)) .>>
        spaces .>>.
        opt (attempt (argList)) .>>
        spaces .>>.
        opt (attempt (betweenChars '{' '}' (sepBy property (spacesAroundChar ','))))
        |>> fun (((name, realName), arguments), select) -> {
            name = Option.defaultValue name realName
            aliasedName = Option.map (fun _ -> name) realName
            arguments = arguments
            select = select
        }
        
    spacesAround property'
    
let queryParser =
    let queryParser' =
        pstring "query" >>.
        spaces >>.
        betweenChars '{' '}' (sepBy property (spacesAroundChar ','))
    
    spacesAround queryParser' .>> eof

let sampleQuery = """
query {
    users(firstName: "Suwako") {
        id,
        name: fullName,
        posts {
            id,
            text,
            allComments: comments(showDeleted: true, intTest: 123, doubleTest: 123.1, intTest2: 123) {
                id,
                text
            }
        }
    }
}
"""

[<EntryPoint>]
let main argv =
    match run queryParser sampleQuery with
    | Success(r, _, _) -> printfn "success %O" r
    | Failure(err, _, _) -> printfn "error %s" err
    0
