open Prompt
open Markov
open System
open System.IO
open System.Text.RegularExpressions

let loadFiles filenames =
    let SEPARATORS = [| " "; "\t"; "\r\n"; "\n"; "--" |]
    let headingsToIgnore = new Regex("CONTENTS|[IVXLCDM]+\.")

    filenames
    |> List.ofSeq
    |> List.map (fun filename ->
        let text =
            // Read all lines in the file, split them into words
            File.ReadAllLines(filename)
            |> List.ofSeq
            |> List.map (fun l ->
                l.Trim().Split(SEPARATORS, StringSplitOptions.RemoveEmptyEntries)
                |> List.ofSeq)
            // Coalesce lines into paragraphs, starting a new paragraph each time a blank line is encountered.
            |> List.fold
                (fun ((current :: rest) as acc) line ->
                    match line with
                    | [] -> [] :: acc
                    | _ -> (current @ line) :: rest)
                [ [] ]
            |> List.rev
            // Filter out paragraphs that
            //   * are less than two words long
            //   * begin with the word "CONTENTS"
            //   * begin with a Roman numeral ending with a period (i.e. a chapter heading)
            |> List.filter (fun p ->
                match p with
                | []
                | _::[] ->
                    false
                | w::_ when headingsToIgnore.IsMatch(w) ->
                    false
                | _ ->
                    true)
        filename, text)

let fileOptions =
    Directory.EnumerateFiles(@"texts", "*.txt")
    |> List.ofSeq
    |> List.indexed
    |> List.map (fun (i, f) -> (i + 1), f)

// Present the file options
List.iter
    (fun (i, f) -> printfn "%d. %s" i f)
    fileOptions

let fileChoices =
    fileOptions
    |> promptMultiple "Choose texts (blank line to choose all): " (fst >> string)
    |> List.map snd

printfn "\nLoading..."
let files = loadFiles fileChoices

printf "Building markov chain..."

let markov =
    (Map.empty, files)
    ||> List.fold
        (fun markov (_, paragraphs) ->
            printf "."
            processText markov paragraphs)
    |> finalize

printf "\n"

let rec mainLoop () =
    printfn "\n%s\n" (generateParagraph markov) |> ignore
    match promptChoice "Another? " YesNo.hotkey [Yes; No] with
    | Yes -> mainLoop()
    | No -> ()

mainLoop() |> ignore