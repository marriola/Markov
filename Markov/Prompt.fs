module Prompt

open System

type YesNo = Yes | No
with
    static member hotkey = function
        | Yes -> "y"
        | No -> "n"

let promptChoice message hotkey options =
    let optionsDict =
        options
        |> List.map (fun opt -> hotkey opt, opt)
        |> Map.ofList

    let optionStrings = List.map hotkey options

    let rec getInput () =
        printf "%s" message
        printf "[%s] " (String.concat "/" optionStrings)
        let choice = Console.ReadLine().Trim()
        match Map.tryFind choice optionsDict with
        | Some opt -> opt
        | None -> getInput()

    getInput()

let promptMultiple message hotkey options =
    let optionsDict =
        options
        |> List.map (fun opt -> hotkey opt, opt)
        |> Map.ofList

    let optionStrings = List.map hotkey options

    let rec getInput () =
        printf "%s" message

        let inputs =
            match Console.ReadLine() with
            | "" ->
                optionStrings
            | input ->
                input.Split(',')
                |> Array.map (fun s -> s.Trim())
                |> List.ofArray

        let choices, invalid = List.partition (fun s -> Map.containsKey s optionsDict) inputs

        match choices, invalid with
        | _, [] ->
            List.map (fun s -> Map.find s optionsDict) choices
        | _, _ ->
            printfn "Invalid choices: %s" (String.concat ", " invalid)
            getInput()

    getInput()
