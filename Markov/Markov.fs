module Markov
open Word
open System
open System.Collections.Generic

let (|KeyValue|_|) (kvp: KeyValuePair<_, _>) =
    Some (kvp.Key, kvp.Value)

type MarkovChain = Map<Word, Map<Word, int>>
type WeightedMarkovChain = Map<Word, (Word * float) list>

/// Sum the weights cumulatively in ascending order, then sort them in descending order.
/// By doing this, we subdivide the range of possible values, [0, 1), according to the
/// specified probability distribution.
///
/// This turns a list like
/// [foo, 0.1; bar, 0.5; baz, 0.4]
/// into
/// [bar, 0.5; baz, 0.1; foo, 0.0]
let private sumWeights weights =
    let rec inner lastWeight out weights =
        match weights with
        | [] ->
        //| (Word "", _)::_ ->
            out
        | (w, weight)::xs ->
            let nextWord = (w, lastWeight)
            inner (weight + lastWeight) (nextWord :: out) xs
    weights
    |> List.sortBy (fun ((_, weight)) -> weight)
    |> inner 0.0 []

/// Adds a word to a markov chain
let addWord markov lastWord word =
    let nextCounts =
        match Map.tryFind lastWord markov with
        | None ->
            Map.ofList [word, 1]
        | Some counts ->
            let nextCount =
                match Map.tryFind word counts with
                | None -> 1
                | Some count -> count + 1
            Map.add word nextCount counts
    Map.add lastWord nextCounts markov
        
/// Adds the words in a paragraph to a markov chain.
let processParagraph markov words =
    let rec processParagraph' markov lastWord words =
        match words with
        | [] -> markov
        | x::xs ->
            let markov = addWord markov lastWord x
            processParagraph' markov x xs

    let firstWord :: rest = (List.map Word words) @ [EndOfParagraph]
    processParagraph' markov firstWord rest
        
/// Adds the words in a list of paragraphs to a markov chain.
let processText markov lines =
    let rec processText' (markov: MarkovChain) lines =
        match lines with
        | [] -> markov
        | x::xs ->
            processText' (processParagraph markov x) xs
    processText' markov lines

let finalize markov =
    markov
    |> Seq.map (fun (KeyValue (headword, counts)) ->
        let total =
            counts
            |> Seq.sumBy (fun (KeyValue (_, count)) -> count)
            |> float
        let weights =
            counts
            |> List.ofSeq
            |> List.map (fun (KeyValue (key, count)) -> key, float count / total)
            |> List.sortByDescending (fun (_, weight) -> weight)
            |> sumWeights
        headword, weights)
    |> Map.ofSeq

let generateParagraph (weightedMarkov: WeightedMarkovChain) =
    let random = Random()
    let rec inner (lastWord::_ as out) =
        let nextWeight = random.NextDouble()
        match Map.tryFind (Word lastWord) weightedMarkov with
        | Some weights ->
            let nextWord, _ =
                weights
                |> List.find (fun (_, weight) -> nextWeight > weight)
            inner (string nextWord :: out)
        | None ->
            out
            
    let (Word firstWord) =
        weightedMarkov
        |> Seq.map (fun kvp -> kvp.Key)
        // EndOfParagraph will never start a paragraph, so we can match on Word here.
        |> Seq.filter (fun (Word w) -> Char.IsUpper(w.[0]))
        |> Seq.sortBy (fun _ -> random.Next())
        |> Seq.head
        
    inner [firstWord]
    |> List.rev
    |> String.concat " "