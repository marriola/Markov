module Markov
open System
open System.Collections.Generic

let (|KeyValue|_|) (kvp: KeyValuePair<_, _>) =
    Some (kvp.Key, kvp.Value)

type Word = Word of string | EndOfParagraph
with
    override this.ToString() =
        match this with
        | Word w -> w
        | EndOfParagraph -> ""
//        | EndOfParagraph -> "<END>" // For debugging purposes

type MarkovChain = Map<Word, Map<Word, int>>
type WeightedMarkovChain = Map<Word, (Word * float) list>

/// Converts the weights paired with each word into cutoff values to compare to
/// a randomly generated float in the range [0, 1). The output is sorted in
/// descending order so that 
///
/// This turns a list like
/// [foo, 0.1; bar, 0.5; baz, 0.4]
/// into
/// [foo, 0.0; baz, 0.1; bar, 0.5]
/// and finally into
/// [bar, 0.5; baz, 0.1; foo, 0.0]
let private calculateCutoffs weights =
    let rec inner lastWeight out weights =
        match weights with
        | [] ->
            out
        | (w, weight)::xs ->
            let nextWord = (w, lastWeight)
            inner (weight + lastWeight) (nextWord :: out) xs
    weights
    |> List.sortBy (fun (_, weight) -> weight)
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

/// Converts the word counts in the markov chain to weighted cutoff values.
let finalize markov =
    markov
    |> Seq.map (fun (KeyValue (headword, counts)) ->
        let total =
            counts
            |> Seq.sumBy (fun (KeyValue (_, count)) -> count)
            |> float
        let cutoffs =
            counts
            |> List.ofSeq
            |> List.map (fun (KeyValue (key, count)) -> key, float count / total)
            |> List.sortByDescending (fun (_, weight) -> weight)
            |> calculateCutoffs
        headword, cutoffs)
    |> Map.ofSeq

/// Generates a paragraph of text from a weighted markov chain.
let generateParagraph (weightedMarkov: WeightedMarkovChain) =
    let random = Random()
    let rec inner (lastWord::_ as out) =
        match Map.tryFind (Word lastWord) weightedMarkov with
        | Some cutoffs ->
            let nextWeight = random.NextDouble()
            let nextWord, _ =
                cutoffs
                |> List.find (fun (_, cutoff) -> nextWeight > cutoff)
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