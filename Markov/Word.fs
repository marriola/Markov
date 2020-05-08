module Word

type Word = Word of string | EndOfParagraph
with
    override this.ToString() =
        match this with
        | Word w -> w
        | EndOfParagraph -> ""
//        | EndOfParagraph -> "<END>" // For debugging purposes
