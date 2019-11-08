module Search

type OnDiskIndex = {
    TermEntries: int
    TermAtlas: System.IO.Stream
    TermIndex: System.IO.Stream
    StringEntries: int
    StringAtlas: System.IO.Stream
    StringIndex: System.IO.Stream
    BacklinkEntries: int
    BacklinkAtlas: System.IO.Stream
    BacklinkIndex: System.IO.Stream
}

/// <summary>
/// Regex for matching page titles, which we search using backlinks instead of
/// full-text
/// </summary>
let page_title_re = new System.Text.RegularExpressions.Regex("[A-Z][a-z]+([A-Z][a-z]+)+")

/// <summary>
/// Builds a list of all of the page titles in the wiki
/// </summary>
let build_titles_index (wiki_dir: string) =
    let wiki_dir_info = new System.IO.DirectoryInfo(wiki_dir)
    wiki_dir_info.GetFiles()
    |> Seq.map (fun info -> System.String.Intern(info.Name.Replace(".html", "")))
    |> Set.ofSeq

/// <summary>
/// Queries the page title index
/// </summary>
let query_titles_index (title_index: Set<string>) (query: string)  =
    let query = query.ToLower()

    title_index
    |> Seq.filter (fun title -> title.ToLower().Contains(query.ToLower()))
    |> Set.ofSeq

let rng = new System.Random()

/// <summary>
/// Samples a random page from the title index
/// </summary>
let random_page_title (title_index: Set<string>) =
    let incr = 1.0 / double (Set.count title_index)
    let (result, _) =
        title_index
        |> Set.fold (fun (result, counter) value ->
                     match (result, counter - incr) with
                     | (Some _, _) -> (result, counter)
                     | (_, remaining) when remaining < 0.0 -> (Some value, 0.0)
                     | (_, remaining) -> (None, remaining))
                    (None, rng.NextDouble())

    match result with
    | Some title -> title
    | None -> Set.minElement title_index

/// <summary>
/// Gets the basic words in a string
/// </summary>
let get_search_terms (text: string) =
    // We're borrowing the Elasticsearch model here, where we treat
    // non-alphanumerics as word delimiters in addition to letter/number
    // transitions and case transitions. We also strip plurals and index only the
    // singular form
    let WORD_STATE_NONE = 0
    let WORD_STATE_LOWER = 1
    let WORD_STATE_UPPER = 2
    let WORD_STATE_DIGIT = 3

    let upper_chars = Set.ofList ['A'..'Z']
    let lower_chars = Set.ofList ['a'..'z']
    let digit_chars = Set.ofList ['0'..'9']

    let add_word (word: char list) (words: Set<string>) =
        let word =
            match word with
            | 's' :: rest -> rest
            | _ -> word

        let chars =
            word
            |> List.rev
            |> Array.ofList

        if Array.length chars > 0 then
            let result = (new System.String(chars)).ToLower()
            Set.add result words
        else
            words

    let rec iter (word_state, word, words) input =
        // Continuation states: we can continue the same word if any of these
        // are true. The lower-case check is special because we want to allow
        // upper-case to transition into lower case ("TitleCase") and include
        // both sides in the same word ({"Title", "Case"})
        if (word_state = WORD_STATE_NONE || word_state = WORD_STATE_UPPER) && Set.contains input upper_chars then
            (WORD_STATE_UPPER, input :: word, words)
        elif (word_state = WORD_STATE_NONE || word_state = WORD_STATE_LOWER || word_state = WORD_STATE_UPPER) && Set.contains input lower_chars then
            (WORD_STATE_LOWER, input :: word, words)
        elif (word_state = WORD_STATE_NONE || word_state = WORD_STATE_DIGIT) && Set.contains input digit_chars then
            (WORD_STATE_DIGIT, input :: word, words)

        // Case splitting: these are like the other word building cases, but
        // they have a definite state after. We know that they will have some
        // input already in the word because WORD_STATE_NONE was processed already.
        elif Set.contains input upper_chars then
            (WORD_STATE_UPPER, [input], add_word word words)
        elif Set.contains input lower_chars then
            (WORD_STATE_LOWER, [input], add_word word words)
        elif Set.contains input digit_chars then
            (WORD_STATE_DIGIT, [input], add_word word words)

        // Punctuation splitting: anything that's non-alphanumeric is ignored
        // for indexing purposes
        elif word = [] then
            (WORD_STATE_NONE, [], words)
        else
            (WORD_STATE_NONE, [], add_word word words)

    let (_, remainder, words) =
        text
        |> Seq.fold iter (WORD_STATE_NONE, [], Set.empty)

    let result =
        if remainder = [] then
            words
        else
            add_word remainder words

    result

/// <summary>
/// Gets the number of entries within an atlas file
/// </summary>
let get_atlas_entries (filename: string) =
    let info = new System.IO.FileInfo(filename)
    int (info.Length / 8L)

/// <summary>
/// Converts a string into a string key
/// </summary>
let find_string_key (odi: OnDiskIndex) (lowercase: bool) (term: string) =
    use index_binary = new System.IO.BinaryReader(odi.StringIndex, System.Text.Encoding.UTF8, true)
    use atlas_binary = new System.IO.BinaryReader(odi.StringAtlas, System.Text.Encoding.UTF8, true)

    let term =
        if lowercase then
            term.ToLower()
        else
            term

    let rec bsearch lower_bound upper_bound =
        let midpoint = (lower_bound + upper_bound) / 2

        odi.StringAtlas.Seek(int64 midpoint * 8L, System.IO.SeekOrigin.Begin)
        let offset = atlas_binary.ReadInt64()

        odi.StringIndex.Seek(offset, System.IO.SeekOrigin.Begin)
        let entry = index_binary.ReadString()

        if entry = term then
            midpoint
        elif lower_bound = upper_bound then
            -1
        elif lower_bound = upper_bound - 1 then
            // Since the sum of these two values will be odd, the midpoint will always be rounded
            // down. To examine the upper bound in this case we have to explicitly focus on it.
            bsearch upper_bound upper_bound
        elif System.String.CompareOrdinal(term, entry) < 0 then
            bsearch lower_bound midpoint
        else
            bsearch midpoint upper_bound

    bsearch 0 (odi.StringEntries - 1)

/// <summary>
/// Retrieves a term given its key
/// </summary>
let find_key_string (odi: OnDiskIndex) (key: int) =
    use atlas_binary = new System.IO.BinaryReader(odi.StringAtlas, System.Text.Encoding.UTF8, true)
    use index_binary = new System.IO.BinaryReader(odi.StringIndex, System.Text.Encoding.UTF8, true)

    odi.StringAtlas.Seek(int64 key * 8L, System.IO.SeekOrigin.Begin)
    |> ignore

    let offset = atlas_binary.ReadInt64()

    odi.StringIndex.Seek(offset, System.IO.SeekOrigin.Begin)
    |> ignore

    index_binary.ReadString()

/// <summary>
/// Gets the pages which fall under a specific term key
/// </summary>
let find_term_matches (odi: OnDiskIndex) (key: int) =
    use index_binary = new System.IO.BinaryReader(odi.TermIndex, System.Text.Encoding.UTF8, true)
    use atlas_binary = new System.IO.BinaryReader(odi.TermAtlas, System.Text.Encoding.UTF8, true)

    let rec bsearch lower_bound upper_bound =
        let midpoint = (lower_bound + upper_bound) / 2

        odi.TermAtlas.Seek(int64 midpoint * 8L, System.IO.SeekOrigin.Begin)
        let offset = atlas_binary.ReadInt64()

        odi.TermIndex.Seek(offset, System.IO.SeekOrigin.Begin)
        let entry = index_binary.ReadInt32()

        if entry = key then
            let result_count = index_binary.ReadInt32()
            [1..result_count]
            |> Seq.map (fun _ ->
                        index_binary.ReadInt32())
            |> Set.ofSeq

        elif lower_bound = upper_bound then
            Set.empty
        elif lower_bound = upper_bound - 1 then
            // Since the sum of these two values will be odd, the midpoint will always be rounded
            // down. To examine the upper bound in this case we have to explicitly focus on it.
            bsearch upper_bound upper_bound
        elif entry > key then
            bsearch lower_bound midpoint
        else
            bsearch midpoint upper_bound

    bsearch 0 (odi.TermEntries - 1)

/// <summary>
/// Queries the full index to find the pages which contain all the given terms
/// </summary>
let query_full_index (odi: OnDiskIndex) (terms: string) =
    let iter matches term_key =
        if Set.isEmpty matches then
            Set.empty
        else
            let new_matches = find_term_matches odi term_key
            Set.intersect matches new_matches

    let term_keys =
        terms
        |> get_search_terms
        |> Set.map (find_string_key odi true)

    if Set.contains -1 term_keys then
        Set.empty
    elif Set.isEmpty term_keys then
        Set.empty
    else
        let first_term = Set.minElement term_keys
        let rest_terms = Set.remove first_term term_keys

        rest_terms
        |> Set.fold iter (find_term_matches odi first_term)
        |> Set.map (find_key_string odi)

/// <summary>
/// Gets the pages which backlink the given string key
/// </summary>
let find_backlink_matches (odi: OnDiskIndex) (key: int) =
    use index_binary = new System.IO.BinaryReader(odi.BacklinkIndex, System.Text.Encoding.UTF8, true)
    use atlas_binary = new System.IO.BinaryReader(odi.BacklinkAtlas, System.Text.Encoding.UTF8, true)

    let rec bsearch lower_bound upper_bound =
        let midpoint = (lower_bound + upper_bound) / 2

        odi.BacklinkAtlas.Seek(int64 midpoint * 8L, System.IO.SeekOrigin.Begin)
        let offset = atlas_binary.ReadInt64()

        odi.BacklinkIndex.Seek(offset, System.IO.SeekOrigin.Begin)
        let entry = index_binary.ReadInt32()

        if entry = key then
            let result_count = index_binary.ReadInt32()
            [1..result_count]
            |> Seq.map (fun _ ->
                        index_binary.ReadInt32())
            |> Set.ofSeq

        elif lower_bound = upper_bound then
            Set.empty
        elif lower_bound = upper_bound - 1 then
            // Since the sum of these two values will be odd, the midpoint will always be rounded
            // down. To examine the upper bound in this case we have to explicitly focus on it.
            bsearch upper_bound upper_bound
        elif entry > key then
            bsearch lower_bound midpoint
        else
            bsearch midpoint upper_bound

    bsearch 0 (odi.BacklinkEntries - 1)

/// <summary>
/// Queries the backlink index to find the pages which link to the given page
/// </summary>
let query_backlink_index (odi: OnDiskIndex) (page: string) =
    let link_key = find_string_key odi false page
    if link_key = -1 then
        Set.empty
    else
        find_backlink_matches odi link_key
        |> Set.map (find_key_string odi)
