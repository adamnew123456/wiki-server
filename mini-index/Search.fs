module Search

open System.Xml

(*
  The full-text index format we use is split into three parts:

    - The string index contains all of the text fragments used within the term
      index, in a sorted form which makes it easy to search. It let us avoid
      storing duplicate values within the term index (which would increase its
      size massively) and lets us keep track of strings by keys which are easier
      to sort than the strings themselves.

    - The term index is a big map which takes a string key representing a search
      term (such as "extreme" or "refactoring") and associates it with a set of
      string keys representing page names. This is what actually allows us to
      perform searches using the terms the user gives to us.

    - The backlink index is like the term index, but maps a page title to the set
      of pages which access it. This lets us make page reference searches (what
      you get by clicking on a page title) faster and more exact than you would
      get with a full-text search.

  In addition to these three index types , we also split each of the three indexes
  into an "idx" and an "atlas". The idx contains the actual data, either strings or
  map entries. The atlas contains the offset of the start of each entry in the idx.
  For example, the string atlas might contain the entries:

    (0, 8)

  While the string idx contains the actual contents (assuming strings were
  comma-separated):

    "extremere,factoring"

  Having the atlas makes it easier to implement lookups at the nth item. We can
  easily find out where string 42 starts by looking 42 * 8 bytes into the atlas,
  reading the offset, and then looking at that offset in the idx.

  We also sort both the atlas and idx data, which makes both of them easier to
  scan via binary search later on.
 *)

type OnDiskIndex = {
    TermAtlas: string
    TermIndex: string
    StringAtlas: string
    StringIndex: string
    BacklinkAtlas: string
    BacklinkIndex: string
}

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
/// Builds a full-text search index of a single page in the wiki
/// </summary>
let build_single_full_index (filename: string) =
    use reader = System.IO.File.OpenText(filename)

    // HTML reading states. We only care about the content inside of the HTML
    // tags, but we do need to keep track of them to avoid indexing attributes
    // or tag names.
    let READ_STATE_TEXT = 0
    let READ_STATE_IN_TAG = 1
    let READ_STATE_IN_SQUOTE = 2
    let READ_STATE_IN_DQUOTE = 3
    let READ_STATE_IN_ENTITY = 4

    let add_buffer_terms buffer terms =
        let chars =
            buffer
            |> List.rev
            |> Array.ofList

        let text = new System.String(chars)
        let new_terms =
            get_search_terms text
            |> Set.map (fun text -> System.String.Intern(text))

        Set.union new_terms terms

    let rec iter read_state buffer terms =
        let raw_ch = reader.Read()
        if raw_ch = -1 then
            add_buffer_terms buffer terms
        else
            let ch = char raw_ch

            // HTML tag state transitions
            if read_state = READ_STATE_TEXT && ch = '<' then
                let new_terms = add_buffer_terms buffer terms
                iter READ_STATE_IN_TAG [] new_terms
            elif read_state = READ_STATE_IN_TAG && ch = '"' then
                iter READ_STATE_IN_DQUOTE [] terms
            elif read_state = READ_STATE_IN_TAG && ch = '\'' then
                iter READ_STATE_IN_SQUOTE [] terms
            elif read_state = READ_STATE_IN_SQUOTE && ch = '\'' then
                iter READ_STATE_IN_TAG [] terms
            elif read_state = READ_STATE_IN_DQUOTE && ch = '"' then
                iter READ_STATE_IN_TAG [] terms
            elif read_state = READ_STATE_IN_TAG && ch = '>' then
                iter READ_STATE_TEXT [] terms

            // HTML entity state transitions. *Technially* we should index
            // these, but in practice letters and numbers won't be encoded this way
            elif read_state = READ_STATE_TEXT && ch = '&' then
                iter READ_STATE_IN_ENTITY [] terms
            elif read_state = READ_STATE_IN_ENTITY && ch = ';' then
                iter READ_STATE_TEXT [] terms

            elif read_state = READ_STATE_TEXT then
                iter READ_STATE_TEXT (ch :: buffer) terms
            else
                iter read_state buffer terms

    iter READ_STATE_TEXT [] Set.empty

/// <summary>
/// Builds a full-text search index across all pages in the wiki
/// </summary>
let build_full_index (dir_name: string) =
    let wiki_dir = new System.IO.DirectoryInfo(dir_name)

    let merge_into_index page terms index =
        terms
        |> Set.fold (fun index term ->
                     let index_pages =
                        match Map.tryFind term index with
                        | Some entry -> Set.add page entry
                        | None -> Set.singleton page

                     Map.add term index_pages index)
                    index

    wiki_dir.GetFiles()
    |> Seq.fold (fun index file ->
                 let terms = build_single_full_index file.FullName
                 let page = file.Name.Replace(".html", "")
                 printf "  Indexing %d terms for page %s...\n" (Set.count terms) page
                 merge_into_index page terms index)
                Map.empty

/// <summary>
/// Builds a string index and atlas from the full-text index
/// </summary>
let write_string_index (ft_index: Map<string, Set<string>>) (odi: OnDiskIndex) =
    use index_stream = System.IO.File.Open(odi.StringIndex, System.IO.FileMode.Create)
    use index_binary = new System.IO.BinaryWriter(index_stream)

    use atlas_stream = System.IO.File.Open(odi.StringAtlas, System.IO.FileMode.Create)
    use atlas_binary = new System.IO.BinaryWriter(atlas_stream)

    let write_text in_index (text: string) =
        if Map.containsKey text in_index then
            in_index
        else
            let offset = index_stream.Position
            atlas_binary.Write(offset)
            index_binary.Write(text)
            Map.add text (Map.count in_index) in_index

    ft_index
    |> Map.toSeq
    |> Seq.collect (fun (term, matches) ->
                    Set.toSeq (Set.add term matches))
    |> Seq.sortWith (fun a b ->
                     System.String.CompareOrdinal(a, b))
    |> Seq.fold write_text Map.empty

/// <summary>
/// Builds a matches index and atlas from the full-text index
/// </summary>
let write_term_index (ft_index: Map<string, Set<string>>)
                     (in_index: Map<string, int>)
                     (odi: OnDiskIndex) =
    use index_stream = System.IO.File.Open(odi.TermIndex, System.IO.FileMode.Create)
    use index_binary = new System.IO.BinaryWriter(index_stream, System.Text.Encoding.UTF8)

    use atlas_stream = System.IO.File.Open(odi.TermAtlas, System.IO.FileMode.Create)
    use atlas_binary = new System.IO.BinaryWriter(atlas_stream, System.Text.Encoding.UTF8)

    ft_index
    |> Map.toSeq
    |> Seq.sortWith (fun (a, _) (b, _) ->
                     System.String.CompareOrdinal(a, b))
    |> Seq.iter (fun (term, matches) ->
                 let offset = index_stream.Position
                 atlas_binary.Write(offset)
                 index_binary.Write(Map.find term in_index)
                 index_binary.Write(Set.count matches)

                 matches
                 |> Set.iter (fun mtch ->
                              index_binary.Write(Map.find mtch in_index)))

/// <summary>
/// Gets the number of entries within an atlas file
/// </summary>
let get_atlas_entries (filename: string) =
    let info = new System.IO.FileInfo(filename)
    int (info.Length / 8L)

/// <summary>
/// Reads all page titles from the string index
/// </summary>
let read_page_string_index (odi: OnDiskIndex) =
    use index_stream = System.IO.File.OpenRead(odi.StringIndex)
    use index_binary = new System.IO.BinaryReader(index_stream, System.Text.Encoding.UTF8)

    let count = get_atlas_entries odi.StringAtlas
    let page_regex = new System.Text.RegularExpressions.Regex("[A-Z][a-z]+([A-Z][a-z]+)+")

    let rec iter current index =
        if current = count then
            index
        else
            let name = index_binary.ReadString()
            if page_regex.IsMatch(name) then
                iter (current + 1) (Map.add name current index)
            else
                iter (current + 1) index

    iter 0 Map.empty

/// <summary>
/// Builds a set of all the pages which link the given page
/// </summary>
let build_single_link_map (filename: string) =
    use reader = System.IO.File.OpenText(filename)

    // All of the pages in the archive are parseable as XML once BeautifulSoup
    // processes them, so we can easily pick out specific elements and
    // attributes via the XML parser
    let reader = XmlReader.Create(reader)

    let rec iter in_h1 links =
        if reader.Read() then
            match reader.NodeType with
            | XmlNodeType.Element ->
                if reader.Name = "h1" then
                    iter true links
                elif in_h1 then
                    iter in_h1 links
                elif reader.Name <> "a" then
                    iter in_h1 links
                elif not reader.HasAttributes then
                    iter in_h1 links
                elif not <| reader.MoveToAttribute("href") then
                    iter in_h1 links
                else
                    let url = reader.Value
                    // All the page links generated by the Python stage are
                    // relative /wiki, and don't require any prefix
                    if url.Contains("/") then
                        iter in_h1 links
                    else
                        iter in_h1 (Set.add url links)

            | XmlNodeType.EndElement ->
                if reader.Name = "h1" then
                    iter false links
                else
                    iter in_h1 links

            | _ ->
                iter in_h1 links
        else
            links

    iter false Set.empty

/// <summary>
/// Builds a full link map for every page in the wiki
/// </summary>
let build_full_link_map (dir_name: string) =
    let wiki_dir = new System.IO.DirectoryInfo(dir_name)

    wiki_dir.GetFiles()
    |> Seq.fold (fun linkmap file ->
                 let links = build_single_link_map file.FullName
                 let page = file.Name.Replace(".html", "")
                 printf "  Indexing %d links on page %s...\n" (Set.count links) page

                 links
                 |> Set.fold (fun linkmap link ->
                              let old_pages =
                                  match Map.tryFind link linkmap with
                                  | Some pages -> pages
                                  | None -> Set.empty
                              Map.add link (Set.add page old_pages) linkmap)
                             linkmap)
                Map.empty

/// <summary>
/// Builds a link index and atlas from the backlink map
/// </summary>
let write_link_map (link_map: Map<string, Set<string>>)
                   (title_map: Map<string, int>)
                   (odi: OnDiskIndex) =
    use index_stream = System.IO.File.Open(odi.BacklinkIndex, System.IO.FileMode.Create)
    use index_binary = new System.IO.BinaryWriter(index_stream, System.Text.Encoding.UTF8)

    use atlas_stream = System.IO.File.Open(odi.BacklinkAtlas, System.IO.FileMode.Create)
    use atlas_binary = new System.IO.BinaryWriter(atlas_stream, System.Text.Encoding.UTF8)

    link_map
    |> Map.toSeq
    |> Seq.sortWith (fun (a, _) (b, _) ->
                     System.String.CompareOrdinal(a, b))
    |> Seq.iter (fun (link, pages) ->
                 let offset = index_stream.Position
                 if Map.containsKey link title_map then
                    atlas_binary.Write(offset)
                    index_binary.Write(Map.find link title_map)
                    index_binary.Write(Set.count pages)

                    pages
                    |> Set.iter (fun page ->
                                  index_binary.Write(Map.find page title_map))
                 else
                   ())
