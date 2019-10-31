[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | ["build"; dirname; index_dir] ->
        let ft = Search.build_full_index dirname
        let (odi: Search.OnDiskIndex) = {
            TermAtlas=System.IO.Path.Combine(index_dir, "term.atlas")
            TermIndex=System.IO.Path.Combine(index_dir, "term.idx")
            StringAtlas=System.IO.Path.Combine(index_dir, "str.atlas")
            StringIndex=System.IO.Path.Combine(index_dir, "str.idx")
        }
        let str_idx = Search.write_string_index ft odi
        Search.write_term_index ft str_idx odi
        0

    | _ ->
        eprintf "Commands: build DIRECTORY STR_BASE INDEX_BASE\n"
        1
