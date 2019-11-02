[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | ["full-text"; dirname; index_dir] ->
        let ft = Search.build_full_index dirname
        let (odi: Search.OnDiskIndex) = {
            TermAtlas=System.IO.Path.Combine(index_dir, "term.atlas")
            TermIndex=System.IO.Path.Combine(index_dir, "term.idx")
            StringAtlas=System.IO.Path.Combine(index_dir, "str.atlas")
            StringIndex=System.IO.Path.Combine(index_dir, "str.idx")
            // Neither of these are used for the first stage
            BacklinkAtlas=""
            BacklinkIndex=""
        }
        let str_idx = Search.write_string_index ft odi
        Search.write_term_index ft str_idx odi
        0

    | ["links"; dirname; index_dir] ->
        let (odi: Search.OnDiskIndex) = {
            StringAtlas=System.IO.Path.Combine(index_dir, "str.atlas")
            StringIndex=System.IO.Path.Combine(index_dir, "str.idx")
            BacklinkAtlas=System.IO.Path.Combine(index_dir, "link.atlas")
            BacklinkIndex=System.IO.Path.Combine(index_dir, "link.idx")
            // Not used by the second stage
            TermAtlas=""
            TermIndex=""
        }

        if not <| System.IO.File.Exists(odi.StringAtlas) || not <| System.IO.File.Exists(odi.StringIndex) then
            printf "The full-text index must be generated before building the link index\n"
            1
        else
            let title_map  = Search.read_page_string_index odi
            let link_map = Search.build_full_link_map dirname
            Search.write_link_map link_map title_map odi
            0

    | _ ->
        eprintf "Commands: full-text WIKI-DIR INDEX-DIR | links WIKI-DIR INDEX-DIR\n"
        1
