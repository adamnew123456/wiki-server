(*
  Our configuration is taken from INI files with this structure:

  [network]
  port = 8080
  base-path = /...

  [paths]
  ;; Directory containing Wiki HTML files
  wiki = /.../.../...
  ;; Directory containing static assets
  static = /.../.../...
  ;; Directory containing the output of the 
  ft-index = /.../.../...
*)

type Configuration = {
    Port: int
    UrlBase: string
    WikiDirectory: string
    StaticDirectory: string
    FullIndexDirectory: string
}

/// <summary>
/// Consumes the given INI-format file and returns a mapping of sections and keys to
/// values.
/// </summary>
let read_ini_file (filename: string) =
    let is_empty_line line =
        line = ""
        || line.StartsWith("#")
        || line.StartsWith(";")

    System.IO.File.ReadAllLines(filename)
    |> Seq.fold (fun (sections, current_section) line ->
                 let line = line.Trim()
                 if is_empty_line line then
                     (sections, current_section)

                 elif line.StartsWith("[") && line.EndsWith("]") then
                     let name = line.Substring(1, line.Length - 2)
                     (sections, name)

                 elif line.Contains("=") then
                     let separator = line.IndexOf("=")
                     let key = line.Substring(0, separator).Trim()
                     let value = line.Substring(separator + 1).Trim()
                     (Map.add (current_section, key) value sections, current_section)

                 else
                     failwithf "Error in section %s: line '%s' not section or key\n"
                               current_section
                               line)

                (Map.empty, "DEFAULT")
    |> fst

/// <summary>
/// Builds a configuration from the given parsed INI file
/// </summary>
let build_config (ini: Map<string * string, string>) =
    if not <| Map.containsKey ("network", "port") ini then
        failwith "Configuration needs value for [network].port"
    elif not <| Map.containsKey ("network", "base-path") ini then
        failwith "Configuration needs value for [network].base-path"
    elif not <| Map.containsKey ("paths", "wiki") ini then
        failwith "Configuration needs value for [paths].wiki"
    elif not <| Map.containsKey ("paths", "static") ini then
        failwith "Configuration needs value for [paths].static"
    elif not <| Map.containsKey ("paths", "ft-index") ini then
        failwith "Configuration needs value for [paths].ft-index"
    else
        {Port=System.Int32.Parse(Map.find ("network", "port") ini)
         UrlBase=(Map.find ("network", "base-path") ini).TrimEnd('/')
         WikiDirectory=Map.find ("paths", "wiki") ini
         StaticDirectory=Map.find ("paths", "static") ini
         FullIndexDirectory=Map.find ("paths", "ft-index") ini}

(*
  We're going to be serving these endpoints:

  - "/wiki/:page:" is a particular wiki page

  - "/wiki" shows the wiki home page. / is also an alias for this.

  - "/wiki/?search=x" searches the titles of pages in the wiki for x

  - "/fullSearch?search=x" searches the full contents of the wiki for the given text.
      If a ?search=x is not provided then it looks at the referrer header

  - "/static/" serves static files which aren't wiki pages
*)

/// <summary>
/// Sends a static file through the given HttpListenerResponse
/// </summary>
let send_file (filename: string)
              (content_type: string)
              (response: System.Net.HttpListenerResponse) =
    if not <| System.IO.File.Exists(filename) then
        response.StatusCode <- 404
        response.StatusDescription <- "File Not Found"
        response.OutputStream.Close()
    else
        let info = new System.IO.FileInfo(filename)
        response.ContentLength64 <- info.Length
        response.ContentType <- content_type
        response.Headers.Add("Cache-Control", "max-age=2592000")

        let file_stream = System.IO.File.OpenRead(filename)
        file_stream.CopyTo(response.OutputStream)
        response.OutputStream.Close()

/// <summary>
/// Builds a results page containing the pages which match a search
/// </summary>
let send_search_results (pages: Set<string>)
                        (response: System.Net.HttpListenerResponse) =
    response.StatusCode <- 200
    response.StatusDescription <- "OK"

    let header = "<html><head><meta charset=\"utf-8\"><title>Search Results</title></head><body><h1>Search Results</h1>"
    let footer = "</body></html>"

    let html =
        if Set.isEmpty pages then
            sprintf "%s No pages matched the search %s" header footer
        else
            let matches =
                pages
                |> Seq.map (fun page -> sprintf "<li> <a href=\"wiki/%s\"> %s </a> </li>"
                                                page
                                                page)
                |> String.concat ""

            sprintf "%s <ul> %s </ul> %s" header matches footer

    let encoded = System.Text.Encoding.UTF8.GetBytes(html)
    response.ContentLength64 <- int64 (Array.length encoded)
    response.ContentType <- "text/html"
    response.OutputStream.Write(encoded, 0, Array.length encoded)
    response.OutputStream.Close()

/// <summary>
/// Parses a query string from a Uri into a map
/// </summary>
let parse_qs (qs: string) =
    let decode s =
        System.Web.HttpUtility.UrlDecode(s)

    if qs = "" then
        Map.empty
    else
        let mapping = qs.Substring(1)

        mapping.Split('&')
        |> Seq.map (fun assignment ->
                    let eql_index = assignment.IndexOf('=')
                    if eql_index = -1 then
                        (decode assignment, "")
                    else
                        let key = assignment.Substring(0, eql_index)
                        let value = assignment.Substring(eql_index + 1)
                        (decode key, decode value))
        |> Map.ofSeq

/// <summary>
/// Gets a search query from the current context, either a URL query or referer
/// </summary>
let get_search_string (request: System.Net.HttpListenerRequest) =
    let parameters = parse_qs request.Url.Query
    match Map.tryFind "search" parameters with
    | Some query -> Some query
    | None ->
        try
            let referer_raw = request.Headers.Get("referer")
            if referer_raw = null then
                None
            else
                let referer = new System.Uri(referer_raw)
                let path_parts =
                    referer.AbsolutePath.Split('/')
                    |> List.ofArray
                    |> List.rev

                match path_parts with
                | page :: "wiki" :: _ -> Some page
                | _ -> None
        with
        | :? System.UriFormatException ->
            None

/// <summary>
/// Routes a request to the appropriate handler
/// </summary>
let route (config: Configuration)
          (titles: Set<string>)
          (odi: Search.OnDiskIndex)
          (request: System.Net.HttpListenerRequest)
          (response: System.Net.HttpListenerResponse) =
    if request.Url.AbsolutePath.StartsWith(config.UrlBase) then
        let path = request.Url.AbsolutePath.Substring(config.UrlBase.Length)
        let search = get_search_string request

        if path.Contains("..") then
            // Avoid leaking data from relative paths pointing outside of our
            // roots
            response.StatusCode <- 404
            response.StatusDescription <- "Not Found"
            response.OutputStream.Close()

        elif path = "/" or path = "" then
            let static_path = sprintf "%s/%s" config.StaticDirectory "wiki.html"
            send_file static_path "text/html" response

        elif path = "/wiki" then
            match search with
            | None ->
                let static_path = sprintf "%s/%s" config.StaticDirectory "wiki.html"
                send_file static_path "text/html" response

            | Some query ->
                let results = Search.query_titles_index titles query
                send_search_results results response

        elif path.StartsWith("/wiki/") then
            let page = path.Substring(6)
            let page_path = sprintf "%s/%s.html" config.WikiDirectory page
            send_file page_path "text/html" response

        elif path = "/fullSearch" then
            match search with
            | None ->
                let static_path = sprintf "%s/%s" config.StaticDirectory "wiki.html"
                send_file static_path "text/html" response

            | Some query ->
                let results = Search.query_full_index odi query
                send_search_results results response

        elif path = "/static" then
            // Nothing will point to the root of /static
            response.StatusCode <- 404
            response.StatusDescription <- "Not Found"
            response.OutputStream.Close()

        elif path.StartsWith("/static") then
            let target = path.Substring(8)
            let mime_type =
                if target.EndsWith(".html") then
                    "text/html"
                elif target.EndsWith(".gif") then
                    "image/gif"
                else
                    "application/octet-stream"

            let static_path = sprintf "%s/%s" config.StaticDirectory target
            send_file static_path mime_type response
        else
            response.StatusCode <- 404
            response.StatusDescription <- "Not Found"
            response.OutputStream.Close()
    else
        response.StatusCode <- 404
        response.StatusDescription <- "Not Found"
        response.OutputStream.Close()

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    | [config_file] ->
        let raw_config = read_ini_file config_file
        let config = build_config raw_config

        let title_index = Search.build_titles_index config.WikiDirectory

        let term_atlas_path = System.IO.Path.Combine(config.FullIndexDirectory, "term.atlas")
        let term_index_path = System.IO.Path.Combine(config.FullIndexDirectory, "term.idx")
        let str_atlas_path = System.IO.Path.Combine(config.FullIndexDirectory, "str.atlas")
        let str_index_path = System.IO.Path.Combine(config.FullIndexDirectory, "str.idx")

        let (odi: Search.OnDiskIndex) = {
            TermEntries=Search.get_atlas_entries term_atlas_path
            TermAtlas=System.IO.File.OpenRead(term_atlas_path)
            TermIndex=System.IO.File.OpenRead(term_index_path)
            StringEntries=Search.get_atlas_entries str_atlas_path
            StringAtlas=System.IO.File.OpenRead(str_atlas_path)
            StringIndex=System.IO.File.OpenRead(str_index_path)
        }

        let listener = new System.Net.HttpListener()
        listener.Prefixes.Add(sprintf "http://+:%d/" config.Port)
        listener.Start()

        while true do
            let context = listener.GetContext()
            route config title_index odi context.Request context.Response

        0

    | _ ->
        eprintf "A configuration file is required"
        1
