# What Is This?

This is the code behind [my mirror](https://kidneybone.com/c2/) of 
[Ward's Wiki](http://wiki.c2.com). It serves both the static content as well 
as providing a basic title search and a full-text search.

# How Can I Run It?

First, you'll need some basic dependencies to run the code:

- Python 3
- .NET Core CLI tools

You'll also need a copy of the [C2 archive](https://archive.org/details/c2.com-wiki_201501) 
from the Internet Archive. All that this tool requires is the static `wiki?*.html` files
in a directory somewhere; the rest of the metadata isn't required.

## Cleaning the Imported Files

The wiki dump contains links which we need to redirect, as well as scripts and
other things which aren't necessary. You can use the cleaner to convert the
dump files into ones which contain the links we want:

```
$ cd cleaner
$ python3 -m venv env
$ . env/bin/activate
$ pip3 install -r requirements.txt
$ mkdir new-wiki-dir/
$ python3 clean-wiki.py original-wiki-dir/ new-wiki-dir/
```

Note that you may need to adjust the encodings on one or two pages by hand.
Most of the wiki is either valid UTF-8 or valid CP1252, but there are some
pages which are mixes of the two that Python can't cope with.

## Generating The Full-Text Index

With the wiki files cleaned, you'll need to generate the index files which
wiki-server uses to provide full-text search (it does this internally; you
can read the Search.fs code to see how).

```
$ cd mini-index
$ dotnet run  -- full-text new-wiki-dir/ index-dir/
```

This can potentially take several minutes to run, as it has to process around
275 MB of HTML and produce a pair of sorted indexes. You only need to run this
process once however, since the content of the index doesn't change.

## Generating The Backlink Index

In addition to a full-text index, wiki-server also makes use of a backlink
index which is used to efficiently implement searches for page names, like
what you get if you click on a page title. Generating this index requires
an additional step after generating the full-text index:

```
$ cd mini-index
$ dotnet run -- links new-wiki-dir/ index-dir/
```

This is typically much quicker, since the backlink index is much smaller
and doesn't load the entire term index. 

For reference, these are the sizes of the files that you should expect
after both indexing passes are complete:

| Index      | Size |
| -----      | ---- |
| link.atlas | 288K |
| link.idx   | 1.9M |
| str.atlas  | 1.5M |
| str.idx    | 2.0M |
| term.atlas | 1.2M |
| term.idx   | 33M  |

## Writing A Basic Server Configuration

Before you can run the server with the index and converted dump, you'll need to
write a small configuration file to tell the server where to find the content
it needs.

```
[network]
; The port to bind the server to. The hostname is always localhost.
port = 9999

; Base URL path that the server is hosted at. Usually / unless you run it 
; behind a reverse proxy.
base-path = /

[paths]
; The full path to the directory containing the converted wiki files.
wiki = new-wiki-dir/

; The full path to the directory containing the static assets.
static = static/

; The full path to the directory containing the indexer's .idx and .atlas files
ft-index = index-dir/
```

## Run The Server

Once the configuration is written, you can provide its path to the server. This
will make the wiki mirror available on your local machine at whatever port you
chose in the configuration.

```
$ cd wiki-server
$ dotnet run -- config.ini
```
