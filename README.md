# wembley
Command-line utility to pretty-print a whole codebase into a document form. 
This output is usable for code reviews or archival purposes. Two formats are
supported as of now: GitHub Flavoured Markdown and LaTeX (package `minted` used
for source code rendering).

## Usage
```
$ wembley --help
wembley - pretty-print a whole codebase into a document

Usage: wembley [-e|--extensions EXTS] (-n|--name NAME) [-f|--format FORMAT]
               (-o|--output PATH) [-d|--root-dir PATH]

Available options:
  -h,--help                Show this help text
  -e,--extensions EXTS     Comma-separated list of relevant file
                           extensions (default: "hs")
  -n,--name NAME           Name of the codebase. Appears in footers and as a
                           title
  -f,--format FORMAT       Format of the resulting document, supported: latex,
                           markdown (default: latex)
  -o,--output PATH         Location of the resulting document
  -d,--root-dir PATH       Root source directory path (default: ".")
```

## Install
There are two methods of obtaining the utility:
 * running `cabal install wembley`
 * cloning this repo and running `stack build --pedantic && stack exec wembley`

## Example
Running the following command in your shell:
```
$ git clone https://github.com/lovasko/m_list.git
$ wembley --extensions h,c \
          --name m_list \
          --format markdown \
          --output m_list.md \
          --root-dir ./m_list/src
```
will yield this
[result](https://gist.github.com/lovasko/9900732a993ccaa335b81b4dc3b96135).
Output in the format `latex`, once rendered, looks like
[this](example/m_list.pdf).

## License
The `wembley` utility is licensed under the terms of the
[3-clause BSD license](LICENSE).

## Author
Daniel Lovasko <daniel.lovasko@gmail.com>
