[![Build Status](https://travis-ci.org/0Petya/debruijn-assembler.svg?branch=master)](https://travis-ci.org/0Petya/debruijn-assembler)

# De Bruijn Graph Sequence Assembler

An online De Bruijn graph sequence assembler that will generate a visual and interactive graph. This tool is intended for educational use; it's probably too slow to use in production.

You can input raw seqencing reads directly into it, or upload a [FASTQ](https://en.wikipedia.org/wiki/De_Bruijn_graph) or [FASTA](https://en.wikipedia.org/wiki/FASTA_format) file. It will then generate a De Bruijn graph using [d3-graphviz](https://github.com/magjac/d3-graphviz) and will find all possible [Eulerian paths](https://en.wikipedia.org/wiki/Eulerian_path) (the _solutions_) from the only possible node, or a random node (that will work) if exists an Eulerian cycle. The core is written in [Elm](https://elm-lang.org), a pure functional language that compiles to JavaScript.

There are five major components to the tool: generating the [kmers](https://en.wikipedia.org/wiki/K-mer), generating the graph, compressing the graph, finding all Eulerian paths from a starting kmer, and resolving repeats.

* Generating kmers uses a sliding window and has a time complexity of `O(n^2)` where `n` is the number of reads and `k` is the kmer size desired.

* Generating the graph involves identifying `k-1` overlaps for each kmer and compiling to [dot](https://www.graphviz.org/doc/info/lang.html) for [Graphviz](https://www.graphviz.org) and has a time complexity of `O(n^2)` where `n` is the number of kmers. I am unsure the time complexity for Graphviz to generate the given graph.

* Compressing the graph involves finding components in the graph that can be compressed and has a time complexity of `O(n+e)` where `n` is the number of kmers and `e` is the number of overlaps.

* Finding Eulerian paths uses [depth-first search](https://en.wikipedia.org/wiki/Depth-first_search) and I am uncertain about the time complexity as each kmer potentially can have branching paths with cycles that each trigger their own DFS.

* Resolving the repeats involves cutting out repeats found in the graph then uses a very similar algorithm to what's used in finding all the Eulerian paths to find each individual sequence. Finding repeats and cutting them out has a time complexity of `O(n)` where `n` is the number of kmers. Finding those individual paths in the cut graph has a time complexity of `O(n+e)` where `N` is the number of kmers and `e` is the number of overlaps. It's much quicker than finding all the Eulerian paths due to having the repeats cut out removes the possibility for any branching paths.

[More about De Bruijn graphs and their applications in genome assembly](https://en.wikipedia.org/wiki/De_Bruijn_graph).

Check out the tool [here](https://0petya.github.io/debruijn-assembler/)!

# Setup

To setup locally for development, you'll need a few dependencies:
* [Make](https://www.gnu.org/software/make/)
* [Elm](https://elm-lang.org)
* [Node.js](https://nodejs.org/en/)
* [Browserify](http://browserify.org/)

To build the project, you just need to execute `make`:
```
make
```

To build for production use:
```
make env=prod
```
