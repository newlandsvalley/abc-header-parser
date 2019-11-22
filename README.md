# AbcHeaderParser

EXPERIMENTAL - this is my first encounter with Stack and Cabal

This package is a simple parser for ABC notation headers (i.e. ABC metadata). The intention is eventually to use it in a Haskell re-write of the [musicrest](https://github.com/newlandsvalley/musicrest) server.  This will need to store separately the most important headers (title, key, rhythm etc.) in order to support searches.

As well as parsing the headers, a validation layer is included which not only ensures that essential headers are present but also enforces values within some headers (such as rhythm) which (in musicrest) must be constrained to sets of known values. It also tries its best to normalise other headers (e.g. key signature) which may have a lot of variety in the wild.
