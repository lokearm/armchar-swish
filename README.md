# armchar-swish

ArM character generator implemented in declarative programming,
using Haskell.

The repository includes several distinct attempts.  The current
attempt uses data files in JSON and CSV.  The code for this is
essentially
+ The `ArM.Char` modules
+ The `ArmChar.hs` main script
This provides a CLI tool to produce character sheets and validation
reports from the character defined in JSON.

Previous attempts used data files in RDF and the Swish library.
One problem with this approach is the computational complexity
of logic inference on RDF graphs.  The code includes attempts
on both a client/server solution and a standalone CLI tool, but
both are currently incomplete and on hold.

Further documentation is found in the docs hierarchy.  

+ [CLI using JSON](docs/CLI%20using%20JSON.md)
+ [CLI with Swish](docs/CLI%20with%20Swish.md)
