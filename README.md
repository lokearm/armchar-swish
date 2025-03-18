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


Further documentation on the
[CLI using JSON](docs/CLI%20using%20JSON.md)
is found in the docs hierarchy.  
Sample character files are found in the `Data` subdirectory.

Previous attempts used data files in RDF and the Swish library.
One problem with this approach is the computational complexity
of logic inference on RDF graphs.  The code includes attempts
on both a client/server solution and a standalone CLI tool, but
both are currently incomplete and on hold.
This has been removed from the main branch, but exist on the swish
branch.  See also [CLI with Swish](docs/CLI%20with%20Swish.md).

## Usage

The following command builds a single character, in this case Eogan
mac Eogan.
```
armchar -c Data/eogan.json -o eogan.md  -t "Summer 1255" -T summer1255eogan.md
```

To build several characters defined in a saga file, the following
command does the trick:
```
mkdir -p GameStart Current LongSheet
armchar -s Data/hibernia.json -g GameStart -D Current -t "Summer 1255" -d LongSheet
```
Not the three directory options, 
+ `GameStart` for characters at their entry into the game
+ `Current` for current character sheets in compact format, in this
   case advanced until Summer 1255 inclusive.
+ `LongSheet` for more verbose characters.

The test files are included in the distribution.
