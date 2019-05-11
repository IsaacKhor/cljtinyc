# Compiler for a subset of C99

This is written for my CS230 Compilers class. 

## To Build:

1. Clone this repo: `git clone https://github.com/IsaacKhor/cljtinyc.git`
1. Install leiningen via homebrew: `brew install leiningen`
2. `lein jar` to build jar, `lein uberjar` to include all deps

## Usage

    $ java -jar cljtinyc-0.1.0-standalone.jar [args]

## Options

```
Usage: cljtinyc [-123ah] source_path
Options:
  -a, --show-ast  Show the generated AST
  -1, --show-ir1  Show the first IR pass
  -2, --show-ir2  Show the second IR pass (linear IR)
  -3, --show-ir3  Show the third IR pass (MIPS asm)
  -m, --show-asm  Show the output MIPS asm
  -h, --help      Show this message
```

## Examples

`lein run resources/a5.c` to test the program that tests for primes. Outputs
the asm to `out.a`. 

### Bugs

The algorithm generating code for when there are insufficient registers fails
for reasons currently unknown, so make sure that any expressions that you
are trying to generate code for don't use more than 16 registers.

## License

Copyright © 2019 Isaac Khor.

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
