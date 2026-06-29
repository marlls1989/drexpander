# drexpander

**Pulsar's dual-rail expansor** — a small Haskell command-line tool in the
[Pulsar](https://github.com/marlls1989/pulsar) ecosystem for designing QDI
(quasi-delay-insensitive) asynchronous circuits using SDDS-NCL / NULL
Convention Logic.

It takes a **single-rail Verilog netlist** (one wire per logical net) and
rewrites it into a **dual-rail-ready netlist** that the Pulsar synthesis flows
can then map onto NCL cells. It is a *tool source*: the flows consume the
prebuilt binary committed into their `bin/` directory, not this repository.

## What it does

For every module in the input it:

1. **Reads & parses** the Verilog (via the `verilog` fork below).
2. **Introspects** the module — collects its inputs, outputs and wires.
3. **Bit-blasts** buses into per-bit nets (`a[3:0]` → `a_0`, `a_1`, …),
   resolving indexed references on assignments and instance port maps.
4. **Injects reset/clock** connections into `dff`, `tielo` and `tiehi`
   instances, and rewrites continuous `assign`s into explicit `buff` cells.
5. **Swaps in dual-rail adaptors** — an internal `drwire` per net plus a
   `drinput`/`droutput` IO adaptor per port, and a new module port list of
   `_t`/`_f`/`_ack` triples.
6. **Prints** the transformed modules to stdout.

## The `_t` / `_f` / `_ack` convention

Each single-rail net `n` becomes a dual-rail triple under the SDDS-NCL
encoding:

| Rail     | Meaning                                |
|----------|----------------------------------------|
| `n_t`    | "true" data rail                       |
| `n_f`    | "false" data rail                      |
| `n_ack`  | return-to-zero acknowledge             |

The IO adaptors bind these to their `.t` / `.f` / `.ack` / `.drw` ports. Note
the **acknowledge direction is asymmetric**: for a module **input** the data
rails are inputs and the ack is an output; for a module **output** it is the
mirror image. These adaptor names and ports are the contract with the flow's
SystemVerilog adaptor modules (`tech/*alho.sv`) and must stay in sync with them.

## Usage

```
drexpander [OPTIONS] FILES...
```

| Flag                | Default   | Meaning           |
|---------------------|-----------|-------------------|
| `FILES` (positional)| —         | input netlist(s)  |
| `--reset`, `-r`     | `reset`   | reset port name   |
| `--clock`, `-c`     | `clk`     | clock port name   |

The flows invoke it through Genus's `shell` command, e.g.:

```tcl
shell drexpander ${OUTDIR}/${DESIGN}.v > ${OUTDIR}/ncl_${DESIGN}.v
```

## Build

Requires [Stack](https://docs.haskellstack.org/). Resolver **lts-24.46**
(GHC 9.10). The package is defined with **hpack** — edit `package.yaml`, not the
generated `drexpander.cabal`.

```bash
stack build                                          # build
stack test                                           # Tasty + golden suite
stack install --local-bin-path /path/to/<flow>/bin   # drop the binary into a flow
```

`stack.yaml` pins two forks maintained alongside Pulsar as `extra-deps` (both
under github.com/marlls1989): **`verilog`** and **`gasp`**.

> The `bin/drexpander` committed into the flow repos is an x86-64 Linux ELF —
> build on the platform that will run Genus.

## Code map

- `app/expander.hs` — CLI entry point (`optparse-applicative` wrapper).
- `src/DRExpander.hs` — the whole library (types, introspection, bus
  bit-blasting, dual-rail adaptor emission, reset/clock fix-ups, and the
  `processModule` pipeline).
- `test/Spec.hs` — Tasty (HUnit + QuickCheck) plus a golden test against
  `test/golden/design.processed.v`.

## Known issues / caveats (not fixed)

- **Dead dependencies.** `package.yaml` declares `algebraic-graphs` and
  `regex-tdfa`, but neither is imported by the current code.
- **License metadata drift.** `package.yaml` says `license: BSD3`, but the
  `LICENSE` file is **MIT**.
- **Partial `error` in `fixInstancesBitBlast`.** The `assign` rewrite only
  handles a plain identifier or a single indexed bit on each side; any other
  construct triggers a runtime `error`. This is a deliberate restriction on the
  netlists the tool accepts, not a general Verilog rewriter.
- **Branch caveat.** `master` uses the established **per-wire** `drinput`/
  `droutput` adaptors. The incomplete `bus-io` branch reworks them into a
  bus-style form and does **not** compile end-to-end — do not assume the two
  branches agree on the adaptor interface.
