# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`drexpander` is a small **Haskell** tool in the **Pulsar** ecosystem for QDI (quasi-delay-insensitive)
asynchronous circuits (SDDS-NCL / NULL Convention Logic). It builds a **single command-line
executable**, `drexpander` ("Pulsar's dual-rail expansor"), that **prepares a single-rail Verilog
netlist for SDDS-NCL dual-rail expansion** — bit-blasting buses and emitting the dual-rail IO adaptor
instances the flow then maps to NCL cells.

It is a **tool source**, not a flow: the Pulsar synthesis flows consume the **prebuilt binary**, not
this repo. The same source also lives as the `pulsar/haskell/drexpander` git submodule and is shared by
both the public `pulsar` and private `restricted-pulsar` flows.

## Build

Requires [Stack](https://docs.haskellstack.org/) (no system libraries — the old GLPK/constrainer
dependency was removed).

```bash
stack build                                          # resolver lts-22.28 -> GHC 9.6
stack install --local-bin-path /path/to/<flow>/bin   # drops the `drexpander` binary into a flow's bin/
```

`stack.yaml` pins two forks maintained alongside Pulsar as `extra-deps` (both under
github.com/marlls1989): **`verilog`** and **`gasp`**. The package is defined with **hpack**
(`package.yaml`); `drexpander.cabal` is generated and git-ignored — edit `package.yaml`, not the cabal.

The `bin/drexpander` committed into the flow repos is an **x86-64 Linux ELF** — build on the platform
that will run Genus.

## How the flows use it

Both flows invoke it through Genus's `shell` command in their RTL front-end:

```tcl
shell drexpander ${OUTDIR}/${DESIGN}.v > ${OUTDIR}/ncl_${DESIGN}.v
```
- `pulsar/scripts/syn_rtl.tcl:58`
- `restricted-pulsar/scripts/syn_rtl.tcl:62`

CLI (`app/expander.hs`, a thin `optparse-applicative` wrapper): positional `FILES`, `--reset`/`-r`
(default `reset`), `--clock`/`-c` (default `clk`). It prints the transformed modules to stdout.

## Code map

- `app/expander.hs` — CLI entry point; parses options into `PrgOptions`, calls `processVerilogFiles`.
- `src/DRExpander.hs` — the whole library. Key pieces:
  - `Wire = Wire String | Bus Integer Integer String`; `bitBlastWire` / `expandBusWireName`
    (`name` ++ `"_"` ++ idx) expand buses to per-bit wires.
  - `vlogDRWireInputInst` / `vlogDRWireOutputInst` / `vlogDRAdaptor` — emit the per-wire dual-rail IO
    adaptor instances (`drinput`/`droutput`) using the **`_t`/`_f`/`_ack`** dual-rail net naming.
  - `fixDffReset` / `fixTieResetClk` — rewrite reset/clock connections.
  - `processModule` / `processVerilogFiles` — top-level pipeline (read → transform → emit).

## Conventions

- **British spelling** in identifiers and output (consistent with the other Pulsar Rust/Haskell repos).
- The **`_t`/`_f`/`_ack`** dual-rail encoding is the contract with the flow's tech libraries — keep IO
  adaptor port names in sync with the flow's SystemVerilog adaptor modules (`tech/*alho.sv` in the
  flows).

## Branches

- `master` — stable. The IO adaptors are the established **per-wire** `drinput`/`droutput` form.
- `bus-io` — an **incomplete experiment** parking commits that rewrite the adaptors into a **bus-style**
  form (`drinput`/`droutput`/`busack`/`drwire`, `vlogDRInAckAdaptor`). It is the generator side of the
  matching `bus-io` branch in `restricted-pulsar` (`drbusin`/`drbusout`); both were moved off `master`
  because the feature does not yet compile end-to-end. Do not assume `master` and `bus-io` agree on the
  adaptor interface.

## Notes

- License: the `LICENSE` file is **MIT**; `package.yaml` still declares `license: BSD3` — update the
  metadata if you touch licensing.
- There is no meaningful `README.md` or test suite yet (`test/Spec.hs` is a stub).
