# This repository must always use literate programming

Lilac is written in Lilac. The Org files (`*.org` and `design/*.org`) are the
source of truth. Source files such as `src/**/*.hs`, `lilac.cabal`, `Makefile`,
`nix/*.nix`, and this `CLAUDE.md` are tangled output, and `make tangle`
overwrites them. This applies to every change, however small, and even if the
request names a tangled file (for example "fix this bug in `src/Foo.hs`").

## Rules

- Never edit a tangled file directly, and never create a new source file by
  hand. Change the Org block that produces it. To create a new file, add a root
  block with a `:tangle` header.
- Before changing any file, find out where its contents come from: search the
  Org files for its `:tangle` path and for the code itself, because most code
  reaches its file through Noweb references. Only the paths excluded by
  `nonTangledPathspecs` in `Lilac.toml` are not tangled.
- The prose is part of the program. Explain why the code is shaped the way it
  is, and update nearby prose so that it remains true.
- Search the Org files, not tangled output. The `.rgignore` file already hides
  most tangled files from `rg`.
- Commit the Org change together with its regenerated tangled files.
- If tangling does not work (for example `lilac` is not on `PATH`; the Nix dev
  shell provides it), fix that or report it. Do not hand-edit tangled files to
  compensate.
- `lilac` builds itself from the tangled sources, so it cannot run if they fail
  to compile. In that case, fix the Org file and run
  `make -B tangle LILAC=lilac-nix`, which uses the Lilac binary built by Nix.
  Then run `make -B tangle` to go back to the usual binary. Don't restore old
  tangled files to get `lilac` working again.

## Checking a change

1. Run `make tangle` to regenerate the tangled files.
2. Run `make test` and `make lint`. `lilac lint` reports tracked files that no
   Org block produces.
3. Check `git status` to see that only the expected tangled files changed.
