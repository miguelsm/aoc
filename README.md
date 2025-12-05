# Advent of Code

Solutions to [Advent of Code](https://adventofcode.com/) in Clojure.

## Layout

- Inputs: `resources/inputs/<year>/dayNN.txt`
- Clojure: `src/aoc/y<year>/dNN.clj` (namespace `aoc.y<year>.dNN`)
- Fennel and other languages: `alt/<lang>/<year>/dayNN.*`

Babashka tasks:

- `bb run-clj <year> <dayNN>` – run the Clojure solution.
- `bb run-fennel <year> <dayNN>` – run the Fennel solution (requires `fennel` on PATH).

## Tests

- `clojure -M:test` – run all Clojure tests (uses `cognitect.test-runner`).
