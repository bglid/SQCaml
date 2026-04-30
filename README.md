<div align="center">

# SQCaml

[![OCaml](https://img.shields.io/badge/OCaml-EC6813?logo=ocaml&logoColor=fff)](#)
[![Build](https://github.com/bglid/SQCaml/actions/workflows/build.yml/badge.svg)](https://github.com/bglid/SQCaml/actions/workflows/build.yml)


◥ *Minimal OCaml B+ tree storage engine with a SQL-like REPL* ◤
- - -
</div>

## Project Background

The impetus for this project was to hack on something in OCaml. I had just spent the last few months doing a bunch of functional DSA in OCaml, but was looking to do a larger project. After getting some advice to look into the underlying structure and implementations of database algorithms, I decided to try implementing a minimally-workable B+ tree in OCaml. 

The idea for creating the top-level REPL with `ocamllex` and `menhir` came from stumbling upon a video going through a small language implementation. 

> [!NOTE]
> A lot of credit for getting to write to disk, as noted in the [acknowledgements](#acknowledgements), comes from the blogs written by [Artjom Plaunov](https://artjomplaunov.github.io/database/b+/tree/2025/01/13/btrees1.html). 
>I encourage you to check out their blogs and work.
- - - 
## Todos:

High-level overview of todos/roadmap

|  #  | Step                                                    | Status |
| :-: | ------------------------------------------------------- | :----: |
|  1  | Basic REPL and interpreter setup                        |   ✔    |
|  2  | Disk-writing API                                        |   ✔    |
|  3  | B+ Tree algorithms                                      |*in progress*|
|  4  | Basic Top-level SQL Parsing                             |   ✔    |
|  5  | Stretch goals... TBD                                    |   ❌   |

- - - 
## How-to run:

This project is built and executed with `Dune`. After cloning the repo, from the project root, run:

```bash
dune build
```

To start the REPL, run
```bash
dune exec SQCaml 
```

You can then run SQL-style commands like:
```sql
INSERT INTO Mbta (id, stop_name, rail_line)
VALUES (100, 'Englewood ave.', 'G');
```

> [!IMPORTANT]
> The current implementaiton only has a fixed single-table schema of:
> ```SQL
> (id:int, stop_name:varchar, rail_line:varchar)
>```
> For a list of accepted commands, see [docs](./docs/commands.md)

- - -
## Acknowledgements
###### 1. A lot of the lower-level disk-writing API was reimplemented from the work by [Artjom Plaunov](https://artjomplaunov.github.io/database/b+/tree/2025/01/13/btrees1.html) - check out their work.
###### 2. Ideas were also taken from the well-known [Let's Build a Simple Database](https://cstack.github.io/db_tutorial/). 
- - -

🐫︎
