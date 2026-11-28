# Design rules

Generato is arranged so that AI coding agents and humans can work in it safely and efficiently. Agents copy existing patterns and take the shortest path to a working change, so the code makes that path the correct one.

- Data: a design is its records and who writes and reads them. A fact about a variable is stated once: rank, symmetry and the variable it differentiates where the tensor is declared, storage where the script binds it. A component is one record, built when its variable is bound. Later stages read its fields, and nothing recovers a fact by taking a name apart. The one name that carries meaning is the index letter, where `i` onward is spatial.
- Architecture: the layout says where new code goes. Reusable physics, tensors and equations, goes in `module/`, which names no storage, backend or output. A target-code idiom goes in `codes/`. What two backends share goes in `src/`, except what a backend shares with one that extends it, which stays in its table in `codes/`.
- Boundaries: a backend is one registered value, holding its formatters and the modes it accepts, and the script selects it explicitly. `src/` names no backend and nothing that only one backend understands.
- State: user configuration, the mode of a step and the component registry are three separate things: the context, an argument of a node, and the variable records. Each key is declared in one place and has one writer.
- Failure: a generator that cannot produce correct code produces none. Output is built in memory by one writer and written once, on success. An error has one path: a message with text, a non-zero exit, no file.
- Golden: every backend and every public feature has an active golden test, and an output change is a diff of a golden file. What no test exercises may be deleted.
