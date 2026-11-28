(* ::Package:: *)

(* Generato`Program`, a program is a list of nodes, and Generate turns it into a file: check, lower, render, emit *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Program`", {"Generato`Failure`", "Generato`Context`", "Generato`Variable`", "Generato`Component`", "Generato`Equation`", "Generato`Backend`", "Generato`Emit`"}];

If[!GetQuietMode[],
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Program`, {2026, 11, 28}"];
  System`Print["------------------------------------------------------------"];
];

Load::usage = "Load[varlist, mode] is a node of a program: it brings the components of the variables of varlist into scope. mode is a string, a key the backend that Generate names has under \"Load\". Load[component, mode] is the node of one component, written in xAct's notation, say gam[{1, -cart}, {2, -cart}]: an independent component of a bound variable. Load[varlist, mode, ChartName -> chart] names the chart of the components. A node is inert: building one generates nothing and changes no state.";

Store::usage = "Store[varlist, mode] is a node of a program: it writes the equations of the components of the variables of varlist. mode is a string, a key the backend that Generate names has under \"Store\". Store[component, mode] is the node of one component, written in xAct's notation, say gam[{1, -cart}, {2, -cart}]: an independent component of a bound variable. Store[varlist, mode, SuffixName -> suffix, ChartName -> chart, ExtraReplaceRules -> rules] takes the suffix name the equations were set with, the chart of the components, and rules applied to both sides of each equation. A node is inert: building one generates nothing and changes no state.";

ChartName::usage = "ChartName -> chart, an option of Load and Store, names the chart whose components the node is about. The default is the chart the variables were bound in.";

ExtraReplaceRules::usage = "ExtraReplaceRules -> rules, an option of Store, is a list of rules applied to the right-hand side of each equation and to what it assigns to.";

Generate::usage = "Generate[backend, file, program] writes file: the program, rendered by the backend named. program is a list whose elements are lines of text, Load and Store nodes, and lists of these, nested to any depth; it is flattened and read top to bottom. Every node is checked before anything is computed: its shape, that each variable is bound, that a component given alone is an independent one, that the backend has a formatter for the kind of the node, its mode and the storage of each variable, that each variable declares the relation the formatter reads, and that a Store has an equation. Every problem is reported with the position of its node in the flattened program, and then the run stops and nothing is written.";

LoadStatement::usage = "LoadStatement[config, mode, variable, component] returns the Load statement of one component: the record a backend formatter is handed. variable and component are the records binding wrote, and config is the user configuration.";

StoreStatement::usage = "StoreStatement[config, mode, suffix, coordinate, variable, component, extrareplacerules] returns the Store statement of one component, with the right-hand side of the equation set under the suffix name suffix, evaluated in the chart coordinate.";

SetMainPrint::usage = "SetMainPrint was removed, and a call of it stops the run. Pass the program as a list to Generate.";

SetOutputFile::usage = "SetOutputFile was removed, and a call of it stops the run. Name the output file in Generate[backend, file, program].";

PrintInitializations::usage = "PrintInitializations was removed, and a call of it stops the run. Write a Load[varlist, mode] node in the program handed to Generate.";

PrintEquations::usage = "PrintEquations was removed, and a call of it stops the run. Write a Store[varlist, mode] node in the program handed to Generate.";

pr::usage = "pr was removed, and a call of it stops the run. A line of output is a string in the program handed to Generate.";

Begin["`Private`"];

(*
  A program is a value. The script builds a list and hands it to Generate:

    Generate[backend, "file.hxx", {
      "",
      Load[EvolVarlist, "In"],
      "",
      Store[TempVarlist, "Temp"]
    }]

  A string is a line of output. A node is Load or Store with a varlist, a
  mode and options; it has no definition, so building one evaluates to
  itself. What a node means is decided by the backend Generate names: the
  mode is a key of its table, and the storage a formatter is looked up under
  is the one the variable was bound with.

  A node takes one component where it takes a varlist, written in xAct's
  notation:

    Store[dg[{3, -cart}, {0, -cart}, {1, -cart}], "Out"]

  That is how a script writes components in an order of its own, say from a
  Table, or some of them inside a branch of the C code. The component
  resolves through its head to a bound variable, and is the "Comp" of one
  of its records.

  The options of a node are named here once. They are read by name, so a
  script may write them as symbols of any context, or as strings.
*)
Options[Load] = {ChartName -> Automatic};

Options[Store] = {ChartName -> Automatic, SuffixName -> "", ExtraReplaceRules -> {}};

Protect[Load, Store];

(* The kind of statement a node lowers to, by its head *)
$Kinds = <|Load -> "Load", Store -> "Store"|>;

OptionName[name_Symbol] := SymbolName[name];

OptionName[name_String] := name;

OptionName[_] := None;

(* The options of a node are ones its head takes, and those of a Store have the shape lowering reads *)
NodeOptionsQ[head_Symbol, options_List] :=
  SubsetQ[OptionName /@ Keys[Options[head]], OptionName /@ Keys[options]] &&
  (
    head =!= Store ||
    (
      StringQ[OptionValue[Store, options, SuffixName]] &&
      MatchQ[OptionValue[Store, options, ExtraReplaceRules], {(_Rule | _RuleDelayed)...}]
    )
  );

(* The chart of a node: the one it names, or the one the variables were bound in *)
NodeChart[head_Symbol, options_List] :=
  Replace[OptionValue[head, options, ChartName], Automatic :> GetDefaultChart[]];

(* An expression as text for a message; it may be a whole varlist, so only its start is kept *)
ShortText[expr_] :=
  StringTake[ToString[expr, InputForm], UpTo[200]];

QuotedList[names_List] :=
  StringRiffle["\"" <> ToString[#] <> "\""& /@ names, ", "];

SuffixText[""] := "";

SuffixText[suffix_String] := " with SuffixName \"" <> suffix <> "\"";

(*
  A statement is what crosses to a backend, one per component: Backend.wl
  says what its fields are. These two functions are its one writer, and
  lowering a node is what calls them.

  A statement is self-contained: it carries the records and a copy of the
  user configuration, and a Store its evaluated right-hand side, so the
  formatter that gets it reads nothing else. The replace rules are applied
  to the right-hand side and to what the equation assigns to, the "Expr" of
  the statement's copy of the component record; the "Name" a declaration
  prints is not rewritten.
*)
LoadStatement[config_Association, mode_String, variable_Association, component_Association] :=
  <|
    "Kind" -> "Load",
    "Mode" -> mode,
    "Variable" -> variable,
    "Component" -> component,
    "Base" -> BaseRecord[variable],
    "Config" -> config
  |>;

StoreStatement[config_Association, mode_String, suffix_String, coordinate_, variable_Association, component_Association, extrareplacerules_] :=
  <|
    "Kind" -> "Store",
    "Mode" -> mode,
    "Variable" -> variable,
    "Component" -> MapAt[ReplaceAll[extrareplacerules], component, "Expr"],
    "Base" -> BaseRecord[variable],
    "RHS" -> ComputeRHSValue[coordinate, component["Comp"], suffix, extrareplacerules],
    "Config" -> config
  |>;

Protect[LoadStatement, StoreStatement];

(*
  The check: what is wrong with a node, found without computing anything.
  A problem is {tag, position, arguments...}: the message Generate::tag and
  what it is given. position is the place of the node in the flattened
  program, counted from 1. A node may have several problems, and one per
  variable of its varlist.

  What a node is about is a varlist or one component. Either resolves to
  bound variables, and from there the two are checked alike: the storage,
  the relation and the equation are facts of the variable.
*)

(* The head a var spec names, or $Failed if it is not a var spec *)
VarHead[var_] :=
  Quiet[Catch[ParseVar[var]["Head"], GeneratoFailure]];

(*
  Whether an equation is set for a head under a suffix name. SetEQN and
  SetEQNDelayed leave a rule on the symbol RHSOf names for the two
  (SetEQNDetail in Equation.wl), so the question is asked of that symbol. It is
  asked per variable: a component that no rule matches is found when the
  equation is evaluated.
*)
EquationSetQ[head_Symbol, suffix_String] :=
  DownValues[Evaluate[RHSOf[head, suffix]]] =!= {};

(* A relation names a variable whose record every statement carries as "Base", so it must be bound too *)
BaseProblems[variable_Association, position_Integer] :=
  Map[
    Function[relation,
      With[{basehead = Lookup[variable, relation, None]},
        If[basehead =!= None && !BoundQ[DeclaredVariable[basehead]],
          {"EBase", position, variable["Head"], relation, basehead}
          ,
          Nothing
        ]
      ]
    ]
    ,
    {"DerivativeOf", "TimeDerivativeOf"}
  ];

(* The backend has a formatter for the storage the variable is bound with, and the variable declares what a marked one reads *)
FormatterProblems[table_Association, kind_String, mode_String, variable_Association, position_Integer] :=
  Replace[
    FindFormatter[table, kind, mode, variable["Storage"]]
    ,
    {
      Missing["NoFormatter", storages_List] :>
        {
          {
            "EStorage", position, table["Name"], kind, mode, variable["Head"], variable["Storage"],
            If[storages === {}, "The mode has no formatter.", "The mode is for " <> QuotedList[storages] <> " storage."]
          }
        },
      Requires[relation_String, _] /; Lookup[variable, relation, None] === None :>
        {{"ERelation", position, variable["Head"], relation, kind, mode, table["Name"]}},
      _ -> {}
    }
  ];

BoundVariableProblems[table_Association, kind_String, mode_String, modeknown_, suffix_String, position_Integer, variable_Association] :=
  Join[
    BaseProblems[variable, position],
    If[modeknown, FormatterProblems[table, kind, mode, variable, position], {}],
    If[kind === "Store" && !EquationSetQ[variable["Head"], suffix], {{"EEquation", position, variable["Head"], SuffixText[suffix]}}, {}]
  ];

(* A var of a varlist names its variable by its head *)
VariableProblems[table_Association, kind_String, mode_String, modeknown_, suffix_String, position_Integer, var_] :=
  Module[{head = VarHead[var], variable},
    variable = If[head === $Failed, None, DeclaredVariable[head]];
    Which[
      head === $Failed,
        {{"EVar", position, ShortText[var]}}
      ,
      !BoundQ[variable],
        {{"EUnbound", position, head}}
      ,
      True,
        BoundVariableProblems[table, kind, mode, modeknown, suffix, position, variable]
    ]
  ];

(*
  One component names its variable by its head too, and must be one of its
  independent components: the mirror of a symmetric pair is none, and
  neither is the tensor with its abstract indices.
*)
ComponentProblems[table_Association, kind_String, mode_String, modeknown_, suffix_String, position_Integer, comp_] :=
  With[{variable = DeclaredVariable[Head[comp]]},
    Which[
      !BoundQ[variable],
        {{"EUnbound", position, Head[comp]}}
      ,
      MissingQ[FindComponent[variable, comp]],
        {{"EComponent", position, ShortText[comp], Head[comp]}}
      ,
      True,
        BoundVariableProblems[table, kind, mode, modeknown, suffix, position, variable]
    ]
  ];

SubjectProblems[table_Association, kind_String, mode_String, modeknown_, suffix_String, position_Integer, varlist_List] :=
  Join @@ (VariableProblems[table, kind, mode, modeknown, suffix, position, #]& /@ varlist);

SubjectProblems[table_Association, kind_String, mode_String, modeknown_, suffix_String, position_Integer, comp_] :=
  ComponentProblems[table, kind, mode, modeknown, suffix, position, comp];

NodeProblems[table_Association, line_String, position_Integer] :=
  {};

(* subject: a varlist, or one component, which like a varlist is an expression with a symbol for its head *)
NodeProblems[table_Association, (head : Load | Store)[subject : _Symbol[___], mode_String, options : (_Rule | _RuleDelayed)...], position_Integer] /; NodeOptionsQ[head, {options}] :=
  With[{kind = $Kinds[head], modes = AcceptedModes[table, $Kinds[head]], suffix = If[head === Store, OptionValue[Store, {options}, SuffixName], ""]},
    Join[
      If[MemberQ[modes, mode],
        {}
        ,
        {
          {
            "EMode", position, table["Name"], kind, mode,
            If[modes === {}, "It has no " <> kind <> " mode.", "It accepts " <> QuotedList[modes] <> "."]
          }
        }
      ],
      SubjectProblems[table, kind, mode, MemberQ[modes, mode], suffix, position, subject]
    ]
  ];

NodeProblems[table_Association, node_, position_Integer] :=
  {{"ENode", position, ShortText[node]}};

ReportProblem[{tag_String, arguments___}] :=
  Message[MessageName[Generate, tag], arguments];

(*
  Every problem is one message. The kernel stops printing a message name
  after its third use in a run and says so with General::stop; that is
  switched off while the problems are reported, so the fourth problem of a
  kind is printed like the first.
*)
ReportProblems[problems_List] :=
  With[{suppressing = Head[General::stop] =!= $Off},
    Off[General::stop];
    WithCleanup[
      Scan[ReportProblem, problems]
      ,
      If[suppressing, On[General::stop]]
    ]
  ];

(*
  Lowering: a node becomes its statements, one per component record, in the
  order binding gave them. A line of text is passed on as it is. The same
  statements are made for every backend; the right-hand sides are evaluated
  here, which is where the time of a large program goes, so it starts after
  the check has passed.
*)

(*
  The {variable record, component record} pairs of what a node is about,
  each a component in the chart: every component of every variable of a
  varlist, or the one component the node names.
*)
NodeComponents[varlist_List, coordinate_] :=
  Join @@
    Map[
      Function[var,
        With[{variable = VariableRecord[var]},
          Function[component,
            RequireChart[variable, component, coordinate];
            {variable, component}
          ] /@ variable["Components"]
        ]
      ]
      ,
      varlist
    ];

NodeComponents[comp_, coordinate_] :=
  With[{variable = VariableRecord[Head[comp]], component = ComponentRecord[comp]},
    RequireChart[variable, component, coordinate];
    {{variable, component}}
  ];

NodeStatements[config_Association, line_String, position_Integer] :=
  {line};

NodeStatements[config_Association, Load[subject_, mode_String, options___], position_Integer] :=
  Function[{variable, component},
    PrintVerbose["    Load ", component["Comp"], "..."];
    LoadStatement[config, mode, variable, component]
  ] @@@ NodeComponents[subject, NodeChart[Load, {options}]];

(*
  A Store whose variable has an equation may still have a component that no
  rule of the equation matches. Its right-hand side then evaluates to
  itself, the symbol RHSOf names applied to the component, which is no C
  code; the run stops at the first one.
*)
NodeStatements[config_Association, Store[subject_, mode_String, options___], position_Integer] :=
  With[
    {
      coordinate = NodeChart[Store, {options}],
      suffix = OptionValue[Store, {options}, SuffixName],
      extrareplacerules = OptionValue[Store, {options}, ExtraReplaceRules]
    },
    Function[{variable, component},
      PrintVerbose["    Store ", component["Comp"], "..."];
      With[{statement = StoreStatement[config, mode, suffix, coordinate, variable, component, extrareplacerules]},
        If[!FreeQ[statement["RHS"], RHSOf[variable["Head"], suffix]],
          Stop[Generate::ENoRule, position, ShortText[component["Comp"]], variable["Head"], SuffixText[suffix]]
        ];
        statement
      ]
    ] @@@ NodeComponents[subject, coordinate]
  ];

(*
  Generate - the one entry point: resolve the backend, check every node,
  then lower, render and write. Nothing is computed before the check has
  passed, and nothing is written before the last line is rendered, so a run
  that stops anywhere leaves no file. The configuration is read once and
  copied into each statement.
*)
Generate[backend_String, file_String, program_List] :=
  Module[{table = ResolveBackend[backend], nodes = Flatten[program], problems, config, statements},
    problems = Join @@ MapIndexed[NodeProblems[table, #1, First[#2]]&, nodes];
    If[problems =!= {},
      ReportProblems[problems];
      Stop[Generate::EProblems, Length[problems], If[Length[problems] == 1, "problem", "problems"]]
    ];
    config = GetConfiguration[];
    statements = Join @@ MapIndexed[NodeStatements[config, #1, First[#2]]&, nodes];
    Emit[file, Render[table, statements]]
  ];

(* Left undefined, a call of another shape would be inert: the script would run to its end, write nothing and report success *)
Generate[arguments___] :=
  Stop[Generate::EArguments, ShortText[{arguments}]];

Generate::EArguments = "Generate takes the name of a backend, the name of the output file and the program, a list: Generate[backend, file, program]. It was given the arguments `1`.";

Generate::ENode = "Node `1`: `2` is not a part of a program. A program is made of lines of text, Load[varlist, mode] and Store[varlist, mode] nodes, and lists of these. A node takes one component in place of a varlist. Load takes the option ChartName; Store takes ChartName, SuffixName, a string, and ExtraReplaceRules, a list of rules.";

Generate::EVar = "Node `1`: `2` is not a var spec. A node takes a varlist as GridTensors, TileTensors or TempTensors returned it, or a part of one, or one component that is not in a list.";

Generate::EComponent = "Node `1`: `2` is not an independent component of the bound variable `3`. A component is written in xAct's notation, with a number and a chart on each slot, and the mirror of a symmetric pair is not independent.";

Generate::EUnbound = "Node `1`: variable `2` is not bound. Bind it with GridTensors, TileTensors or TempTensors before the program is generated.";

Generate::EBase = "Node `1`: `2` declares `3` -> `4`, and `4` is not bound. Bind `4` with GridTensors, TileTensors or TempTensors.";

Generate::EMode = "Node `1`: backend \"`2`\" has no `3` mode \"`4`\". `5`";

Generate::EStorage = "Node `1`: backend \"`2`\" has no `3` mode \"`4`\" for `5`, which is bound as \"`6`\" storage. `7`";

Generate::ERelation = "Node `1`: `2` declares no `3`, which the `4` mode \"`5`\" of backend \"`6`\" reads. Add `3` -> v to its declaration, where v is the variable `2` is a derivative of.";

Generate::EEquation = "Node `1`: no equation is set for `2``3`.";

Generate::EProblems = "`1` `2` in the program, nothing was generated.";

Generate::ENoRule = "Node `1`: the equation set for `3``4` gives the component `2` no right-hand side: no rule of it matches the component.";

Protect[Generate];

(*
  The names of the surface a script had before its program was a value.
  Left undefined, a call of one would be inert: an old script would run to
  its end, write nothing and report success. So each stays defined, only to
  stop the run and say what to write in its place. A script's Import of a
  backend file, the last of that surface, is stopped by DefineBackend in
  Backend.wl.

  SetMainPrint holds its argument, as it did: the main content of an old
  script is print calls, and the message the script gets is the one of
  SetMainPrint.
*)
SetAttributes[SetMainPrint, HoldAll];

RemovedName[name_Symbol, pointer_String] := (
  MessageName[name, "ERemoved"] = SymbolName[name] <> " was removed. " <> pointer <> " See README, \"Porting a script\".";
  name[___] := Stop[MessageName[name, "ERemoved"]];
  Protect[name];
);

RemovedName[SetMainPrint, "Pass the program as a list to Generate."];

RemovedName[SetOutputFile, "Name the output file in Generate[backend, file, program]."];

RemovedName[PrintInitializations, "Write a Load[varlist, mode] node in the program handed to Generate."];

RemovedName[PrintEquations, "Write a Store[varlist, mode] node in the program handed to Generate."];

RemovedName[pr, "A line of output is a string in the program handed to Generate."];

End[];

EndPackage[];
