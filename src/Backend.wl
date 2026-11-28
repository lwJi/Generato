(* ::Package:: *)

(* Generato`Backend`, a backend is one registered table of formatters, and Render turns statements into lines *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Backend`", {"Generato`Failure`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Backend`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

CText::usage = "CText[expr] returns expr as C text, wrapped at 78 columns with no trailing newline.";

DefineBackend::usage = "DefineBackend[name, table] registers a backend under name. table states what differs from what the backend builds on: the backend table[\"Extends\"] names, or $BackendDefaults. A formatter is at table[kind][mode][storage], where storage is \"Grid\", \"Tile\", \"Temp\" or All for any. Registering has no effect on output, and a name registered again replaces the earlier table.";

ResolveBackend::usage = "ResolveBackend[name] returns the table of a registered backend, merged key by key, down to the storage, with the backend it extends or with $BackendDefaults. The table carries \"Name\" -> name. It stops the run if no backend of that name is registered.";

Render::usage = "Render[table, statements] returns the lines of a list of statements. A string is a line and passes through; a statement record is handed to the formatter at table[kind][mode][storage of its variable], or at All if the storage has no key. It stops the run if the table has no such formatter.";

$BackendDefaults::usage = "$BackendDefaults is the table every backend builds on: the \"Store\" formatters for the modes \"Out\", \"AddTo\" and \"Temp\", for any storage.";

Assign::usage = "Assign[statement] is a \"Store\" formatter: the component, \"=\", the right-hand side, \";\".";

AddAssign::usage = "AddAssign[statement] is a \"Store\" formatter: the component, \"+=\", the right-hand side, \";\".";

DeclareAssignPlain::usage = "DeclareAssignPlain[statement] is a \"Store\" formatter: the temporary type and the component as an equation refers to it, \"=\", the right-hand side, \";\".";

DeclareAssign::usage = "DeclareAssign[statement] is a \"Store\" formatter: \"const\", the temporary type and the bare name of the component, \"=\", the right-hand side, \";\".";

Begin["`Private`"];

(*
  CText - the one helper that turns an expression into C text.
  The goldens hold right-hand sides in the layout Put gives CForm[expr]: a
  long expression is broken over lines no longer than 77 characters, and a
  continuation line is indented by two spaces. InputForm at PageWidth 78
  gives the same text, and the newline that ends it is removed.
*)
CText[expr_] :=
  Module[{text = ToString[CForm[expr], InputForm, PageWidth -> 78]},
    If[StringEndsQ[text, "\n"], StringDrop[text, -1], text]
  ];

Protect[CText];

(*
  A statement is what crosses from core to a backend, one per component:

    <|"Kind" -> "Store", "Mode" -> "Temp",
      "Variable" -> <|"Head" -> vU, "Name" -> "vU", "Storage" -> "Temp", ...|>,
      "Component" -> <|"Name" -> "vU1", "Expr" -> vU1, "Slot" -> 0, ...|>,
      "Base" -> None,
      "RHS" -> MDD11 uU1 + MDD12 uU2 + MDD13 uU3,
      "Config" -> <|"TempVariableType" -> "vreal", ...|>|>

  "Variable" and "Component" are the records binding wrote, "Base" is the
  record of the variable a declared relation names, or None, and "Config"
  is the user configuration. A formatter is a function from one statement
  to its lines. It reads its argument and nothing else: it calls no getter
  and writes nothing, so it is tested by calling it on a literal record.

  The lines of an equation are one token each, which is the layout of the
  goldens: the left-hand side, the operator, the right-hand side, ";", and
  an empty line. A declaration prints the bare "Name"; an assignment prints
  "Expr", which carries the point index of the storage.
*)
Assign[statement_Association] :=
  {CText[statement["Component", "Expr"]], "=", CText[statement["RHS"]], ";", ""};

AddAssign[statement_Association] :=
  {CText[statement["Component", "Expr"]], "+=", CText[statement["RHS"]], ";", ""};

DeclareAssignPlain[statement_Association] :=
  {statement["Config", "TempVariableType"], CText[statement["Component", "Expr"]], "=", CText[statement["RHS"]], ";", ""};

DeclareAssign[statement_Association] :=
  {"const " <> statement["Config", "TempVariableType"], statement["Component", "Name"], "=", CText[statement["RHS"]], ";", ""};

Protect[Assign, AddAssign, DeclareAssignPlain, DeclareAssign];

(*
  $BackendDefaults - what a backend builds on when it extends no other.
  The table is keyed by the kind of a statement, its mode and the storage
  of its variable, and All stands for any storage.
*)
$BackendDefaults = <|
  "Store" -> <|
    "Out" -> <|All -> Assign|>,
    "AddTo" -> <|All -> AddAssign|>,
    "Temp" -> <|All -> DeclareAssignPlain|>
  |>
|>;

Protect[$BackendDefaults];

(*
  The registry: name -> table as DefineBackend was given it. It is private
  to this file; DefineBackend is its one writer and ResolveBackend its one
  reader.
*)
$Backends = <||>;

DefineBackend[name_String, table_Association] :=
  ($Backends[name] = table;);

Protect[DefineBackend];

(*
  MergeTables - a table over the one it builds on, key by key.
  Where both hold a table under a key the two are merged in turn, which
  ends at a formatter, so stating one storage of one mode leaves every
  other key of the base in place. Anywhere else the later value stands.
*)
MergeTables[base_Association, table_Association] :=
  Merge[{base, table}, If[MatchQ[#, {_Association, _Association}], MergeTables @@ #, Last[#]]&];

ResolveBackend[name_String] :=
  ResolveBackend[name, {}];

(* extending: the names on the way here, so a backend that extends itself stops *)
ResolveBackend[name_String, extending_List] :=
  Module[{table = Lookup[$Backends, name, None], base},
    If[table === None,
      Stop[ResolveBackend::EUnknown, name, Keys[$Backends]]
    ];
    If[MemberQ[extending, name],
      Stop[ResolveBackend::ECycle, name, Append[extending, name]]
    ];
    base =
      If[KeyExistsQ[table, "Extends"],
        ResolveBackend[table["Extends"], Append[extending, name]]
        ,
        $BackendDefaults
      ];
    Append[MergeTables[base, KeyDrop[table, "Extends"]], "Name" -> name]
  ];

ResolveBackend::EUnknown = "No backend is registered as \"`1`\". DefineBackend[name, table] registers one; registered are `2`.";

ResolveBackend::ECycle = "Backend \"`1`\" extends itself: `2`.";

Protect[ResolveBackend];

(* The formatter of a statement: the one for the storage of its variable, or the one for any storage *)
Formatter[table_Association, statement_Association] :=
  Module[{kind = statement["Kind"], mode = statement["Mode"], storage = statement["Variable", "Storage"], bystorage, formatter},
    bystorage = Lookup[Lookup[table, kind, <||>], mode, <||>];
    formatter = Lookup[bystorage, storage, Lookup[bystorage, All, None]];
    If[formatter === None,
      Stop[Render::EKey, table["Name"], kind, mode, storage]
    ];
    formatter
  ];

RenderStatement[table_Association, line_String] :=
  {line};

RenderStatement[table_Association, statement_Association] :=
  Module[{lines = Formatter[table, statement][statement]},
    If[!MatchQ[lines, {___String}],
      Stop[Render::ELines, table["Name"], statement["Kind"], statement["Mode"], StringTake[ToString[lines, InputForm], UpTo[200]]]
    ];
    lines
  ];

RenderStatement[table_Association, statement_] :=
  Stop[Render::EStatement, StringTake[ToString[statement, InputForm], UpTo[200]]];

Render[table_Association, statements_List] :=
  Join @@ (RenderStatement[table, #]& /@ statements);

Render::EKey = "Backend \"`1`\" has no \"`2`\" formatter for the mode \"`3`\" and the storage `4`.";

Render::ELines = "The \"`2`\" formatter of backend \"`1`\" for the mode \"`3`\" returned `4`, which is not a list of lines.";

Render::EStatement = "`1` is not a statement. A statement is a line of text or a record with \"Kind\", \"Mode\" and \"Variable\".";

Protect[Render];

End[];

EndPackage[];
