(* ::Package:: *)

(* BackendTests.wl *)
(* Unit tests for Generato`Backend module: CText, the store formatters, the registry and its merge, and Render *)
(* A formatter is a function of one statement, so it is tested on a literal record: no tensor is declared here *)
(* The registry lasts as long as the kernel, so each backend registered here has a name of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading BackendTests.wl..."]];

(* Load Generato which includes Backend *)
If[!MemberQ[$Packages, "Generato`Backend`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* The text PutAppend writes for expr, which CText reproduces less the final newline *)
PutAppendText[expr_] :=
  Module[{file = FileNameJoin[{$TemporaryDirectory, "generato_backend_putappend.txt"}], text},
    If[FileExistsQ[file], DeleteFile[file]];
    PutAppend[CForm[expr], file];
    text = ReadString[file];
    DeleteFile[file];
    text
  ];

(* A sum long enough to be wrapped several times *)
backendTestLongSum = Sum[k Symbol["backendTestC" <> ToString[k]]^2, {k, 1, 40}];

(*
  A store statement written out by hand: the component backendTestV1 of a
  variable with the given storage, as an equation refers to it on a grid.
*)
backendTestStatement[mode_String, storage_String] :=
  <|
    "Kind" -> "Store",
    "Mode" -> mode,
    "Variable" -> <|"Head" -> backendTestV, "Name" -> "backendTestV", "Slots" -> {backendTestI}, "Storage" -> storage|>,
    "Component" -> <|"Indices" -> {1}, "Slot" -> 0, "Name" -> "backendTestV1", "Expr" -> backendTestV1[[backendTestIjk]]|>,
    "Base" -> None,
    "RHS" -> backendTestA backendTestB + backendTestD,
    "Config" -> <|"TempVariableType" -> "vreal"|>
  |>;

(* Formatters that say which one was called, and with which component *)
backendTestMark[text_String] := Function[statement, {text <> " " <> statement["Component", "Name"]}];

(* ========================================= *)
(* Test: CText *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a short expression is one line, with no newline at its end *)
    CText[backendTestA backendTestB + backendTestD],
    "backendTestA*backendTestB + backendTestD",
    TestID -> "CText-ShortExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Part expression prints as a C subscript, as a point index does *)
    CText[backendTestA[[backendTestIjk]]],
    "backendTestA[backendTestIjk]",
    TestID -> "CText-PartExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a long expression is broken over lines shorter than 78 characters,
       and a continuation line is indented by two spaces *)
    Module[{textLines = StringSplit[CText[backendTestLongSum], "\n"]},
      {
        Length[textLines] > 3,
        Max[StringLength /@ textLines] < 78,
        AllTrue[Rest[textLines], StringStartsQ[#, "  "] &],
        StringEndsQ[CText[backendTestLongSum], "\n"]
      }
    ],
    {True, True, True, False},
    TestID -> "CText-WrapsLongExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CText is the text PutAppend[CForm[expr]] writes, less its final newline *)
    Map[
      CText[#] <> "\n" === PutAppendText[#] &,
      {
        backendTestA backendTestB + backendTestD,
        backendTestLongSum,
        Expand[(backendTestA + backendTestB + backendTestD)^6],
        backendTestA[[backendTestIjk]]^2 / (3 backendTestB[[backendTestIjk]]) - 0.5 Sqrt[backendTestD],
        backendTestF @@ Table[Symbol["backendTestArg" <> ToString[k]], {k, 1, 30}]
      }
    ],
    {True, True, True, True, True},
    TestID -> "CText-MatchesPutAppend"
  ]
];

(* ========================================= *)
(* Test: the store formatters, each on a literal statement *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* an assignment is one token a line: what the equation refers to, "=", the right-hand side, ";", an empty line *)
    Assign[backendTestStatement["Out", "Grid"]],
    {"backendTestV1[backendTestIjk]", "=", "backendTestA*backendTestB + backendTestD", ";", ""},
    TestID -> "Formatter-Assign"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    AddAssign[backendTestStatement["AddTo", "Grid"]],
    {"backendTestV1[backendTestIjk]", "+=", "backendTestA*backendTestB + backendTestD", ";", ""},
    TestID -> "Formatter-AddAssign"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the plain declaration prints the type, and the component as an equation refers to it *)
    DeclareAssignPlain[backendTestStatement["Temp", "Grid"]],
    {"vreal", "backendTestV1[backendTestIjk]", "=", "backendTestA*backendTestB + backendTestD", ";", ""},
    TestID -> "Formatter-DeclareAssignPlain"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the const declaration prints the bare name of the component *)
    DeclareAssign[backendTestStatement["Temp", "Grid"]],
    {"const vreal", "backendTestV1", "=", "backendTestA*backendTestB + backendTestD", ";", ""},
    TestID -> "Formatter-DeclareAssign"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a long right-hand side is one entry, wrapped as CText wraps it *)
    With[{lines = Assign[Append[backendTestStatement["Out", "Grid"], "RHS" -> backendTestLongSum]]},
      {Length[lines], lines[[3]] === CText[backendTestLongSum], StringContainsQ[lines[[3]], "\n"]}
    ],
    {5, True, True},
    TestID -> "Formatter-LongRightHandSideIsOneEntry"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a formatter reads its argument and nothing else: the setting in the context plays no part *)
    ResetContext[];
    SetTempVariableType["fromTheContext"];
    {
      First[DeclareAssign[backendTestStatement["Temp", "Temp"]]],
      First[DeclareAssignPlain[backendTestStatement["Temp", "Temp"]]]
    },
    {"const vreal", "vreal"},
    TestID -> "Formatter-ReadsOnlyItsStatement"
  ]
];

ResetContext[];

(* ========================================= *)
(* Test: the defaults, and the registry *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* the defaults are keyed by kind, mode and storage, and All stands for any storage *)
    $BackendDefaults,
    <|"Store" -> <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssignPlain|>|>|>,
    TestID -> "BackendDefaults-StoreModes"
  ]
];

DefineBackend["backendTestPlain", <||>];

AppendTo[$AllTests,
  VerificationTest[
    (* a backend that states nothing is the defaults under its name *)
    ResolveBackend["backendTestPlain"],
    Append[$BackendDefaults, "Name" -> "backendTestPlain"],
    TestID -> "ResolveBackend-NothingStated-IsTheDefaults"
  ]
];

(* The table codes/CarpetXGPU.wl states *)
DefineBackend["backendTestBase", <|"Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>|>];

AppendTo[$AllTests,
  VerificationTest[
    (* stating one mode leaves the other modes of the defaults in place *)
    ResolveBackend["backendTestBase"],
    <|
      "Store" -> <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssign|>|>,
      "Name" -> "backendTestBase"
    |>,
    TestID -> "ResolveBackend-OneModeStated-KeepsTheRest"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* registering has no effect but on the registry: a name registered again replaces the earlier table *)
    DefineBackend["backendTestTwice", <|"Store" -> <|"Out" -> <|All -> AddAssign|>|>|>];
    DefineBackend["backendTestTwice", <|"Store" -> <|"AddTo" -> <|All -> Assign|>|>|>];
    ResolveBackend["backendTestTwice"]["Store"],
    <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> Assign|>, "Temp" -> <|All -> DeclareAssignPlain|>|>,
    TestID -> "DefineBackend-RegisteredAgain-Replaces"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a name that is not registered stops *)
    Catch[
      ResolveBackend["backendTestNotRegistered"];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ResolveBackend::EUnknown},
    TestID -> "ResolveBackend-NotRegistered-Stops"
  ]
];

(* ========================================= *)
(* Test: a variant states only what differs (probe 1, at unit level) *)
(* ========================================= *)

(* It differs from the backend it extends in one formatter: how a tile-stored component is assigned *)
backendTestTileAssign = backendTestMark["tile"];

DefineBackend["backendTestVariant", <|
  "Extends" -> "backendTestBase",
  "Store" -> <|"Out" -> <|"Tile" -> backendTestTileAssign|>|>
|>];

AppendTo[$AllTests,
  VerificationTest[
    (* the merge goes down to the storage: the one key is added, and every key of the base stands *)
    ResolveBackend["backendTestVariant"],
    <|
      "Store" -> <|
        "Out" -> <|All -> Assign, "Tile" -> backendTestTileAssign|>,
        "AddTo" -> <|All -> AddAssign|>,
        "Temp" -> <|All -> DeclareAssign|>
      |>,
      "Name" -> "backendTestVariant"
    |>,
    TestID -> "ResolveBackend-Variant-MergedToTheStorage"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the variant prints as its base does, except where its one formatter applies *)
    With[
      {
        statements = {
          "/* a line */",
          backendTestStatement["Temp", "Temp"],
          backendTestStatement["Out", "Grid"],
          backendTestStatement["Out", "Tile"],
          backendTestStatement["AddTo", "Tile"]
        }
      },
      {
        Render[ResolveBackend["backendTestVariant"], Delete[statements, 4]] === Render[ResolveBackend["backendTestBase"], Delete[statements, 4]],
        Render[ResolveBackend["backendTestVariant"], statements[[{4}]]],
        Render[ResolveBackend["backendTestBase"], statements[[{4}]]]
      }
    ],
    {
      True,
      {"tile backendTestV1"},
      {"backendTestV1[backendTestIjk]", "=", "backendTestA*backendTestB + backendTestD", ";", ""}
    },
    TestID -> "ResolveBackend-Variant-DiffersInItsOneFormatter"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variant of a variant builds on both *)
    DefineBackend["backendTestVariant2", <|"Extends" -> "backendTestVariant", "Store" -> <|"Temp" -> <|All -> DeclareAssignPlain|>|>|>];
    ResolveBackend["backendTestVariant2"],
    <|
      "Store" -> <|
        "Out" -> <|All -> Assign, "Tile" -> backendTestTileAssign|>,
        "AddTo" -> <|All -> AddAssign|>,
        "Temp" -> <|All -> DeclareAssignPlain|>
      |>,
      "Name" -> "backendTestVariant2"
    |>,
    TestID -> "ResolveBackend-VariantOfAVariant"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the backend a variant extends must be registered *)
    DefineBackend["backendTestOrphan", <|"Extends" -> "backendTestNotRegistered"|>];
    Catch[
      ResolveBackend["backendTestOrphan"];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ResolveBackend::EUnknown},
    TestID -> "ResolveBackend-ExtendsNotRegistered-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a backend that extends itself, through another or not, stops *)
    DefineBackend["backendTestLoopA", <|"Extends" -> "backendTestLoopB"|>];
    DefineBackend["backendTestLoopB", <|"Extends" -> "backendTestLoopA"|>];
    DefineBackend["backendTestLoopC", <|"Extends" -> "backendTestLoopC"|>];
    {
      Catch[ResolveBackend["backendTestLoopA"]; "NoThrow", GeneratoFailure],
      Catch[ResolveBackend["backendTestLoopC"]; "NoThrow", GeneratoFailure]
    },
    {$Failed, $Failed},
    {ResolveBackend::ECycle, ResolveBackend::ECycle},
    TestID -> "ResolveBackend-ExtendsItself-Stops"
  ]
];

(* ========================================= *)
(* Test: Render *)
(* ========================================= *)

backendTestTable = <|
  "Name" -> "backendTestLiteral",
  "Store" -> <|
    "Out" -> <|All -> backendTestMark["any"], "Tile" -> backendTestMark["tile"]|>,
    "Temp" -> <|"Temp" -> backendTestMark["temp"]|>
  |>
|>;

AppendTo[$AllTests,
  VerificationTest[
    (* a string is a line and passes through; a record gives the lines of its formatter; the order is kept *)
    Render[
      ResolveBackend["backendTestBase"],
      {"{", backendTestStatement["Temp", "Temp"], "", backendTestStatement["AddTo", "Grid"], "}"}
    ],
    {
      "{",
      "const vreal", "backendTestV1", "=", "backendTestA*backendTestB + backendTestD", ";", "",
      "",
      "backendTestV1[backendTestIjk]", "+=", "backendTestA*backendTestB + backendTestD", ";", "",
      "}"
    },
    TestID -> "Render-StringsPassAndRecordsAreFormatted"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Render[backendTestTable, {}],
    {},
    TestID -> "Render-NoStatements"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the formatter is the one for the storage of the variable, and the one for any storage otherwise *)
    Render[
      backendTestTable,
      {backendTestStatement["Out", "Tile"], backendTestStatement["Out", "Grid"], backendTestStatement["Out", "Temp"], backendTestStatement["Temp", "Temp"]}
    ],
    {"tile backendTestV1", "any backendTestV1", "any backendTestV1", "temp backendTestV1"},
    TestID -> "Render-LooksUpKindModeAndStorage"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a mode the table lacks, and a storage a mode lacks, have no formatter *)
    {
      Catch[Render[backendTestTable, {backendTestStatement["AddTo", "Grid"]}]; "NoThrow", GeneratoFailure],
      Catch[Render[backendTestTable, {backendTestStatement["Temp", "Grid"]}]; "NoThrow", GeneratoFailure]
    },
    {$Failed, $Failed},
    {Render::EKey, Render::EKey},
    TestID -> "Render-NoFormatterForModeOrStorage-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* neither has a kind the table lacks *)
    Catch[
      Render[backendTestTable, {ReplacePart[backendTestStatement["Out", "Grid"], "Kind" -> "Load"]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {Render::EKey},
    TestID -> "Render-NoFormatterForKind-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* what a formatter returns must be lines: a name that is no formatter gives none *)
    Catch[
      Render[
        <|"Name" -> "backendTestLiteral", "Store" -> <|"Out" -> <|All -> backendTestNotAFormatter|>|>|>,
        {"/* a line */", backendTestStatement["Out", "Grid"]}
      ];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {Render::ELines},
    TestID -> "Render-FormatterReturnsNoLines-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a statement is a string or a record *)
    Catch[
      Render[backendTestTable, {"/* a line */", backendTestNotAStatement[1]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {Render::EStatement},
    TestID -> "Render-NotAStatement-Stops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["BackendTests.wl completed."]];
