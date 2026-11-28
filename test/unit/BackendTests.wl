(* ::Package:: *)

(* BackendTests.wl *)
(* Unit tests for Generato`Backend module: CText, the store formatters, the registry, its merge and the files it reads, *)
(* the lookup of a formatter, Render and Requires, and for the load formatters of the backends in codes/ *)
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
    (* a name that is not registered, and that codes/ has no file for, stops *)
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
(* Test: a name that is not registered is read from its file *)
(* ========================================= *)

(*
  backendTestWithCodes - evaluates body with a directory of its own in the
  place of codes/, holding the files given as name -> text. Where the
  backends of Generato are is private to Backend.wl, so that is where the
  directory is put.
*)
SetAttributes[backendTestWithCodes, HoldRest];

backendTestWithCodes[files_Association, body_] :=
  Module[{directory = CreateDirectory[]},
    KeyValueMap[Export[FileNameJoin[{directory, #1 <> ".wl"}], #2, "Text"]&, files];
    WithCleanup[
      Block[{Generato`Backend`Private`$CodesDirectory = directory}, body]
      ,
      DeleteDirectory[directory, DeleteContents -> True]
    ]
  ];

AppendTo[$AllTests,
  VerificationTest[
    (* the backends of Generato are the files of codes/, beside src/ *)
    Generato`Backend`Private`$CodesDirectory,
    FileNameJoin[{Environment["GENERATO"], "codes"}],
    SameTest -> (ExpandFileName[#1] === ExpandFileName[#2]&),
    TestID -> "ResolveBackend-CodesDirectory"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the file of the name is read, and the table it registers is resolved like any other; *)
    (* a backend that extends one not registered has its file read the same way *)
    backendTestWithCodes[
      <|
        "backendTestFromFile" -> "DefineBackend[\"backendTestFromFile\", <|\"Store\" -> <|\"Temp\" -> <|All -> DeclareAssign|>|>|>];",
        "backendTestFromFileBase" -> "DefineBackend[\"backendTestFromFileBase\", <|\"Store\" -> <|\"Out\" -> <|All -> AddAssign|>|>|>];"
      |>,
      DefineBackend["backendTestExtendsFile", <|"Extends" -> "backendTestFromFileBase"|>];
      {
        ResolveBackend["backendTestFromFile"],
        ResolveBackend["backendTestExtendsFile"]["Store", "Out"]
      }
    ],
    {
      <|
        "Store" -> <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssign|>|>,
        "Name" -> "backendTestFromFile"
      |>,
      <|All -> AddAssign|>
    },
    TestID -> "ResolveBackend-NotRegistered-ReadsItsFile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a name that is registered is not read from a file: what a script registers is found first *)
    backendTestWithCodes[
      <|"backendTestPlain" -> "backendTestFileWasRead = True;"|>,
      backendTestFileWasRead = False;
      {ResolveBackend["backendTestPlain"]["Name"], backendTestFileWasRead}
    ],
    {"backendTestPlain", False},
    TestID -> "ResolveBackend-Registered-ReadsNoFile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a file that does not register the name it has stops *)
    backendTestWithCodes[
      <|"backendTestMisnamed" -> "DefineBackend[\"backendTestAnotherName\", <||>];"|>,
      Catch[ResolveBackend["backendTestMisnamed"]; "NoThrow", GeneratoFailure]
    ],
    $Failed,
    {ResolveBackend::EFile},
    TestID -> "ResolveBackend-FileRegistersAnotherName-Stops"
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
    (* the modes a table accepts for a kind are its keys there; a kind it lacks has none *)
    {AcceptedModes[backendTestTable, "Store"], AcceptedModes[backendTestTable, "Load"]},
    {{"Out", "Temp"}, {}},
    TestID -> "AcceptedModes-AreTheKeysOfTheKind"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the formatter for a storage is the one at its key, or the one for any storage; *)
    (* where there is none, what is returned says which storages the mode has, and stops nothing *)
    {
      FindFormatter[backendTestTable, "Store", "Out", "Tile"] === backendTestTable["Store", "Out", "Tile"],
      FindFormatter[backendTestTable, "Store", "Out", "Grid"] === backendTestTable["Store", "Out", All],
      FindFormatter[backendTestTable, "Store", "Temp", "Grid"],
      FindFormatter[backendTestTable, "Store", "AddTo", "Grid"],
      FindFormatter[backendTestTable, "Load", "In", "Grid"]
    },
    {True, True, Missing["NoFormatter", {"Temp"}], Missing["NoFormatter", {}], Missing["NoFormatter", {}]},
    TestID -> "FindFormatter-StorageThenAnyThenMissing"
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
(* Setup: load statements written out by hand *)
(* ========================================= *)

(*
  The records binding would write, with the fields a load formatter reads:
  a scalar, a symmetric matrix on spacetime indices with its six components,
  the first derivative of the matrix, the second derivative of the scalar,
  and the time derivative of the matrix.
*)
backendTestScalar =
  <|
    "Head" -> backendTestW, "Name" -> "backendTestW", "Slots" -> {},
    "DerivativeOf" -> None, "TimeDerivativeOf" -> None, "Storage" -> "Grid",
    "Components" -> {<|"Indices" -> {}, "Slot" -> 0, "Name" -> "backendTestW"|>}
  |>;

backendTestMatrix =
  <|
    "Head" -> backendTestG, "Name" -> "backendTestG", "Slots" -> {-a, -b},
    "DerivativeOf" -> None, "TimeDerivativeOf" -> None, "Storage" -> "Grid",
    "Components" ->
      MapIndexed[
        <|"Indices" -> #1, "Slot" -> First[#2] - 1, "Name" -> "backendTestG" <> StringJoin[ToString /@ #1]|>&,
        {{1, 1}, {1, 2}, {1, 3}, {2, 2}, {2, 3}, {3, 3}}
      ]
  |>;

backendTestDMatrix =
  <|
    "Head" -> backendTestDG, "Name" -> "backendTestDG", "Slots" -> {-k, -a, -b},
    "DerivativeOf" -> backendTestG, "TimeDerivativeOf" -> None, "Storage" -> "Temp"
  |>;

backendTestDDScalar =
  <|
    "Head" -> backendTestDDW, "Name" -> "backendTestDDW", "Slots" -> {-i, -j},
    "DerivativeOf" -> backendTestW, "TimeDerivativeOf" -> None, "Storage" -> "Temp"
  |>;

backendTestDtMatrix =
  <|
    "Head" -> backendTestDtG, "Name" -> "backendTestDtG", "Slots" -> {-a, -b},
    "DerivativeOf" -> None, "TimeDerivativeOf" -> backendTestG, "Storage" -> "Grid"
  |>;

(* One component of each: G_23 is the fifth of six, and d_2 G_13 is component 2 of G in direction 1 *)
backendTestScalarComponent = First[backendTestScalar["Components"]];

backendTestMatrixComponent = backendTestMatrix["Components"][[5]];

backendTestDMatrixComponent = <|"Indices" -> {2, 1, 3}, "Slot" -> 8, "Name" -> "backendTestDG213"|>;

backendTestDDScalarComponent = <|"Indices" -> {1, 2}, "Slot" -> 1, "Name" -> "backendTestDDW12"|>;

backendTestDtMatrixComponent = <|"Indices" -> {2, 3}, "Slot" -> 4, "Name" -> "backendTestDtG23"|>;

backendTestLoad[mode_String, storage_String, variable_Association, component_Association, base_] :=
  <|
    "Kind" -> "Load",
    "Mode" -> mode,
    "Variable" -> ReplacePart[variable, "Storage" -> storage],
    "Component" -> component,
    "Base" -> base,
    "Config" -> <|"Project" -> "backendTestProject"|>
  |>;

(* ========================================= *)
(* Test: Requires *)
(* ========================================= *)

(* A formatter that says it was called *)
backendTestCalled = {};

backendTestRecording = Function[statement, AppendTo[backendTestCalled, statement["Component", "Name"]]; {"recorded " <> statement["Component", "Name"]}];

backendTestRequiring = <|
  "Name" -> "backendTestLiteral",
  "Load" -> <|
    "Derivs" -> <|"Temp" -> Requires["DerivativeOf", backendTestRecording]|>,
    "Out" -> <|"Grid" -> Requires["TimeDerivativeOf", backendTestRecording]|>
  |>
|>;

AppendTo[$AllTests,
  VerificationTest[
    (* the mark is inert: it is the value at the key, and a table states which of its loads read a relation *)
    {
      Requires["DerivativeOf", backendTestRecording],
      Cases[backendTestRequiring["Load"], Requires[relation_, _] :> relation, {2}]
    },
    {Requires["DerivativeOf", backendTestRecording], {"DerivativeOf", "TimeDerivativeOf"}},
    TestID -> "Requires-IsAMarkInTheTable"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a statement whose variable declares the relation is handed to the marked formatter *)
    backendTestCalled = {};
    {
      Render[
        backendTestRequiring,
        {
          backendTestLoad["Derivs", "Temp", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix],
          backendTestLoad["Out", "Grid", backendTestDtMatrix, backendTestDtMatrixComponent, backendTestMatrix]
        }
      ],
      backendTestCalled
    },
    {{"recorded backendTestDG213", "recorded backendTestDtG23"}, {"backendTestDG213", "backendTestDtG23"}},
    TestID -> "Requires-RelationDeclared-Formats"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* one whose variable does not declare it stops the run, and the formatter is handed nothing: *)
    (* a variable with no relation, and one with the other relation *)
    backendTestCalled = {};
    {
      Catch[
        Render[backendTestRequiring, {backendTestLoad["Derivs", "Temp", backendTestMatrix, backendTestMatrixComponent, None]}];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        Render[backendTestRequiring, {backendTestLoad["Out", "Grid", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix]}];
        "NoThrow",
        GeneratoFailure
      ],
      backendTestCalled
    },
    {$Failed, $Failed, {}},
    {RequireRelation::EMissing, RequireRelation::EMissing},
    TestID -> "Requires-RelationMissing-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the mark is merged like any formatter, and a variant that replaces the leaf drops the requirement where it says so *)
    DefineBackend["backendTestRequires", KeyDrop[backendTestRequiring, "Name"]];
    DefineBackend["backendTestRequiresNot", <|"Extends" -> "backendTestRequires", "Load" -> <|"Derivs" -> <|"Temp" -> backendTestMark["free"]|>|>|>];
    {
      ResolveBackend["backendTestRequires"]["Load"] === backendTestRequiring["Load"],
      ResolveBackend["backendTestRequiresNot"]["Load"],
      Render[
        ResolveBackend["backendTestRequiresNot"],
        {backendTestLoad["Derivs", "Temp", backendTestMatrix, backendTestMatrixComponent, None]}
      ]
    },
    {
      True,
      <|
        "Derivs" -> <|"Temp" -> backendTestMark["free"]|>,
        "Out" -> <|"Grid" -> Requires["TimeDerivativeOf", backendTestRecording]|>
      |>,
      {"free backendTestG23"}
    },
    TestID -> "Requires-MergedAndReplacedLikeAFormatter"
  ]
];

(* ========================================= *)
(* Test: the load formatters of the backends in codes/, each on a literal statement *)
(* ========================================= *)

(*
  A backend file is one DefineBackend call, and ResolveBackend reads it when
  its name is first asked for: nothing here loads CarpetXGPU or AMReX.
  The file of Nmesh still ends by starting the write of the script that
  imported it. Here it is read for its table only, so the write is held back.
*)
Block[{WriteToFile},
  Get[FileNameJoin[{Environment["GENERATO"], "codes", "Nmesh.wl"}]]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the keys of a table are the loads the backend accepts: a mode, and under it a storage *)
    Map[Keys, ResolveBackend[#]["Load"]]& /@ {"CarpetXGPU", "AMReX", "Nmesh"},
    {
      <|"In" -> {"Grid", "Tile"}, "Out" -> {"Grid", "Tile"}, "Derivs" -> {"Temp"}, "Temp" -> {"Temp"}|>,
      <|"In" -> {"Grid"}, "Out" -> {"Grid"}|>,
      <|"In" -> {"Grid"}, "Out" -> {"Grid"}, "MoreInOut" -> {"Grid"}, "Temp" -> {"Temp"}|>
    },
    TestID -> "Backends-LoadKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the loads that read a relation say so where they are registered, and no other load does *)
    Map[
      Function[name, Cases[ResolveBackend[name]["Load"], Requires[relation_, _] :> relation, {2}]],
      {"CarpetXGPU", "AMReX", "Nmesh"}
    ],
    {{"DerivativeOf"}, {}, {"TimeDerivativeOf"}},
    TestID -> "Backends-LoadsThatRequireARelation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the stores are the defaults, except that CarpetXGPU and AMReX declare a temporary const *)
    #["Store"]& /@ ResolveBackend /@ {"CarpetXGPU", "AMReX", "Nmesh"},
    {
      <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssign|>|>,
      <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssign|>|>,
      <|"Out" -> <|All -> Assign|>, "AddTo" -> <|All -> AddAssign|>, "Temp" -> <|All -> DeclareAssignPlain|>|>
    },
    TestID -> "Backends-Stores"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CarpetXGPU: a grid function is read from gf_<variable> at the index of the layout rule, in and out alike; *)
    (* a scalar has no index *)
    Render[
      ResolveBackend["CarpetXGPU"],
      {
        backendTestLoad["In", "Grid", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["Out", "Grid", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["In", "Grid", backendTestScalar, backendTestScalarComponent, None]
      }
    ],
    {
      "const auto backendTestG23 = gf_backendTestG[4];",
      "const auto backendTestG23 = gf_backendTestG[4];",
      "const auto backendTestW = gf_backendTestW;"
    },
    TestID -> "CarpetXGPU-GridLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CarpetXGPU: a tile is read from tl_<variable>; a derivative stored on a tile has the two indices of the rule *)
    Render[
      ResolveBackend["CarpetXGPU"],
      {
        backendTestLoad["In", "Tile", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["Out", "Tile", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["In", "Tile", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix]
      }
    ],
    {
      "const auto backendTestG23 = tl_backendTestG[4].ptr;",
      "const auto backendTestG23 = tl_backendTestG[4].ptr;",
      "const auto backendTestDG213 = tl_backendTestDG[2][1].ptr;"
    },
    TestID -> "CarpetXGPU-TileLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CarpetXGPU: a derivative that is not stored is computed from the component at its trailing indices; *)
    (* its order is the number of slots it has more, and the directions are its leading indices *)
    Render[
      ResolveBackend["CarpetXGPU"],
      {
        backendTestLoad["Derivs", "Temp", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix],
        backendTestLoad["Derivs", "Temp", backendTestDDScalar, backendTestDDScalarComponent, backendTestScalar]
      }
    ],
    {
      "const auto backendTestDG213 = calcderivs1_2(backendTestG13, p.i, p.j, p.k);",
      "const auto backendTestDDW12 = calcderivs2_12(backendTestW, p.i, p.j, p.k);"
    },
    TestID -> "CarpetXGPU-DerivLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CarpetXGPU: a temporary is declared by its bare name *)
    Render[ResolveBackend["CarpetXGPU"], {backendTestLoad["Temp", "Temp", backendTestMatrix, backendTestMatrixComponent, None]}],
    {"auto backendTestG23;"},
    TestID -> "CarpetXGPU-Declare"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* AMReX: a variable is read from the array of its name, in and out alike *)
    Render[
      ResolveBackend["AMReX"],
      {
        backendTestLoad["In", "Grid", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["Out", "Grid", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["In", "Grid", backendTestScalar, backendTestScalarComponent, None]
      }
    ],
    {
      "auto &backendTestG23 = backendTestG[4];",
      "auto &backendTestG23 = backendTestG[4];",
      "auto &backendTestW = backendTestW;"
    },
    TestID -> "AMReX-GridLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Nmesh: an evolved variable is read from vlu at its index in the project: one letter per slot, *)
    (* "t" for a spacetime index and "x" for a spatial one, and the slot of the component after the first *)
    Render[
      ResolveBackend["Nmesh"],
      {
        backendTestLoad["In", "Grid", backendTestMatrix, First[backendTestMatrix["Components"]], None],
        backendTestLoad["In", "Grid", backendTestMatrix, backendTestMatrixComponent, None],
        backendTestLoad["In", "Grid", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix],
        backendTestLoad["In", "Grid", backendTestScalar, backendTestScalarComponent, None]
      }
    ],
    {
      "double *backendTestG11 = Vard(node, Vind(vlu,backendTestProject->i_backendTestGtt));",
      "double *backendTestG23 = Vard(node, Vind(vlu,backendTestProject->i_backendTestGtt+4));",
      "double *backendTestDG213 = Vard(node, Vind(vlu,backendTestProject->i_backendTestDGxtt+8));",
      "double *backendTestW = Vard(node, Vind(vlu,backendTestProject->i_backendTestW));"
    },
    TestID -> "Nmesh-InLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Nmesh: a right-hand side is read from vlr at the index of the variable it is the time derivative of *)
    Render[
      ResolveBackend["Nmesh"],
      {backendTestLoad["Out", "Grid", backendTestDtMatrix, backendTestDtMatrixComponent, backendTestMatrix]}
    ],
    {"double *backendTestDtG23 = Vard(node, Vind(vlr,backendTestProject->i_backendTestGtt+4));"},
    TestID -> "Nmesh-OutLoad"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Nmesh: any other variable of the node is read at its own index, and a temporary is declared double *)
    Render[
      ResolveBackend["Nmesh"],
      {
        backendTestLoad["MoreInOut", "Grid", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix],
        backendTestLoad["MoreInOut", "Grid", backendTestScalar, backendTestScalarComponent, None],
        backendTestLoad["Temp", "Temp", backendTestMatrix, backendTestMatrixComponent, None]
      }
    ],
    {
      "double *backendTestDG213 = Vard(node, ibackendTestDGxtt+8);",
      "double *backendTestW = Vard(node, ibackendTestW);",
      "double backendTestG23;"
    },
    TestID -> "Nmesh-MoreInOutLoadAndDeclare"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a load formatter reads its statement and nothing else: the settings in the context play no part *)
    ResetContext[];
    SetProject["fromTheContext"];
    SetGridPointIndex["[[fromTheContext]]"];
    Render[
      ResolveBackend["Nmesh"],
      {backendTestLoad["In", "Grid", backendTestMatrix, backendTestMatrixComponent, None]}
    ],
    {"double *backendTestG23 = Vard(node, Vind(vlu,backendTestProject->i_backendTestGtt+4));"},
    TestID -> "LoadFormatter-ReadsOnlyItsStatement"
  ]
];

ResetContext[];

(* What Render gives one statement through a backend in codes/, or $Failed if it stops *)
backendTestRender[name_String, statement_Association] :=
  Catch[Render[ResolveBackend[name], {statement}], GeneratoFailure];

AppendTo[$AllTests,
  VerificationTest[
    (* CarpetXGPU has no load for a mode it lacks, and none for a temporary that is read like a grid function *)
    {
      backendTestRender["CarpetXGPU", backendTestLoad["MoreInOut", "Grid", backendTestMatrix, backendTestMatrixComponent, None]],
      backendTestRender["CarpetXGPU", backendTestLoad["In", "Temp", backendTestMatrix, backendTestMatrixComponent, None]]
    },
    {$Failed, $Failed},
    {Render::EKey, Render::EKey},
    TestID -> "CarpetXGPU-NoLoadForModeOrStorage-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* AMReX loads a grid variable and nothing else: it has no tile *)
    {
      backendTestRender["AMReX", backendTestLoad["In", "Tile", backendTestMatrix, backendTestMatrixComponent, None]],
      backendTestRender["AMReX", backendTestLoad["Out", "Tile", backendTestMatrix, backendTestMatrixComponent, None]]
    },
    {$Failed, $Failed},
    {Render::EKey, Render::EKey},
    TestID -> "AMReX-NoTileLoad-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* neither does it compute a derivative or declare a temporary *)
    {
      backendTestRender["AMReX", backendTestLoad["Derivs", "Temp", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix]],
      backendTestRender["AMReX", backendTestLoad["Temp", "Temp", backendTestMatrix, backendTestMatrixComponent, None]]
    },
    {$Failed, $Failed},
    {Render::EKey, Render::EKey},
    TestID -> "AMReX-NoDerivsOrTempLoad-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Nmesh computes no derivative and has no tile *)
    {
      backendTestRender["Nmesh", backendTestLoad["Derivs", "Temp", backendTestDMatrix, backendTestDMatrixComponent, backendTestMatrix]],
      backendTestRender["Nmesh", backendTestLoad["In", "Tile", backendTestMatrix, backendTestMatrixComponent, None]]
    },
    {$Failed, $Failed},
    {Render::EKey, Render::EKey},
    TestID -> "Nmesh-NoDerivsOrTileLoad-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the loads that read a relation stop at a variable that does not declare it *)
    {
      backendTestRender["CarpetXGPU", backendTestLoad["Derivs", "Temp", backendTestMatrix, backendTestMatrixComponent, None]],
      backendTestRender["Nmesh", backendTestLoad["Out", "Grid", backendTestMatrix, backendTestMatrixComponent, None]]
    },
    {$Failed, $Failed},
    {RequireRelation::EMissing, RequireRelation::EMissing},
    TestID -> "Backends-LoadWithoutItsRelation-Stops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["BackendTests.wl completed."]];
