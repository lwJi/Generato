(* ::Package:: *)

(* ProgramTests.wl *)
(* Unit tests for Generato`Program module: a node is inert, Generate checks every node before anything is computed *)
(* and reports every problem with its position, and a program that passes is lowered, rendered and written once *)
(* The store of the records lasts as long as the kernel, like the xAct tensors, so each test uses heads of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading ProgramTests.wl..."]];

(* Load Generato which includes Program *)
If[!MemberQ[$Packages, "Generato`Program`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* ========================================= *)
(* Setup: bound variables on the test manifold from BasicTests *)
(* ========================================= *)

(* Reuse TestM3 manifold if it exists, otherwise define it *)
If[!MemberQ[$Manifolds, TestM3],
  DefManifold[TestM3, 3, IndexRange[a, z]];
  DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
  DefMetric[1, testEuclid[-i, -j], testCD];
];

ResetContext[];
SetPVerbose[False];
SetPrintDate[False];
SetGridPointIndex["[[ijk]]"];
SetTilePointIndex["[[tI]]"];

progTestGridVarlist = GridTensors[{progTestS[], PrintAs -> "s"}, {progTestU[i], PrintAs -> "u"}];

progTestOutVarlist = GridTensors[{progTestR[i]}];

progTestTileVarlist = TileTensors[{progTestT[i]}];

progTestTempVarlist = TempTensors[{progTestV[i]}];

(* A derivative that declares what it is a derivative of, and a variable of the same shape that declares nothing *)
progTestDerivVarlist = TempTensors[{progTestDS[-i], DerivativeOf -> progTestS}];

progTestPlainVarlist = TempTensors[{progTestP[-i]}];

(* A variable that is declared and not bound, and a bound derivative of it *)
progTestDeclaredVarlist = DefTensors[{progTestD[i]}];

progTestDDVarlist = TempTensors[{progTestDD[-k, i], DerivativeOf -> progTestD}];

(* A variable whose equation counts how often it is evaluated *)
progTestCountedVarlist = TempTensors[{progTestW[i]}];

(* A variable whose equation ends in a throw Generato did not raise *)
progTestThrowingVarlist = TempTensors[{progTestX[i]}];

SetEQN[progTestV[i_], progTestS[] progTestU[i]];

SetEQN[progTestR[i_], progTestV[i]];

SetEQN[{SuffixName -> "alt"}, progTestR[i_], 2 progTestV[i]];

(* Only the first component of progTestT has a right-hand side *)
SetEQN[progTestT[{1, testCart}], progTestU[{1, testCart}]];

progTestCount = 0;

SetEQNDelayed[progTestW[i_], (progTestCount++; progTestU[i])];

SetEQNDelayed[progTestX[i_], Throw["not raised by Generato"]];

(* A formatter that keeps each statement it is handed, and says which one it is, with the mode and the component *)
progTestStatements = {};

progTestKeepAs[text_String] :=
  Function[statement, AppendTo[progTestStatements, statement]; {text <> " " <> statement["Mode"] <> " " <> statement["Component", "Name"]}];

(* A backend with a load per storage, a load that reads a relation, and the stores of the defaults but for two *)
DefineBackend["progTestKeep", <|
  "Load" -> <|
    "In" -> <|"Grid" -> progTestKeepAs["grid"], "Tile" -> progTestKeepAs["tile"]|>,
    "Derivs" -> <|"Temp" -> Requires["DerivativeOf", progTestKeepAs["deriv"]]|>
  |>,
  "Store" -> <|"Out" -> <|All -> progTestKeepAs["store"]|>, "Temp" -> <|All -> progTestKeepAs["store"]|>|>
|>];

(* A backend that states nothing: it has the stores of the defaults and no load *)
DefineBackend["progTestPlain", <||>];

progTestFile = FileNameJoin[{$TemporaryDirectory, "generato_program_test.hxx"}];

(*
  progTestGenerate - hands a program to Generate, to be written through a
  backend to a file of the tests. It returns the lines of the file between
  its header and its footer, or $Failed if the run stopped; the file is
  gone after either.
*)
progTestGenerate[backend_String, program_] :=
  Module[{result, lines},
    progTestStatements = {};
    If[FileExistsQ[progTestFile], DeleteFile[progTestFile]];
    SetPrintHeaderMacro[False];
    result = Catch[Generate[backend, progTestFile, program], GeneratoFailure];
    If[result === $Failed,
      $Failed
      ,
      lines = StringSplit[ReadString[progTestFile], "\n", All];
      DeleteFile[progTestFile];
      lines[[4 ;; -4]]
    ]
  ];

(* The problems the check finds in a program, as {tag, position, arguments...}, without a message *)
progTestProblems[backend_String, program_List] :=
  Join @@ MapIndexed[Generato`Program`Private`NodeProblems[ResolveBackend[backend], #1, First[#2]]&, Flatten[program]];

(* ========================================= *)
(* Test: a node is inert *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Load and Store have no definition: a node evaluates to itself, a value with the arguments it was given *)
    Module[{load = Load[progTestGridVarlist, "In"], store = Store[progTestOutVarlist, "Out", SuffixName -> "alt"]},
      {
        Head[load], List @@ load === {progTestGridVarlist, "In"},
        Head[store], List @@ store === {progTestOutVarlist, "Out", SuffixName -> "alt"},
        DownValues[Load], UpValues[Load], SubValues[Load],
        DownValues[Store], UpValues[Store], SubValues[Store]
      }
    ],
    {Load, True, Store, True, {}, {}, {}, {}, {}, {}},
    TestID -> "Node-EvaluatesToItself"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* building a program generates nothing and changes no state, whatever its nodes ask for: *)
    (* a mode no backend has, a variable that is not bound, an equation that would count or throw *)
    ResetContext[];
    progTestCount = 0;
    If[FileExistsQ[progTestFile], DeleteFile[progTestFile]];
    Module[{before = $CurrentContext, bound = VariableRecord[progTestU], program},
      program = {
        "// a line",
        Load[progTestGridVarlist, "NoSuchMode"],
        {Load[progTestDeclaredVarlist, "In"], Store[progTestCountedVarlist, "Temp"]},
        Store[progTestThrowingVarlist, "Temp"]
      };
      {Length[Flatten[program]], $CurrentContext === before, VariableRecord[progTestU] === bound, progTestCount, FileExistsQ[progTestFile]}
    ],
    {5, True, True, 0, False},
    TestID -> "Node-BuildingAProgramIsInert"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the options of a node are named once, with what a node without them means *)
    {OptionValue[Load, {}, ChartName], OptionValue[Store, {}, {ChartName, SuffixName, ExtraReplaceRules}]},
    {Automatic, {Automatic, "", {}}},
    TestID -> "Node-Options"
  ]
];

(* ========================================= *)
(* Test: Generate lowers, renders and writes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* the program is flattened and read top to bottom: a string is a line, a node is the lines of its components, *)
    (* in the order of the varlist; a list nested to any depth is its elements, and an empty one is nothing *)
    ResetContext[];
    progTestGenerate["progTestKeep",
      {
        "// top",
        {Load[progTestGridVarlist, "In"], {}, {{"// nested"}}},
        Store[progTestTempVarlist, "Temp"],
        Table[If[k == 2, {}, "// " <> ToString[k]], {k, 3}],
        ""
      }
    ],
    {
      "// top",
      "grid In progTestS", "grid In progTestU1", "grid In progTestU2", "grid In progTestU3",
      "// nested",
      "store Temp progTestV1", "store Temp progTestV2", "store Temp progTestV3",
      "// 1", "// 3",
      ""
    },
    TestID -> "Generate-FlattensAndKeepsTheOrder"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Generate writes the file once, with its header and footer, and returns its name *)
    ResetContext[];
    SetPrintDate[False];
    If[FileExistsQ[progTestFile], DeleteFile[progTestFile]];
    {
      Generate["progTestKeep", progTestFile, {"// only"}] === progTestFile,
      progTestText = ReadString[progTestFile];
      DeleteFile[progTestFile];
      progTestText
    },
    {
      True,
      "/* generato_program_test.hxx */\n" <>
      "/* Produced with Generato */\n" <>
      "\n" <>
      "#ifndef GENERATO_PROGRAM_TEST_HXX\n" <>
      "#define GENERATO_PROGRAM_TEST_HXX\n" <>
      "\n" <>
      "// only\n" <>
      "\n" <>
      "#endif // #ifndef GENERATO_PROGRAM_TEST_HXX\n" <>
      "\n" <>
      "/* generato_program_test.hxx */\n"
    },
    TestID -> "Generate-WritesTheFileAndReturnsItsName"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a node is lowered to one statement per component record. A statement carries the records as binding *)
    (* wrote them and the configuration as it is when Generate is called, and a Store its right-hand side *)
    ResetContext[];
    SetTempVariableType["progTestReal"];
    progTestGenerate["progTestKeep", {Load[Take[progTestGridVarlist, 1], "In"], Store[progTestTempVarlist, "Temp"]}];
    {
      First[progTestStatements] === LoadStatement[GetConfiguration[], "In", VariableRecord[progTestS], First[VariableRecord[progTestS]["Components"]]],
      Keys /@ progTestStatements,
      Union[#["Variable"]& /@ Rest[progTestStatements]] === {VariableRecord[progTestV]},
      (#["Component"]& /@ Rest[progTestStatements]) === VariableRecord[progTestV]["Components"],
      Union[#["Config"]& /@ progTestStatements] === {GetConfiguration[]},
      First[progTestStatements]["Config", "TempVariableType"],
      ToString[#["RHS"], InputForm]& /@ Rest[progTestStatements]
    },
    {
      True,
      {
        {"Kind", "Mode", "Variable", "Component", "Base", "Config"},
        {"Kind", "Mode", "Variable", "Component", "Base", "RHS", "Config"},
        {"Kind", "Mode", "Variable", "Component", "Base", "RHS", "Config"},
        {"Kind", "Mode", "Variable", "Component", "Base", "RHS", "Config"}
      },
      True,
      True,
      True,
      "progTestReal",
      {"progTestS[[ijk]]*progTestU1[[ijk]]", "progTestS[[ijk]]*progTestU2[[ijk]]", "progTestS[[ijk]]*progTestU3[[ijk]]"}
    },
    TestID -> "Generate-OneSelfContainedStatementPerComponent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the storage a formatter is looked up under is the one the variable is bound with: no node states it, *)
    (* and one mode loads a grid variable and a tile variable each through its own formatter *)
    ResetContext[];
    {
      progTestGenerate["progTestKeep", {Load[Take[progTestGridVarlist, 1], "In"], Load[progTestTileVarlist, "In"]}],
      #["Variable", "Storage"]& /@ progTestStatements
    },
    {
      {"grid In progTestS", "tile In progTestT1", "tile In progTestT2", "tile In progTestT3"},
      {"Grid", "Tile", "Tile", "Tile"}
    },
    TestID -> "Generate-StorageComesFromTheBinding"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a load that reads a relation is handed the record of the variable the declaration names *)
    ResetContext[];
    {
      progTestGenerate["progTestKeep", {Load[progTestDerivVarlist, "Derivs"]}],
      Union[#["Base"]& /@ progTestStatements] === {VariableRecord[progTestS]}
    },
    {{"deriv Derivs progTestDS1", "deriv Derivs progTestDS2", "deriv Derivs progTestDS3"}, True},
    TestID -> "Generate-LoadThatReadsARelation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* SuffixName selects which of the equations set for a variable is written; an option may be named by a string *)
    ResetContext[];
    progTestGenerate["progTestKeep",
      {
        Store[progTestOutVarlist, "Out"],
        Store[progTestOutVarlist, "Out", SuffixName -> "alt"],
        Store[progTestOutVarlist, "Out", "SuffixName" -> "alt", ChartName -> testCart]
      }
    ];
    ToString[#["RHS"], InputForm]& /@ progTestStatements[[{1, 4, 7}]],
    {"progTestV1", "2*progTestV1", "2*progTestV1"},
    TestID -> "Generate-SuffixNameSelectsTheEquation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* ExtraReplaceRules are applied to the right-hand side and to what the equation assigns to, not to the name *)
    ResetContext[];
    progTestGenerate["progTestKeep",
      {Store[progTestOutVarlist, "Out", ExtraReplaceRules -> {progTestV1 -> progTestRhs, progTestR1[[ijk]] -> progTestLhs}]}
    ];
    {ToString[#["RHS"], InputForm], ToString[#["Component", "Expr"], InputForm], #["Component", "Name"]}& /@ Take[progTestStatements, 2],
    {
      {"progTestRhs", "progTestLhs", "progTestR1"},
      {"progTestV2", "progTestR2[[ijk]]", "progTestR2"}
    },
    TestID -> "Generate-ExtraReplaceRules"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable has components in the chart it was bound in: a node that names another chart stops the run *)
    ResetContext[];
    progTestGenerate["progTestKeep", {Load[progTestGridVarlist, "In", ChartName -> progTestOtherChart]}],
    $Failed,
    {RequireChart::EChart},
    TestID -> "Generate-OtherChart-Stops"
  ]
];

(* ========================================= *)
(* Test: the check reports every problem with its position *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a program that is in order has no problem *)
    progTestProblems["progTestKeep",
      {
        "", Load[progTestGridVarlist, "In"], Load[progTestTileVarlist, "In"], Load[progTestDerivVarlist, "Derivs"],
        Store[progTestTempVarlist, "Temp"], Store[progTestOutVarlist, "Out", SuffixName -> "alt"], Store[progTestOutVarlist, "AddTo"]
      }
    ],
    {},
    TestID -> "Check-NoProblem"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* what is neither a line of text nor a node, and a node of another shape: the position is the place *)
    (* in the flattened program, counted from 1 *)
    progTestProblems["progTestKeep",
      {
        "// 1",
        {7, {Load[progTestGridVarlist]}},
        Load[progTestU[i], "In"],
        Load[progTestGridVarlist, "In", SuffixName -> "alt"],
        Store[progTestOutVarlist, "Out", SuffixName -> 5],
        Store[progTestOutVarlist, "Out", ExtraReplaceRules -> progTestNotAList],
        progTestNotANode[progTestGridVarlist, "In"]
      }
    ][[All, 1 ;; 2]],
    {{"ENode", 2}, {"ENode", 3}, {"ENode", 4}, {"ENode", 5}, {"ENode", 6}, {"ENode", 7}, {"ENode", 8}},
    TestID -> "Check-NotANode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a mode the backend lacks is one problem of the node, which names the backend, the mode and the modes it accepts; *)
    (* a backend with no load says so *)
    progTestProblems[#, {"", Load[progTestGridVarlist, "MoreInOut"], Store[progTestTempVarlist, "NoSuchStore"]}]& /@ {"progTestKeep", "progTestPlain"},
    {
      {
        {"EMode", 2, "progTestKeep", "Load", "MoreInOut", "It accepts \"In\", \"Derivs\"."},
        {"EMode", 3, "progTestKeep", "Store", "NoSuchStore", "It accepts \"Out\", \"AddTo\", \"Temp\"."}
      },
      {
        {"EMode", 2, "progTestPlain", "Load", "MoreInOut", "It has no Load mode."},
        {"EMode", 3, "progTestPlain", "Store", "NoSuchStore", "It accepts \"Out\", \"AddTo\", \"Temp\"."}
      }
    },
    TestID -> "Check-UnknownMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a mode the backend has, and not for the storage the variable is bound with, is a problem of the variable *)
    progTestProblems["progTestKeep", {Load[Join[progTestGridVarlist, progTestTempVarlist], "In"], Load[progTestGridVarlist, "Derivs"]}],
    {
      {"EStorage", 1, "progTestKeep", "Load", "In", progTestV, "Temp", "The mode is for \"Grid\", \"Tile\" storage."},
      {"EStorage", 2, "progTestKeep", "Load", "Derivs", progTestS, "Grid", "The mode is for \"Temp\" storage."},
      {"EStorage", 2, "progTestKeep", "Load", "Derivs", progTestU, "Grid", "The mode is for \"Temp\" storage."}
    },
    TestID -> "Check-StorageTheModeLacks"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* what is not a var spec, a variable that is not declared, one that is declared and not bound, *)
    (* and one whose relation names a variable that is not bound *)
    progTestProblems["progTestKeep",
      {
        Load[{progTestU, {progTestNotDeclared[i]}}, "In"],
        Load[progTestDeclaredVarlist, "In"],
        Load[progTestDDVarlist, "Derivs"]
      }
    ],
    {
      {"EVar", 1, "progTestU"},
      {"EUnbound", 1, progTestNotDeclared},
      {"EUnbound", 2, progTestD},
      {"EBase", 3, progTestDD, "DerivativeOf", progTestD}
    },
    TestID -> "Check-VariableNotBound"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a formatter marked with Requires reads a relation: a variable that does not declare it is a problem, *)
    (* found from the mark in the table, and one that declares it is none *)
    progTestProblems["progTestKeep", {Load[Join[progTestDerivVarlist, progTestPlainVarlist], "Derivs"]}],
    {{"ERelation", 1, progTestP, "DerivativeOf", "Load", "Derivs", "progTestKeep"}},
    TestID -> "Check-RelationTheFormatterReads"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Store needs an equation set for the variable under its suffix name; a Load needs none *)
    progTestProblems["progTestKeep",
      {
        Load[progTestGridVarlist, "In"],
        Store[progTestGridVarlist, "Out"],
        Store[progTestOutVarlist, "Out", SuffixName -> "Puncture"],
        Store[progTestOutVarlist, "Out", SuffixName -> "alt"]
      }
    ],
    {
      {"EEquation", 2, progTestS, ""},
      {"EEquation", 2, progTestU, ""},
      {"EEquation", 3, progTestR, " with SuffixName \"Puncture\""}
    },
    TestID -> "Check-StoreWithoutAnEquation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a node may have several problems, and every one of them is found: an unknown mode does not hide *)
    (* a variable that is not bound or a missing equation *)
    progTestProblems["progTestKeep", {Store[Join[progTestDeclaredVarlist, progTestGridVarlist, progTestTempVarlist], "NoSuchStore"]}][[All, 1 ;; 2]],
    {{"EMode", 1}, {"EUnbound", 1}, {"EEquation", 1}, {"EEquation", 1}},
    TestID -> "Check-EveryProblemOfANode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* every problem is reported in one run, one message each, and then the run stops: *)
    (* nothing is computed, so no right-hand side is evaluated, and nothing is written *)
    ResetContext[];
    progTestCount = 0;
    {
      progTestGenerate["progTestKeep",
        {
          Store[progTestCountedVarlist, "Temp"],
          Load[progTestGridVarlist, "MoreInOut"],
          Load[progTestTempVarlist, "In"],
          Load[progTestPlainVarlist, "Derivs"],
          Load[progTestDeclaredVarlist, "In"],
          Store[progTestOutVarlist, "Out", SuffixName -> "Puncture"],
          progTestNotANode
        }
      ],
      progTestCount,
      progTestStatements,
      FileExistsQ[progTestFile]
    },
    {$Failed, 0, {}, False},
    {Generate::EMode, Generate::EStorage, Generate::ERelation, Generate::EUnbound, Generate::EEquation, Generate::ENode, Generate::EProblems},
    TestID -> "Generate-ReportsEveryProblemAndComputesNothing"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the fourth and the fifth problem of a kind are reported like the first: the kernel would stop printing *)
    (* a message after its third use, which is switched off for the report and on again after it *)
    ResetContext[];
    {
      progTestGenerate["progTestKeep", Table[Load[progTestGridVarlist, "NoSuchMode" <> ToString[k]], {k, 5}]],
      Head[General::stop]
    },
    {$Failed, String},
    {Generate::EMode, Generate::EMode, Generate::EMode, Generate::EMode, Generate::EMode, Generate::EProblems},
    TestID -> "Generate-ReportsMoreThanThreeProblemsOfAKind"
  ]
];

(* ========================================= *)
(* Test: a run that stops after the check writes nothing *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a variable with an equation may have a component no rule of it matches, which is found when *)
    (* the right-hand sides are evaluated: the run stops at the first one *)
    ResetContext[];
    {
      progTestProblems["progTestKeep", {Store[progTestTileVarlist, "Out"]}],
      progTestGenerate["progTestKeep", {"// before", Store[progTestTileVarlist, "Out"]}],
      FileExistsQ[progTestFile]
    },
    {{}, $Failed, False},
    {Generate::ENoRule},
    TestID -> "Generate-ComponentWithoutARightHandSide-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a throw Generato did not raise, in a right-hand side, is not caught here: it ends the run, *)
    (* and since nothing is written before the last line is rendered, no file is left *)
    ResetContext[];
    If[FileExistsQ[progTestFile], DeleteFile[progTestFile]];
    {
      Catch[Catch[Generate["progTestKeep", progTestFile, {"// before", Store[progTestThrowingVarlist, "Temp"]}]; "NoThrow", GeneratoFailure]],
      FileExistsQ[progTestFile]
    },
    {"not raised by Generato", False},
    TestID -> "Generate-ThrowInARightHandSide-WritesNothing"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a program that passes the check has each right-hand side evaluated once *)
    ResetContext[];
    progTestCount = 0;
    {
      progTestGenerate["progTestKeep", {Store[progTestCountedVarlist, "Temp"]}],
      progTestCount
    },
    {{"store Temp progTestW1", "store Temp progTestW2", "store Temp progTestW3"}, 3},
    TestID -> "Generate-EvaluatesEachRightHandSideOnce"
  ]
];

(* ========================================= *)
(* Test: a call that cannot generate stops, and is not inert *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a backend that is not registered, and that Generato has no file for, stops the run before the program is read *)
    ResetContext[];
    progTestCount = 0;
    {
      progTestGenerate["progTestNotRegistered", {Store[progTestCountedVarlist, "Temp"], progTestNotANode}],
      progTestCount
    },
    {$Failed, 0},
    {ResolveBackend::EUnknown},
    TestID -> "Generate-UnknownBackend-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a call without a program, or with a program that is not a list, stops: left inert, *)
    (* the script would write nothing and report success *)
    {
      Catch[Generate["progTestKeep", progTestFile]; "NoThrow", GeneratoFailure],
      Catch[Generate["progTestKeep", progTestFile, Load[progTestGridVarlist, "In"]]; "NoThrow", GeneratoFailure],
      FileExistsQ[progTestFile]
    },
    {$Failed, $Failed, False},
    {Generate::EArguments, Generate::EArguments},
    TestID -> "Generate-OtherArguments-Stops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ProgramTests.wl completed."]];
