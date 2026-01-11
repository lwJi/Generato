(* ::Package:: *)

(* ContextTests.wl *)
(* Unit tests for Generato`Context module *)

If[Environment["QUIET"] =!= "1", Print["Loading ContextTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Import Context explicitly to access functions *)
Needs["Generato`Context`"];

(* ========================================= *)
(* Test: CreateContext *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    AssociationQ[ctx],
    True,
    TestID -> "CreateContext-ReturnsAssociation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* Check that all expected keys are present *)
    KeyExistsQ[ctx, "Phase"] &&
    KeyExistsQ[ctx, "GridPointIndex"] &&
    KeyExistsQ[ctx, "TempVariableType"] &&
    KeyExistsQ[ctx, "MapComponentToVarlist"] &&
    KeyExistsQ[ctx, "MainPrint"],
    True,
    TestID -> "CreateContext-HasExpectedKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    GetCtx[ctx, "Phase"],
    None,
    TestID -> "CreateContext-PhaseDefaultsToNone"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    GetCtx[ctx, "GridPointIndex"],
    "",
    TestID -> "CreateContext-GridPointIndexDefaultsToEmpty"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    GetCtx[ctx, "TempVariableType"],
    "double",
    TestID -> "CreateContext-TempVariableTypeDefaultsToDouble"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    GetCtx[ctx, "DerivsOrder"],
    1,
    TestID -> "CreateContext-DerivsOrderDefaultsTo1"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    GetCtx[ctx, "DerivsAccuracy"],
    4,
    TestID -> "CreateContext-DerivsAccuracyDefaultsTo4"
  ]
];

(* ========================================= *)
(* Test: CreateContext with overrides *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[<|"GridPointIndex" -> "[[ijk]]"|>];
    GetCtx[ctx, "GridPointIndex"],
    "[[ijk]]",
    TestID -> "CreateContext-WithOverride-GridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[<|"Phase" -> "SetComp", "TensorType" -> "Vect"|>];
    {GetCtx[ctx, "Phase"], GetCtx[ctx, "TensorType"]},
    {"SetComp", "Vect"},
    TestID -> "CreateContext-WithMultipleOverrides"
  ]
];

(* ========================================= *)
(* Test: GetCtx / SetCtx *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = SetCtx[ctx, "GridPointIndex", "[[ijk]]"];
    (* Original should be unchanged *)
    GetCtx[ctx, "GridPointIndex"],
    "",
    TestID -> "SetCtx-OriginalUnchanged"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = SetCtx[ctx, "GridPointIndex", "[[ijk]]"];
    (* New context should have updated value *)
    GetCtx[ctx2, "GridPointIndex"],
    "[[ijk]]",
    TestID -> "SetCtx-NewContextHasUpdate"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = SetCtx[ctx, "Phase", "PrintComp"];
    GetCtx[ctx2, "Phase"],
    "PrintComp",
    TestID -> "SetCtx-PhaseValue"
  ]
];

(* ========================================= *)
(* Test: UpdateCtx *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = UpdateCtx[ctx, <|"Phase" -> "SetComp", "TensorType" -> "Vect"|>];
    {GetCtx[ctx2, "Phase"], GetCtx[ctx2, "TensorType"]},
    {"SetComp", "Vect"},
    TestID -> "UpdateCtx-WithAssociation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = UpdateCtx[ctx, {{"Phase", "PrintComp"}, {"StorageType", "GF"}}];
    {GetCtx[ctx2, "Phase"], GetCtx[ctx2, "StorageType"]},
    {"PrintComp", "GF"},
    TestID -> "UpdateCtx-WithList"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    ctx2 = UpdateCtx[ctx, <|"Phase" -> "SetComp"|>];
    (* Original should be unchanged *)
    GetCtx[ctx, "Phase"],
    None,
    TestID -> "UpdateCtx-OriginalUnchanged"
  ]
];

(* ========================================= *)
(* Test: WithContext *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[<|"GridPointIndex" -> "[[test]]"|>];
    result = WithContext[ctx, $CurrentContext["GridPointIndex"]];
    result,
    "[[test]]",
    TestID -> "WithContext-ScopesContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    originalCtx = $CurrentContext;
    ctx = CreateContext[<|"Phase" -> "TestPhase"|>];
    WithContext[ctx, Null];
    (* $CurrentContext should be restored after WithContext *)
    $CurrentContext["Phase"] === originalCtx["Phase"],
    True,
    TestID -> "WithContext-RestoresContext"
  ]
];

(* ========================================= *)
(* Test: Context has all 30 state variables *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* 10 Basic.wl + 11 ParseMode.wl + 8 Component.wl + 1 Writefile.wl = 30 keys *)
    Length[ctx],
    30,
    TestID -> "CreateContext-Has30Keys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* Verify all Basic.wl state variables *)
    basicKeys = {"CheckInputEquations", "PVerbose", "QuietMode", "PrintDate",
                 "PrintHeaderMacro", "GridPointIndex", "TilePointIndex",
                 "SuffixUnprotected", "OutputFile", "Project"};
    AllTrue[basicKeys, KeyExistsQ[ctx, #] &],
    True,
    TestID -> "CreateContext-HasAllBasicKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* Verify all ParseMode.wl state variables *)
    modeKeys = {"Phase", "IndependentVarlistIndex", "WithoutGridPointIndex",
                "UseTilePointIndex", "PrintCompType", "InitializationsMode",
                "TensorType", "StorageType", "DerivsOrder", "DerivsAccuracy",
                "EquationsMode"};
    AllTrue[modeKeys, KeyExistsQ[ctx, #] &],
    True,
    TestID -> "CreateContext-HasAllModeKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* Verify all Component.wl state variables *)
    compKeys = {"MapComponentToVarlist", "ProcessNewVarlist", "SimplifyEquation",
                "UseLetterForTensorComponent", "TempVariableType",
                "InterfaceWithNonCoordBasis", "SuffixName", "PrefixDt"};
    AllTrue[compKeys, KeyExistsQ[ctx, #] &],
    True,
    TestID -> "CreateContext-HasAllComponentKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ctx = CreateContext[];
    (* Verify Writefile.wl state variable *)
    KeyExistsQ[ctx, "MainPrint"],
    True,
    TestID -> "CreateContext-HasMainPrintKey"
  ]
];

If[Environment["QUIET"] =!= "1", Print["ContextTests.wl completed."]];
