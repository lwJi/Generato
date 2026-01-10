(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module - New Nested Mode System *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: GetMode / SetMode / ResetMode *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["Phase" -> "SetComp"];
    GetMode["Phase"],
    "SetComp",
    TestID -> "Mode-Phase-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["Phase" -> "PrintComp"];
    GetMode["Phase"],
    "PrintComp",
    TestID -> "Mode-Phase-PrintComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    GetMode["Phase"],
    None,
    TestID -> "Mode-Phase-Default-None"
  ]
];

(* ========================================= *)
(* Test: SetComp Options *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["SetComp", "IndependentVarlistIndex" -> True];
    GetMode["SetComp", "IndependentVarlistIndex"],
    True,
    TestID -> "Mode-SetComp-IndependentVarlistIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["SetComp", "WithoutGridPointIndex" -> True];
    GetMode["SetComp", "WithoutGridPointIndex"],
    True,
    TestID -> "Mode-SetComp-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["SetComp", "UseTilePointIndex" -> True];
    GetMode["SetComp", "UseTilePointIndex"],
    True,
    TestID -> "Mode-SetComp-UseTilePointIndex"
  ]
];

(* ========================================= *)
(* Test: PrintComp Init Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Type" -> "Initializations"];
    SetMode["PrintComp", "Init", "Mode" -> "MainIn"];
    GetMode["PrintComp", "Init", "Mode"],
    "MainIn",
    TestID -> "Mode-PrintComp-Init-MainIn"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Init", "TensorType" -> "Vect"];
    GetMode["PrintComp", "Init", "TensorType"],
    "Vect",
    TestID -> "Mode-PrintComp-Init-TensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Init", "StorageType" -> "Tile"];
    GetMode["PrintComp", "Init", "StorageType"],
    "Tile",
    TestID -> "Mode-PrintComp-Init-StorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Init", "DerivsOrder" -> 2];
    SetMode["PrintComp", "Init", "DerivsAccuracy" -> 6];
    {GetMode["PrintComp", "Init", "DerivsOrder"], GetMode["PrintComp", "Init", "DerivsAccuracy"]},
    {2, 6},
    TestID -> "Mode-PrintComp-Init-Derivs"
  ]
];

(* ========================================= *)
(* Test: PrintComp Eqn Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Eqn", "Mode" -> "NewVar"];
    GetMode["PrintComp", "Eqn", "Mode"],
    "NewVar",
    TestID -> "Mode-PrintComp-Eqn-NewVar"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Eqn", "Mode" -> "Main"];
    GetMode["PrintComp", "Eqn", "Mode"],
    "Main",
    TestID -> "Mode-PrintComp-Eqn-Main"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Eqn", "Mode" -> "AddToMain"];
    GetMode["PrintComp", "Eqn", "Mode"],
    "AddToMain",
    TestID -> "Mode-PrintComp-Eqn-AddToMain"
  ]
];

(* ========================================= *)
(* Test: Convenience Helpers *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["Phase" -> "SetComp"];
    InSetCompPhase[],
    True,
    TestID -> "Mode-Helper-InSetCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["Phase" -> "PrintComp"];
    InPrintCompPhase[],
    True,
    TestID -> "Mode-Helper-InPrintCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Type" -> "Initializations"];
    InInitMode[],
    True,
    TestID -> "Mode-Helper-InInitMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Type" -> "Equations"];
    InEqnMode[],
    True,
    TestID -> "Mode-Helper-InEqnMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Init", "Mode" -> "MainOut"];
    GetInitMode[],
    "MainOut",
    TestID -> "Mode-Helper-GetInitMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Eqn", "Mode" -> "Main"];
    GetEqnMode[],
    "Main",
    TestID -> "Mode-Helper-GetEqnMode"
  ]
];

(* ========================================= *)
(* Test: ResetMode Subtree *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Init", "Mode" -> "MainIn"];
    SetMode["PrintComp", "Init", "TensorType" -> "Vect"];
    ResetMode["PrintComp", "Init"];
    {GetMode["PrintComp", "Init", "Mode"], GetMode["PrintComp", "Init", "TensorType"]},
    {None, None},
    TestID -> "Mode-ResetMode-Subtree"
  ]
];

(* ========================================= *)
(* Test: Validation *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    Quiet[Catch[SetMode["Phase" -> "InvalidPhase"]], SetMode::EInvalidValue],
    Null,
    TestID -> "Mode-Validation-InvalidPhase"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetMode[];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
