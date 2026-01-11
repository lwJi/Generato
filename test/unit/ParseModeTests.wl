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
(* Test: IndexOptions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["IndexOptions", "IndependentVarlistIndex" -> True];
    GetMode["IndexOptions", "IndependentVarlistIndex"],
    True,
    TestID -> "Mode-IndexOptions-IndependentVarlistIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["IndexOptions", "WithoutGridPointIndex" -> True];
    GetMode["IndexOptions", "WithoutGridPointIndex"],
    True,
    TestID -> "Mode-IndexOptions-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["IndexOptions", "UseTilePointIndex" -> True];
    GetMode["IndexOptions", "UseTilePointIndex"],
    True,
    TestID -> "Mode-IndexOptions-UseTilePointIndex"
  ]
];

(* ========================================= *)
(* Test: PrintComp Initializations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Type" -> "Initializations"];
    SetMode["PrintComp", "Initializations", "Mode" -> "MainIn"];
    GetMode["PrintComp", "Initializations", "Mode"],
    "MainIn",
    TestID -> "Mode-PrintComp-Initializations-MainIn"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Initializations", "TensorType" -> "Vect"];
    GetMode["PrintComp", "Initializations", "TensorType"],
    "Vect",
    TestID -> "Mode-PrintComp-Initializations-TensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Initializations", "StorageType" -> "Tile"];
    GetMode["PrintComp", "Initializations", "StorageType"],
    "Tile",
    TestID -> "Mode-PrintComp-Initializations-StorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Initializations", "DerivsOrder" -> 2];
    SetMode["PrintComp", "Initializations", "DerivsAccuracy" -> 6];
    {GetMode["PrintComp", "Initializations", "DerivsOrder"], GetMode["PrintComp", "Initializations", "DerivsAccuracy"]},
    {2, 6},
    TestID -> "Mode-PrintComp-Initializations-Derivs"
  ]
];

(* ========================================= *)
(* Test: PrintComp Equations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Equations", "Mode" -> "Temp"];
    GetMode["PrintComp", "Equations", "Mode"],
    "Temp",
    TestID -> "Mode-PrintComp-Equations-Temp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Equations", "Mode" -> "MainOut"];
    GetMode["PrintComp", "Equations", "Mode"],
    "MainOut",
    TestID -> "Mode-PrintComp-Equations-MainOut"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Equations", "Mode" -> "AddToMainOut"];
    GetMode["PrintComp", "Equations", "Mode"],
    "AddToMainOut",
    TestID -> "Mode-PrintComp-Equations-AddToMainOut"
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
    InInitializationsMode[],
    True,
    TestID -> "Mode-Helper-InInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Type" -> "Equations"];
    InEquationsMode[],
    True,
    TestID -> "Mode-Helper-InEquationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Initializations", "Mode" -> "MainOut"];
    GetInitializationsMode[],
    "MainOut",
    TestID -> "Mode-Helper-GetInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Equations", "Mode" -> "MainOut"];
    GetEquationsMode[],
    "MainOut",
    TestID -> "Mode-Helper-GetEquationsMode"
  ]
];

(* ========================================= *)
(* Test: ResetMode Subtree *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetMode[];
    SetMode["PrintComp", "Initializations", "Mode" -> "MainIn"];
    SetMode["PrintComp", "Initializations", "TensorType" -> "Vect"];
    ResetMode["PrintComp", "Initializations"];
    {GetMode["PrintComp", "Initializations", "Mode"], GetMode["PrintComp", "Initializations", "TensorType"]},
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
