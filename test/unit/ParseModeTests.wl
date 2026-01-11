(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module - Flat Context-Based System *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = CreateContext[]);

(* ========================================= *)
(* Test: WithMode with nested path syntax *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"Phase"} -> "SetComp"}, GetPhase[]],
    "SetComp",
    TestID -> "Mode-Phase-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"Phase"} -> "PrintComp"}, GetPhase[]],
    "PrintComp",
    TestID -> "Mode-Phase-PrintComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    GetPhase[],
    None,
    TestID -> "Mode-Phase-Default-None"
  ]
];

(* ========================================= *)
(* Test: IndexOptions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"IndexOptions", "IndependentVarlistIndex"} -> True}, GetIndependentVarlistIndex[]],
    True,
    TestID -> "Mode-IndexOptions-IndependentVarlistIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"IndexOptions", "WithoutGridPointIndex"} -> True}, GetWithoutGridPointIndex[]],
    True,
    TestID -> "Mode-IndexOptions-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"IndexOptions", "UseTilePointIndex"} -> True}, GetUseTilePointIndex[]],
    True,
    TestID -> "Mode-IndexOptions-UseTilePointIndex"
  ]
];

(* ========================================= *)
(* Test: PrintComp Initializations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{
      {"PrintComp", "Type"} -> "Initializations",
      {"PrintComp", "Initializations", "Mode"} -> "MainIn"
    }, GetInitializationsMode[]],
    "MainIn",
    TestID -> "Mode-PrintComp-Initializations-MainIn"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Initializations", "TensorType"} -> "Vect"}, GetTensorType[]],
    "Vect",
    TestID -> "Mode-PrintComp-Initializations-TensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Initializations", "StorageType"} -> "Tile"}, GetStorageType[]],
    "Tile",
    TestID -> "Mode-PrintComp-Initializations-StorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{
      {"PrintComp", "Initializations", "DerivsOrder"} -> 2,
      {"PrintComp", "Initializations", "DerivsAccuracy"} -> 6
    }, {GetDerivsOrder[], GetDerivsAccuracy[]}],
    {2, 6},
    TestID -> "Mode-PrintComp-Initializations-Derivs"
  ]
];

(* ========================================= *)
(* Test: PrintComp Equations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Equations", "Mode"} -> "Temp"}, GetEquationsMode[]],
    "Temp",
    TestID -> "Mode-PrintComp-Equations-Temp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Equations", "Mode"} -> "MainOut"}, GetEquationsMode[]],
    "MainOut",
    TestID -> "Mode-PrintComp-Equations-MainOut"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Equations", "Mode"} -> "AddToMainOut"}, GetEquationsMode[]],
    "AddToMainOut",
    TestID -> "Mode-PrintComp-Equations-AddToMainOut"
  ]
];

(* ========================================= *)
(* Test: Convenience Helpers *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"Phase"} -> "SetComp"}, InSetCompPhase[]],
    True,
    TestID -> "Mode-Helper-InSetCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"Phase"} -> "PrintComp"}, InPrintCompPhase[]],
    True,
    TestID -> "Mode-Helper-InPrintCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Type"} -> "Initializations"}, InInitializationsMode[]],
    True,
    TestID -> "Mode-Helper-InInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Type"} -> "Equations"}, InEquationsMode[]],
    True,
    TestID -> "Mode-Helper-InEquationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Initializations", "Mode"} -> "MainOut"}, GetInitializationsMode[]],
    "MainOut",
    TestID -> "Mode-Helper-GetInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{{"PrintComp", "Equations", "Mode"} -> "MainOut"}, GetEquationsMode[]],
    "MainOut",
    TestID -> "Mode-Helper-GetEquationsMode"
  ]
];

(* ========================================= *)
(* Test: Context isolation in WithMode *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    (* Set initial values, then verify WithMode restores them *)
    $CurrentContext = SetCtx[$CurrentContext, "InitializationsMode", "MainIn"];
    $CurrentContext = SetCtx[$CurrentContext, "TensorType", "Vect"];
    (* WithMode should temporarily change, then restore *)
    WithMode[{
      {"PrintComp", "Initializations", "Mode"} -> "Temp",
      {"PrintComp", "Initializations", "TensorType"} -> "Scal"
    },
      (* Inside WithMode, check the temp values *)
      GetInitializationsMode[] === "Temp" && GetTensorType[] === "Scal"
    ] &&
    (* After WithMode, check restored values *)
    GetInitializationsMode[] === "MainIn" && GetTensorType[] === "Vect",
    True,
    TestID -> "Mode-Context-Isolation"
  ]
];

(* ========================================= *)
(* Test: Validation *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    ctx = CreateContext[];
    Quiet[Catch[SetPhase[ctx, "InvalidPhase"]], SetPhase::EInvalidValue],
    Null,
    TestID -> "Mode-Validation-InvalidPhase"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
