(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module - Flat Context-Based System *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* ========================================= *)
(* Test: WithMode with flat key syntax *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"Phase" -> "SetComp"}, GetPhase[]],
    "SetComp",
    TestID -> "Mode-Phase-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"Phase" -> "PrintComp"}, GetPhase[]],
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
    WithMode[{"IndependentVarlistIndex" -> True}, GetIndependentVarlistIndex[]],
    True,
    TestID -> "Mode-IndexOptions-IndependentVarlistIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"WithoutGridPointIndex" -> True}, GetWithoutGridPointIndex[]],
    True,
    TestID -> "Mode-IndexOptions-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"UseTilePointIndex" -> True}, GetUseTilePointIndex[]],
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
      "PrintCompType" -> "Initializations",
      "InitializationsMode" -> "MainIn"
    }, GetInitializationsMode[]],
    "MainIn",
    TestID -> "Mode-PrintComp-Initializations-MainIn"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"TensorType" -> "Vect"}, GetTensorType[]],
    "Vect",
    TestID -> "Mode-PrintComp-Initializations-TensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"StorageType" -> "Tile"}, GetStorageType[]],
    "Tile",
    TestID -> "Mode-PrintComp-Initializations-StorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{
      "DerivsOrder" -> 2,
      "DerivsAccuracy" -> 6
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
    WithMode[{"EquationsMode" -> "Temp"}, GetEquationsMode[]],
    "Temp",
    TestID -> "Mode-PrintComp-Equations-Temp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"EquationsMode" -> "MainOut"}, GetEquationsMode[]],
    "MainOut",
    TestID -> "Mode-PrintComp-Equations-MainOut"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"EquationsMode" -> "AddToMainOut"}, GetEquationsMode[]],
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
    WithMode[{"Phase" -> "SetComp"}, InSetCompPhase[]],
    True,
    TestID -> "Mode-Helper-InSetCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"Phase" -> "PrintComp"}, InPrintCompPhase[]],
    True,
    TestID -> "Mode-Helper-InPrintCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"PrintCompType" -> "Initializations"}, InInitializationsMode[]],
    True,
    TestID -> "Mode-Helper-InInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"PrintCompType" -> "Equations"}, InEquationsMode[]],
    True,
    TestID -> "Mode-Helper-InEquationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"InitializationsMode" -> "MainOut"}, GetInitializationsMode[]],
    "MainOut",
    TestID -> "Mode-Helper-GetInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    WithMode[{"EquationsMode" -> "MainOut"}, GetEquationsMode[]],
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
    $CurrentContext["InitializationsMode"] = "MainIn";
    $CurrentContext["TensorType"] = "Vect";
    (* WithMode should temporarily change, then restore *)
    WithMode[{
      "InitializationsMode" -> "Temp",
      "TensorType" -> "Scal"
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
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
