(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module - Flat Context-Based System *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

$CurrentContext = SetPVerbose[$CurrentContext, False];
$CurrentContext = SetPrintDate[$CurrentContext, False];

(* ========================================= *)
(* Test: WithMode with flat key syntax *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"Phase" -> "SetComp"}, GetPhase[$CurrentContext]]
    ],
    "SetComp",
    TestID -> "Mode-Phase-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"Phase" -> "PrintComp"}, GetPhase[$CurrentContext]]
    ],
    "PrintComp",
    TestID -> "Mode-Phase-PrintComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      GetPhase[ctx]
    ],
    None,
    TestID -> "Mode-Phase-Default-None"
  ]
];

(* ========================================= *)
(* Test: IndexOptions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"IndependentVarlistIndex" -> True}, GetIndependentVarlistIndex[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-IndexOptions-IndependentVarlistIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"WithoutGridPointIndex" -> True}, GetWithoutGridPointIndex[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-IndexOptions-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"UseTilePointIndex" -> True}, GetUseTilePointIndex[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-IndexOptions-UseTilePointIndex"
  ]
];

(* ========================================= *)
(* Test: PrintComp Initializations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {
        "PrintCompType" -> "Initializations",
        "InitializationsMode" -> "MainIn"
      }, GetInitializationsMode[$CurrentContext]]
    ],
    "MainIn",
    TestID -> "Mode-PrintComp-Initializations-MainIn"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"TensorType" -> "Vect"}, GetTensorType[$CurrentContext]]
    ],
    "Vect",
    TestID -> "Mode-PrintComp-Initializations-TensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"StorageType" -> "Tile"}, GetStorageType[$CurrentContext]]
    ],
    "Tile",
    TestID -> "Mode-PrintComp-Initializations-StorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {
        "DerivsOrder" -> 2,
        "DerivsAccuracy" -> 6
      }, {GetDerivsOrder[$CurrentContext], GetDerivsAccuracy[$CurrentContext]}]
    ],
    {2, 6},
    TestID -> "Mode-PrintComp-Initializations-Derivs"
  ]
];

(* ========================================= *)
(* Test: PrintComp Equations Modes *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"EquationsMode" -> "Temp"}, GetEquationsMode[$CurrentContext]]
    ],
    "Temp",
    TestID -> "Mode-PrintComp-Equations-Temp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"EquationsMode" -> "MainOut"}, GetEquationsMode[$CurrentContext]]
    ],
    "MainOut",
    TestID -> "Mode-PrintComp-Equations-MainOut"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"EquationsMode" -> "AddToMainOut"}, GetEquationsMode[$CurrentContext]]
    ],
    "AddToMainOut",
    TestID -> "Mode-PrintComp-Equations-AddToMainOut"
  ]
];

(* ========================================= *)
(* Test: Convenience Helpers *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"Phase" -> "SetComp"}, InSetCompPhase[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-Helper-InSetCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"Phase" -> "PrintComp"}, InPrintCompPhase[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-Helper-InPrintCompPhase"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"PrintCompType" -> "Initializations"}, InInitializationsMode[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-Helper-InInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"PrintCompType" -> "Equations"}, InEquationsMode[$CurrentContext]]
    ],
    True,
    TestID -> "Mode-Helper-InEquationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"InitializationsMode" -> "MainOut"}, GetInitializationsMode[$CurrentContext]]
    ],
    "MainOut",
    TestID -> "Mode-Helper-GetInitializationsMode"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      WithMode[ctx, {"EquationsMode" -> "MainOut"}, GetEquationsMode[$CurrentContext]]
    ],
    "MainOut",
    TestID -> "Mode-Helper-GetEquationsMode"
  ]
];

(* ========================================= *)
(* Test: Context isolation in WithMode *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx, result},
      (* Create context with specific values *)
      ctx = CreateContext[];
      ctx = SetCtx[ctx, "InitializationsMode", "MainIn"];
      ctx = SetCtx[ctx, "TensorType", "Vect"];
      (* WithMode should provide modified context inside body via $CurrentContext *)
      result = WithMode[ctx, {
        "InitializationsMode" -> "Temp",
        "TensorType" -> "Scal"
      },
        (* Inside WithMode, check the temp values via context-aware getters (reads $CurrentContext) *)
        GetInitializationsMode[$CurrentContext] === "Temp" && GetTensorType[$CurrentContext] === "Scal"
      ];
      (* Original ctx is unchanged (functional style) *)
      result && GetInitializationsMode[ctx] === "MainIn" && GetTensorType[ctx] === "Vect"
    ],
    True,
    TestID -> "Mode-Context-Isolation"
  ]
];

(* ========================================= *)
(* Test: Validation *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    Module[{ctx = CreateContext[]},
      Quiet[Catch[SetPhase[ctx, "InvalidPhase"]], SetPhase::EInvalidValue]
    ],
    Null,
    TestID -> "Mode-Validation-InvalidPhase"
  ]
];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
