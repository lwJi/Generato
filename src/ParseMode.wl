(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`ParseMode`, {2024, 7, 06}"];
  System`Print["------------------------------------------------------------"];
];

GetParseMode::usage = "GetParseMode[key] returns True if the specified parsing mode is active, False otherwise.";

SetParseMode::usage = "SetParseMode[key->value] sets the specified parsing mode.";

SetParseModeAllToFalse::usage = "SetParseModeAllToFalse[] deactivates all parsing modes.";

CleanParseMode::usage = "CleanParseMode[] clears the parsing mode association.";

SetComp::usage = "SetComp is a ParseMode option that indicates component-setting phase, where tensor components are assigned to expressions.";

PrintComp::usage = "PrintComp is a ParseMode option that indicates component-printing phase, where tensor components are written to output.";

Protect[SetComp];

Protect[PrintComp];

GetParseSetCompMode::usage = "GetParseSetCompMode[key] returns True if the specified SetComp mode is active, False otherwise.";

SetParseSetCompMode::usage = "SetParseSetCompMode[key->value] sets the specified SetComp mode.";

SetParseSetCompModeAllToFalse::usage = "SetParseSetCompModeAllToFalse[] deactivates all SetComp modes.";

CleanParseSetCompMode::usage = "CleanParseSetCompMode[] clears the SetComp mode association.";

IndependentVarlistIndex::usage = "IndependentVarlistIndex is a SetComp mode option that resets component indices for each new variable in the varlist.";

WithoutGridPointIndex::usage = "WithoutGridPointIndex is a SetComp mode option that omits the grid point index suffix from variable names.";

UseTilePointIndex::usage = "UseTilePointIndex is a SetComp mode option that uses tile point index instead of grid point index.";

Protect[IndependentVarlistIndex];

Protect[WithoutGridPointIndex];

Protect[UseTilePointIndex];

GetParsePrintCompMode::usage = "GetParsePrintCompMode[key] returns True if the specified PrintComp mode is active, False otherwise.";

SetParsePrintCompMode::usage = "SetParsePrintCompMode[key->value] sets the specified PrintComp mode.";

SetParsePrintCompModeAllToFalse::usage = "SetParsePrintCompModeAllToFalse[] deactivates all PrintComp modes.";

CleanParsePrintCompMode::usage = "CleanParsePrintCompMode[] clears the PrintComp mode association.";

Initializations::usage = "Initializations is a PrintComp mode option that indicates initialization code generation phase.";

Equations::usage = "Equations is a PrintComp mode option that indicates equation code generation phase.";

Protect[Initializations];

Protect[Equations];

GetParsePrintCompInitMode::usage = "GetParsePrintCompInitMode[key] returns the value for the specified initialization mode key.";

SetParsePrintCompInitMode::usage = "SetParsePrintCompInitMode[key->value] sets the specified initialization mode.";

CleanParsePrintCompInitMode::usage = "CleanParsePrintCompInitMode[] clears the initialization mode association.";

GetParsePrintCompEQNMode::usage = "GetParsePrintCompEQNMode[key] returns True if the specified equation mode is active, False otherwise.";

SetParsePrintCompEQNMode::usage = "SetParsePrintCompEQNMode[key->value] sets the specified equation mode.";

CleanParsePrintCompEQNMode::usage = "CleanParsePrintCompEQNMode[] clears the equation mode association.";

GetParsePrintCompInitTensorType::usage = "GetParsePrintCompInitTensorType[key] returns True if the specified tensor type is active, False otherwise.";

SetParsePrintCompInitTensorType::usage = "SetParsePrintCompInitTensorType[key->value] sets the specified tensor type mode.";

CleanParsePrintCompInitTensorType::usage = "CleanParsePrintCompInitTensorType[] clears the tensor type association.";

GetParsePrintCompInitStorageType::usage = "GetParsePrintCompInitStorageType[key] returns True if the specified storage type is active, False otherwise.";

SetParsePrintCompInitStorageType::usage = "SetParsePrintCompInitStorageType[key->value] sets the specified storage type mode.";

CleanParsePrintCompInitStorageType::usage = "CleanParsePrintCompInitStorageType[] clears the storage type association.";

(* ============================================== *)
(* NEW CLEANER API - Preferred for new code      *)
(* ============================================== *)

SetCompPhaseQ::usage = "SetCompPhaseQ[] returns True if currently in SetComp phase.";
PrintCompPhaseQ::usage = "PrintCompPhaseQ[] returns True if currently in PrintComp phase.";
GetCurrentPhase::usage = "GetCurrentPhase[] returns the current phase (SetComp, PrintComp, or None).";

IndependentVarlistIndexQ::usage = "IndependentVarlistIndexQ[] returns True if IndependentVarlistIndex mode is active.";
WithoutGridPointIndexQ::usage = "WithoutGridPointIndexQ[] returns True if WithoutGridPointIndex mode is active.";
UseTilePointIndexQ::usage = "UseTilePointIndexQ[] returns True if UseTilePointIndex mode is active.";

InitializationsPhaseQ::usage = "InitializationsPhaseQ[] returns True if currently in Initializations sub-phase.";
EquationsPhaseQ::usage = "EquationsPhaseQ[] returns True if currently in Equations sub-phase.";
GetCurrentSubPhase::usage = "GetCurrentSubPhase[] returns the current sub-phase (Initializations, Equations, or None).";

MainOutModeQ::usage = "MainOutModeQ[] returns True if in MainOut initialization mode.";
MainInModeQ::usage = "MainInModeQ[] returns True if in MainIn initialization mode.";
DerivsModeQ::usage = "DerivsModeQ[] returns True if in Derivs initialization mode.";
MoreInOutModeQ::usage = "MoreInOutModeQ[] returns True if in MoreInOut initialization mode.";
TempInitModeQ::usage = "TempInitModeQ[] returns True if in Temp initialization mode.";
GetInitMode::usage = "GetInitMode[] returns the current initialization mode.";
GetDerivsOrder::usage = "GetDerivsOrder[] returns the current derivative order.";
GetDerivsAccuracy::usage = "GetDerivsAccuracy[] returns the current derivative accuracy.";

ScalTensorTypeQ::usage = "ScalTensorTypeQ[] returns True if tensor type is Scal.";
VectTensorTypeQ::usage = "VectTensorTypeQ[] returns True if tensor type is Vect.";
SmatTensorTypeQ::usage = "SmatTensorTypeQ[] returns True if tensor type is Smat.";
GetTensorType::usage = "GetTensorType[] returns the current tensor type.";

GFStorageTypeQ::usage = "GFStorageTypeQ[] returns True if storage type is GF.";
TileStorageTypeQ::usage = "TileStorageTypeQ[] returns True if storage type is Tile.";
GetStorageType::usage = "GetStorageType[] returns the current storage type.";

NewVarModeQ::usage = "NewVarModeQ[] returns True if in NewVar equation mode.";
MainEqnModeQ::usage = "MainEqnModeQ[] returns True if in Main equation mode.";
AddToMainModeQ::usage = "AddToMainModeQ[] returns True if in AddToMain equation mode.";
GetEqnMode::usage = "GetEqnMode[] returns the current equation mode.";

InitializeModeState::usage = "InitializeModeState[] resets all mode state to defaults.";
ResetModeState::usage = "ResetModeState[] resets all mode state to defaults (alias for InitializeModeState).";

Begin["`Private`"];

(* ============================================== *)
(* NEW UNIFIED MODE STATE                         *)
(* Replaces 7 separate associations               *)
(* ============================================== *)

$ParseModeState = <|
  "Phase" -> None,  (* SetComp | PrintComp | None *)
  "SetComp" -> <|
    "IndependentVarlistIndex" -> False,
    "WithoutGridPointIndex" -> False,
    "UseTilePointIndex" -> False
  |>,
  "PrintComp" -> <|
    "SubPhase" -> None,  (* Initializations | Equations | None *)
    "Init" -> <|
      "Mode" -> None,  (* MainOut | MainIn | Derivs | MoreInOut | Temp | None *)
      "DerivsOrder" -> 1,
      "DerivsAccuracy" -> 4,
      "TensorType" -> None,  (* Scal | Vect | Smat | None *)
      "StorageType" -> None  (* GF | Tile | None *)
    |>,
    "Equation" -> <|
      "Mode" -> None  (* NewVar | Main | AddToMain | None *)
    |>
  |>
|>;

(* Initialize default state *)
InitializeModeState[] := (
  $ParseModeState = <|
    "Phase" -> None,
    "SetComp" -> <|
      "IndependentVarlistIndex" -> False,
      "WithoutGridPointIndex" -> False,
      "UseTilePointIndex" -> False
    |>,
    "PrintComp" -> <|
      "SubPhase" -> None,
      "Init" -> <|
        "Mode" -> None,
        "DerivsOrder" -> 1,
        "DerivsAccuracy" -> 4,
        "TensorType" -> None,
        "StorageType" -> None
      |>,
      "Equation" -> <|
        "Mode" -> None
      |>
    |>
  |>;
);

Protect[InitializeModeState];

(* ============================================== *)
(* NEW CLEANER API IMPLEMENTATIONS               *)
(* ============================================== *)

(* Phase queries *)
SetCompPhaseQ[] := ($ParseModeState["Phase"] === SetComp);
PrintCompPhaseQ[] := ($ParseModeState["Phase"] === PrintComp);
GetCurrentPhase[] := $ParseModeState["Phase"];

Protect[SetCompPhaseQ];
Protect[PrintCompPhaseQ];
Protect[GetCurrentPhase];

(* SetComp sub-mode queries *)
IndependentVarlistIndexQ[] := $ParseModeState["SetComp", "IndependentVarlistIndex"];
WithoutGridPointIndexQ[] := $ParseModeState["SetComp", "WithoutGridPointIndex"];
UseTilePointIndexQ[] := $ParseModeState["SetComp", "UseTilePointIndex"];

Protect[IndependentVarlistIndexQ];
Protect[WithoutGridPointIndexQ];
Protect[UseTilePointIndexQ];

(* PrintComp sub-phase queries *)
InitializationsPhaseQ[] := ($ParseModeState["PrintComp", "SubPhase"] === Initializations);
EquationsPhaseQ[] := ($ParseModeState["PrintComp", "SubPhase"] === Equations);
GetCurrentSubPhase[] := $ParseModeState["PrintComp", "SubPhase"];

Protect[InitializationsPhaseQ];
Protect[EquationsPhaseQ];
Protect[GetCurrentSubPhase];

(* Init mode queries - using SymbolName for context-independent comparison *)
MainOutModeQ[] := Module[{m = $ParseModeState["PrintComp", "Init", "Mode"]},
  m =!= None && SymbolName[m] === "MainOut"
];
MainInModeQ[] := Module[{m = $ParseModeState["PrintComp", "Init", "Mode"]},
  m =!= None && SymbolName[m] === "MainIn"
];
DerivsModeQ[] := Module[{m = $ParseModeState["PrintComp", "Init", "Mode"]},
  m =!= None && SymbolName[m] === "Derivs"
];
MoreInOutModeQ[] := Module[{m = $ParseModeState["PrintComp", "Init", "Mode"]},
  m =!= None && SymbolName[m] === "MoreInOut"
];
TempInitModeQ[] := Module[{m = $ParseModeState["PrintComp", "Init", "Mode"]},
  m =!= None && SymbolName[m] === "Temp"
];
GetInitMode[] := $ParseModeState["PrintComp", "Init", "Mode"];
GetDerivsOrder[] := $ParseModeState["PrintComp", "Init", "DerivsOrder"];
GetDerivsAccuracy[] := $ParseModeState["PrintComp", "Init", "DerivsAccuracy"];

Protect[MainOutModeQ];
Protect[MainInModeQ];
Protect[DerivsModeQ];
Protect[MoreInOutModeQ];
Protect[TempInitModeQ];
Protect[GetInitMode];
Protect[GetDerivsOrder];
Protect[GetDerivsAccuracy];

(* Tensor type queries - using SymbolName for context-independent comparison *)
ScalTensorTypeQ[] := Module[{t = $ParseModeState["PrintComp", "Init", "TensorType"]},
  t =!= None && SymbolName[t] === "Scal"
];
VectTensorTypeQ[] := Module[{t = $ParseModeState["PrintComp", "Init", "TensorType"]},
  t =!= None && SymbolName[t] === "Vect"
];
SmatTensorTypeQ[] := Module[{t = $ParseModeState["PrintComp", "Init", "TensorType"]},
  t =!= None && SymbolName[t] === "Smat"
];
GetTensorType[] := $ParseModeState["PrintComp", "Init", "TensorType"];

Protect[ScalTensorTypeQ];
Protect[VectTensorTypeQ];
Protect[SmatTensorTypeQ];
Protect[GetTensorType];

(* Storage type queries - using SymbolName for context-independent comparison *)
GFStorageTypeQ[] := Module[{s = $ParseModeState["PrintComp", "Init", "StorageType"]},
  s =!= None && SymbolName[s] === "GF"
];
TileStorageTypeQ[] := Module[{s = $ParseModeState["PrintComp", "Init", "StorageType"]},
  s =!= None && SymbolName[s] === "Tile"
];
GetStorageType[] := $ParseModeState["PrintComp", "Init", "StorageType"];

Protect[GFStorageTypeQ];
Protect[TileStorageTypeQ];
Protect[GetStorageType];

(* Equation mode queries - using SymbolName for context-independent comparison *)
NewVarModeQ[] := Module[{m = $ParseModeState["PrintComp", "Equation", "Mode"]},
  m =!= None && SymbolName[m] === "NewVar"
];
MainEqnModeQ[] := Module[{m = $ParseModeState["PrintComp", "Equation", "Mode"]},
  m =!= None && SymbolName[m] === "Main"
];
AddToMainModeQ[] := Module[{m = $ParseModeState["PrintComp", "Equation", "Mode"]},
  m =!= None && SymbolName[m] === "AddToMain"
];
GetEqnMode[] := $ParseModeState["PrintComp", "Equation", "Mode"];

Protect[NewVarModeQ];
Protect[MainEqnModeQ];
Protect[AddToMainModeQ];
Protect[GetEqnMode];

(* Reset all mode state *)
ResetModeState[] := InitializeModeState[];

Protect[ResetModeState];

(* ============================================== *)
(* LEGACY API - Deprecated, kept for backward    *)
(* compatibility. Use new API above instead.     *)
(* ============================================== *)

(* Level 1: Phase - DEPRECATED: Use SetCompPhaseQ[], PrintCompPhaseQ[] instead *)
GetParseMode[key_] :=
  Module[{},
    Which[
      key === SetComp, $ParseModeState["Phase"] === SetComp,
      key === PrintComp, $ParseModeState["Phase"] === PrintComp,
      True, False
    ]
  ];

Protect[GetParseMode];

(* DEPRECATED: Use new API functions for setting modes *)
(* Handle lists of rules, associations, and single rules *)
SetParseMode[list_List] := SetParseMode[Association[list]];
SetParseMode[rule_Rule] := SetParseMode[<|rule|>];

SetParseMode[assoc_Association] :=
  Module[{},
    If[Lookup[assoc, SetComp, False], $ParseModeState["Phase"] = SetComp];
    If[Lookup[assoc, PrintComp, False], $ParseModeState["Phase"] = PrintComp];
    If[!Lookup[assoc, SetComp, False] && !Lookup[assoc, PrintComp, False],
      $ParseModeState["Phase"] = None];
  ];

Protect[SetParseMode];

SetParseModeAllToFalse[] :=
  Module[{},
    $ParseModeState["Phase"] = None;
  ];

Protect[SetParseModeAllToFalse];

CleanParseMode[] :=
  Module[{},
    $ParseModeState["Phase"] = None;
  ];

Protect[CleanParseMode];

(* Level 2: SetComp sub-modes - DEPRECATED: Use IndependentVarlistIndexQ[], etc. instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParseSetCompMode[key_] :=
  Module[{keyName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    Which[
      keyName === "IndependentVarlistIndex", $ParseModeState["SetComp", "IndependentVarlistIndex"],
      keyName === "WithoutGridPointIndex", $ParseModeState["SetComp", "WithoutGridPointIndex"],
      keyName === "UseTilePointIndex", $ParseModeState["SetComp", "UseTilePointIndex"],
      True, False
    ]
  ];

Protect[GetParseSetCompMode];

(* Handle lists of rules, associations, and single rules *)
SetParseSetCompMode[list_List] := SetParseSetCompMode[Association[list]];
SetParseSetCompMode[rule_Rule] := SetParseSetCompMode[<|rule|>];

SetParseSetCompMode[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "IndependentVarlistIndex", $ParseModeState["SetComp", "IndependentVarlistIndex"] = val,
            keyName === "WithoutGridPointIndex", $ParseModeState["SetComp", "WithoutGridPointIndex"] = val,
            keyName === "UseTilePointIndex", $ParseModeState["SetComp", "UseTilePointIndex"] = val
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParseSetCompMode];

SetParseSetCompModeAllToFalse[] :=
  Module[{},
    $ParseModeState["SetComp"] = <|
      "IndependentVarlistIndex" -> False,
      "WithoutGridPointIndex" -> False,
      "UseTilePointIndex" -> False
    |>;
  ];

Protect[SetParseSetCompModeAllToFalse];

CleanParseSetCompMode[] :=
  Module[{},
    $ParseModeState["SetComp"] = <|
      "IndependentVarlistIndex" -> False,
      "WithoutGridPointIndex" -> False,
      "UseTilePointIndex" -> False
    |>;
  ];

Protect[CleanParseSetCompMode];

(* Level 3: PrintComp sub-modes - DEPRECATED: Use InitializationsPhaseQ[], EquationsPhaseQ[] instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParsePrintCompMode[key_] :=
  Module[{keyName, subPhaseName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    subPhaseName = If[$ParseModeState["PrintComp", "SubPhase"] === None,
      "None",
      SymbolName[$ParseModeState["PrintComp", "SubPhase"]]
    ];
    Which[
      keyName === "Initializations", subPhaseName === "Initializations",
      keyName === "Equations", subPhaseName === "Equations",
      True, False
    ]
  ];

Protect[GetParsePrintCompMode];

(* Handle lists of rules, associations, and single rules *)
SetParsePrintCompMode[list_List] := SetParsePrintCompMode[Association[list]];
SetParsePrintCompMode[rule_Rule] := SetParsePrintCompMode[<|rule|>];

SetParsePrintCompMode[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "Initializations" && val === True, $ParseModeState["PrintComp", "SubPhase"] = k,
            keyName === "Equations" && val === True, $ParseModeState["PrintComp", "SubPhase"] = k
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParsePrintCompMode];

SetParsePrintCompModeAllToFalse[] :=
  Module[{},
    $ParseModeState["PrintComp", "SubPhase"] = None;
  ];

Protect[SetParsePrintCompModeAllToFalse];

CleanParsePrintCompMode[] :=
  Module[{},
    $ParseModeState["PrintComp", "SubPhase"] = None;
  ];

Protect[CleanParsePrintCompMode];

(* Level 4: Init modes - DEPRECATED: Use MainOutModeQ[], GetDerivsOrder[], etc. instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParsePrintCompInitMode[key_] :=
  Module[{keyName, modeName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    modeName = If[$ParseModeState["PrintComp", "Init", "Mode"] === None,
      "None",
      SymbolName[$ParseModeState["PrintComp", "Init", "Mode"]]
    ];
    Which[
      keyName === "MainOut", modeName === "MainOut",
      keyName === "MainIn", modeName === "MainIn",
      keyName === "Derivs", modeName === "Derivs",
      keyName === "MoreInOut", modeName === "MoreInOut",
      keyName === "Temp", modeName === "Temp",
      keyName === "DerivsOrder", $ParseModeState["PrintComp", "Init", "DerivsOrder"],
      keyName === "DerivsAccuracy", $ParseModeState["PrintComp", "Init", "DerivsAccuracy"],
      True, False
    ]
  ];

Protect[GetParsePrintCompInitMode];

(* Handle both rules (key -> val) and associations (<|key -> val|>) *)
SetParsePrintCompInitMode[rule_Rule] := SetParsePrintCompInitMode[<|rule|>];

SetParsePrintCompInitMode[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "MainOut" && val === True, $ParseModeState["PrintComp", "Init", "Mode"] = k,
            keyName === "MainIn" && val === True, $ParseModeState["PrintComp", "Init", "Mode"] = k,
            keyName === "Derivs" && val === True, $ParseModeState["PrintComp", "Init", "Mode"] = k,
            keyName === "MoreInOut" && val === True, $ParseModeState["PrintComp", "Init", "Mode"] = k,
            keyName === "Temp" && val === True, $ParseModeState["PrintComp", "Init", "Mode"] = k,
            keyName === "DerivsOrder", $ParseModeState["PrintComp", "Init", "DerivsOrder"] = val,
            keyName === "DerivsAccuracy", $ParseModeState["PrintComp", "Init", "DerivsAccuracy"] = val
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParsePrintCompInitMode];

CleanParsePrintCompInitMode[] :=
  Module[{},
    (* Preserve TensorType and StorageType when cleaning init mode *)
    $ParseModeState["PrintComp", "Init"] = <|
      "Mode" -> None,
      "DerivsOrder" -> 1,
      "DerivsAccuracy" -> 4,
      "TensorType" -> $ParseModeState["PrintComp", "Init", "TensorType"],
      "StorageType" -> $ParseModeState["PrintComp", "Init", "StorageType"]
    |>;
  ];

Protect[CleanParsePrintCompInitMode];

(* Level 5: Equation modes - DEPRECATED: Use NewVarModeQ[], MainEqnModeQ[], etc. instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParsePrintCompEQNMode[key_] :=
  Module[{keyName, modeName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    modeName = If[$ParseModeState["PrintComp", "Equation", "Mode"] === None,
      "None",
      SymbolName[$ParseModeState["PrintComp", "Equation", "Mode"]]
    ];
    Which[
      keyName === "NewVar", modeName === "NewVar",
      keyName === "Main", modeName === "Main",
      keyName === "AddToMain", modeName === "AddToMain",
      True, False
    ]
  ];

Protect[GetParsePrintCompEQNMode];

(* Handle both rules (key -> val) and associations (<|key -> val|>) *)
SetParsePrintCompEQNMode[rule_Rule] := SetParsePrintCompEQNMode[<|rule|>];

SetParsePrintCompEQNMode[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "NewVar" && val === True, $ParseModeState["PrintComp", "Equation", "Mode"] = k,
            keyName === "Main" && val === True, $ParseModeState["PrintComp", "Equation", "Mode"] = k,
            keyName === "AddToMain" && val === True, $ParseModeState["PrintComp", "Equation", "Mode"] = k
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParsePrintCompEQNMode];

CleanParsePrintCompEQNMode[] :=
  Module[{},
    $ParseModeState["PrintComp", "Equation", "Mode"] = None;
  ];

Protect[CleanParsePrintCompEQNMode];

(* Level 6: Tensor types - DEPRECATED: Use ScalTensorTypeQ[], GetTensorType[], etc. instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParsePrintCompInitTensorType[key_] :=
  Module[{keyName, typeName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    typeName = If[$ParseModeState["PrintComp", "Init", "TensorType"] === None,
      "None",
      SymbolName[$ParseModeState["PrintComp", "Init", "TensorType"]]
    ];
    Which[
      keyName === "Scal", typeName === "Scal",
      keyName === "Vect", typeName === "Vect",
      keyName === "Smat", typeName === "Smat",
      True, False
    ]
  ];

Protect[GetParsePrintCompInitTensorType];

(* Handle both rules (key -> val) and associations (<|key -> val|>) *)
SetParsePrintCompInitTensorType[rule_Rule] := SetParsePrintCompInitTensorType[<|rule|>];

SetParsePrintCompInitTensorType[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "Scal" && val === True, $ParseModeState["PrintComp", "Init", "TensorType"] = k,
            keyName === "Vect" && val === True, $ParseModeState["PrintComp", "Init", "TensorType"] = k,
            keyName === "Smat" && val === True, $ParseModeState["PrintComp", "Init", "TensorType"] = k
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParsePrintCompInitTensorType];

CleanParsePrintCompInitTensorType[] :=
  Module[{},
    $ParseModeState["PrintComp", "Init", "TensorType"] = None;
  ];

Protect[CleanParsePrintCompInitTensorType];

(* Level 7: Storage types - DEPRECATED: Use GFStorageTypeQ[], GetStorageType[], etc. instead *)
(* Compare by symbol name to handle symbols from different contexts *)
GetParsePrintCompInitStorageType[key_] :=
  Module[{keyName, typeName},
    keyName = If[Head[key] === Symbol, SymbolName[key], ToString[key]];
    typeName = If[$ParseModeState["PrintComp", "Init", "StorageType"] === None,
      "None",
      SymbolName[$ParseModeState["PrintComp", "Init", "StorageType"]]
    ];
    Which[
      keyName === "GF", typeName === "GF",
      keyName === "Tile", typeName === "Tile",
      True, False
    ]
  ];

Protect[GetParsePrintCompInitStorageType];

(* Handle both rules (key -> val) and associations (<|key -> val|>) *)
SetParsePrintCompInitStorageType[rule_Rule] := SetParsePrintCompInitStorageType[<|rule|>];

SetParsePrintCompInitStorageType[assoc_Association] :=
  Module[{},
    KeyValueMap[
      Function[{k, val},
        Module[{keyName = If[Head[k] === Symbol, SymbolName[k], ToString[k]]},
          Which[
            keyName === "GF" && val === True, $ParseModeState["PrintComp", "Init", "StorageType"] = k,
            keyName === "Tile" && val === True, $ParseModeState["PrintComp", "Init", "StorageType"] = k
          ]
        ]
      ],
      assoc
    ];
  ];

Protect[SetParsePrintCompInitStorageType];

CleanParsePrintCompInitStorageType[] :=
  Module[{},
    $ParseModeState["PrintComp", "Init", "StorageType"] = None;
  ];

Protect[CleanParsePrintCompInitStorageType];

End[];

EndPackage[];
