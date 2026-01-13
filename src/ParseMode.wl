(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`", {"Generato`Context`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`ParseMode`, {2024, 7, 06}"];
  System`Print["------------------------------------------------------------"];
];

(* Core Functions *)
WithMode::usage = "WithMode[settings, body] sets modes, evaluates body, then restores previous mode state. Settings use flat keys (e.g., \"Phase\" -> \"SetComp\").";

(* Phase Helpers *)
GetPhase::usage = "GetPhase[] returns current phase.";
SetPhase::usage = "SetPhase[phase] sets the current phase.";
InSetCompPhase::usage = "InSetCompPhase[] returns True if in SetComp phase.";
InPrintCompPhase::usage = "InPrintCompPhase[] returns True if in PrintComp phase.";

(* PrintComp Type Helpers *)
GetPrintCompType::usage = "GetPrintCompType[] returns current print comp type.";
SetPrintCompType::usage = "SetPrintCompType[type] sets the current print comp type.";
InInitializationsMode::usage = "InInitializationsMode[] returns True if in Initializations mode.";
InEquationsMode::usage = "InEquationsMode[] returns True if in Equations mode.";

(* Initializations Mode Helpers *)
GetInitializationsMode::usage = "GetInitializationsMode[] returns current initialization mode.";
SetInitializationsMode::usage = "SetInitializationsMode[mode] sets the current initialization mode.";
GetTensorType::usage = "GetTensorType[] returns current tensor type.";
SetTensorType::usage = "SetTensorType[type] sets the current tensor type.";
GetStorageType::usage = "GetStorageType[] returns current storage type.";
SetStorageType::usage = "SetStorageType[type] sets the current storage type.";
GetDerivsOrder::usage = "GetDerivsOrder[] returns derivative order.";
SetDerivsOrder::usage = "SetDerivsOrder[order] sets the derivative order.";
GetDerivsAccuracy::usage = "GetDerivsAccuracy[] returns derivative accuracy.";
SetDerivsAccuracy::usage = "SetDerivsAccuracy[accuracy] sets the derivative accuracy.";

(* Equations Mode Helper *)
GetEquationsMode::usage = "GetEquationsMode[] returns current equation mode.";
SetEquationsMode::usage = "SetEquationsMode[mode] sets the current equation mode.";

(* IndexOptions Helpers *)
GetIndependentVarlistIndex::usage = "GetIndependentVarlistIndex[] returns IndependentVarlistIndex setting.";
SetIndependentVarlistIndex::usage = "SetIndependentVarlistIndex[val] sets IndependentVarlistIndex.";
GetWithoutGridPointIndex::usage = "GetWithoutGridPointIndex[] returns WithoutGridPointIndex setting.";
SetWithoutGridPointIndex::usage = "SetWithoutGridPointIndex[val] sets WithoutGridPointIndex.";
GetUseTilePointIndex::usage = "GetUseTilePointIndex[] returns UseTilePointIndex setting.";
SetUseTilePointIndex::usage = "SetUseTilePointIndex[val] sets UseTilePointIndex.";

Begin["`Private`"];

(* Valid values for flat context keys *)
$ContextModeValidValues = <|
  "Phase" -> {None, "SetComp", "PrintComp"},
  "IndependentVarlistIndex" -> {True, False},
  "WithoutGridPointIndex" -> {True, False},
  "UseTilePointIndex" -> {True, False},
  "PrintCompType" -> {None, "Initializations", "Equations"},
  "InitializationsMode" -> {None, "MainOut", "MainIn", "Derivs", "Derivs1st", "Derivs2nd", "MoreInOut", "Temp"},
  "TensorType" -> {None, "Scal", "Vect", "Smat"},
  "StorageType" -> {None, "GF", "Tile"},
  "DerivsOrder" -> _Integer,
  "DerivsAccuracy" -> _Integer,
  "EquationsMode" -> {None, "Temp", "MainOut", "AddToMainOut"}
|>;


(* ========================================= *)
(* Core Functions *)
(* ========================================= *)

(* Validate value for flat context key *)
ValidateContextModeValue[key_String, value_] :=
  Module[{validValues},
    If[!KeyExistsQ[$ContextModeValidValues, key],
      (* No validation for this key - allow any value *)
      True
      ,
      validValues = $ContextModeValidValues[key];
      If[ListQ[validValues],
        MemberQ[validValues, value]
        ,
        (* It's a pattern (e.g., _Integer) - use pattern matching *)
        MatchQ[value, validValues]
      ]
    ]
  ];

(* Scoped mode settings with automatic restore *)
(* settings is a list of "FlatKey" -> value rules *)
SetAttributes[WithMode, HoldRest];

WithMode[settings_List, body_] :=
  Module[{flatKeys, savedValues},
    (* Extract flat keys from settings *)
    flatKeys = First /@ settings;
    (* Save current values for keys we're about to modify *)
    savedValues = $CurrentContext[#] & /@ flatKeys;
    (* Apply new settings *)
    Scan[
      Function[setting,
        $CurrentContext = Append[$CurrentContext, First[setting] -> Last[setting]]
      ],
      settings
    ];
    (* Restore after body evaluates *)
    WithCleanup[
      body,
      Do[
        $CurrentContext = Append[$CurrentContext, flatKeys[[i]] -> savedValues[[i]]],
        {i, Length[flatKeys]}
      ]
    ]
  ];

Protect[WithMode];

(* ========================================= *)
(* Convenience Helpers *)
(* ========================================= *)

(* Phase helpers - global versions only *)
GetPhase[] := $CurrentContext["Phase"];

InSetCompPhase[] := $CurrentContext["Phase"] === "SetComp";

InPrintCompPhase[] := $CurrentContext["Phase"] === "PrintComp";

(* Phase setter - global version with validation *)
SetPhase[phase_] :=
  Module[{},
    If[!ValidateContextModeValue["Phase", phase],
      Throw @ Message[SetPhase::EInvalidValue, phase]
    ];
    $CurrentContext = Append[$CurrentContext, "Phase" -> phase]
  ];

SetPhase::EInvalidValue = "Invalid phase value: `1`";

Protect[GetPhase];
Protect[InSetCompPhase];
Protect[InPrintCompPhase];
Protect[SetPhase];

(* PrintComp type helpers - global versions only *)
GetPrintCompType[] := $CurrentContext["PrintCompType"];

InInitializationsMode[] := $CurrentContext["PrintCompType"] === "Initializations";

InEquationsMode[] := $CurrentContext["PrintCompType"] === "Equations";

(* PrintCompType setter - global version with validation *)
SetPrintCompType[ptype_] :=
  Module[{},
    If[!ValidateContextModeValue["PrintCompType", ptype],
      Throw @ Message[SetPrintCompType::EInvalidValue, ptype]
    ];
    $CurrentContext = Append[$CurrentContext, "PrintCompType" -> ptype]
  ];

SetPrintCompType::EInvalidValue = "Invalid PrintCompType value: `1`";

Protect[GetPrintCompType];
Protect[InInitializationsMode];
Protect[InEquationsMode];
Protect[SetPrintCompType];

(* Initializations mode helpers - global versions only *)
GetInitializationsMode[] := $CurrentContext["InitializationsMode"];

GetTensorType[] := $CurrentContext["TensorType"];

GetStorageType[] := $CurrentContext["StorageType"];

GetDerivsOrder[] := $CurrentContext["DerivsOrder"];

GetDerivsAccuracy[] := $CurrentContext["DerivsAccuracy"];

(* Initializations mode setters - global versions with validation *)
SetInitializationsMode[mode_] :=
  Module[{},
    If[!ValidateContextModeValue["InitializationsMode", mode],
      Throw @ Message[SetInitializationsMode::EInvalidValue, mode]
    ];
    $CurrentContext = Append[$CurrentContext, "InitializationsMode" -> mode]
  ];

SetInitializationsMode::EInvalidValue = "Invalid InitializationsMode value: `1`";

SetTensorType[ttype_] :=
  Module[{},
    If[!ValidateContextModeValue["TensorType", ttype],
      Throw @ Message[SetTensorType::EInvalidValue, ttype]
    ];
    $CurrentContext = Append[$CurrentContext, "TensorType" -> ttype]
  ];

SetTensorType::EInvalidValue = "Invalid TensorType value: `1`";

SetStorageType[stype_] :=
  Module[{},
    If[!ValidateContextModeValue["StorageType", stype],
      Throw @ Message[SetStorageType::EInvalidValue, stype]
    ];
    $CurrentContext = Append[$CurrentContext, "StorageType" -> stype]
  ];

SetStorageType::EInvalidValue = "Invalid StorageType value: `1`";

SetDerivsOrder[order_] :=
  Module[{},
    If[!ValidateContextModeValue["DerivsOrder", order],
      Throw @ Message[SetDerivsOrder::EInvalidValue, order]
    ];
    $CurrentContext = Append[$CurrentContext, "DerivsOrder" -> order]
  ];

SetDerivsOrder::EInvalidValue = "Invalid DerivsOrder value: `1`";

SetDerivsAccuracy[accuracy_] :=
  Module[{},
    If[!ValidateContextModeValue["DerivsAccuracy", accuracy],
      Throw @ Message[SetDerivsAccuracy::EInvalidValue, accuracy]
    ];
    $CurrentContext = Append[$CurrentContext, "DerivsAccuracy" -> accuracy]
  ];

SetDerivsAccuracy::EInvalidValue = "Invalid DerivsAccuracy value: `1`";

Protect[GetInitializationsMode];
Protect[GetTensorType];
Protect[GetStorageType];
Protect[GetDerivsOrder];
Protect[GetDerivsAccuracy];
Protect[SetInitializationsMode];
Protect[SetTensorType];
Protect[SetStorageType];
Protect[SetDerivsOrder];
Protect[SetDerivsAccuracy];

(* Equations mode helper - global versions only *)
GetEquationsMode[] := $CurrentContext["EquationsMode"];

SetEquationsMode[mode_] :=
  Module[{},
    If[!ValidateContextModeValue["EquationsMode", mode],
      Throw @ Message[SetEquationsMode::EInvalidValue, mode]
    ];
    $CurrentContext = Append[$CurrentContext, "EquationsMode" -> mode]
  ];

SetEquationsMode::EInvalidValue = "Invalid EquationsMode value: `1`";

Protect[GetEquationsMode];
Protect[SetEquationsMode];

(* IndexOptions helpers - global versions only *)
GetIndependentVarlistIndex[] := $CurrentContext["IndependentVarlistIndex"];

GetWithoutGridPointIndex[] := $CurrentContext["WithoutGridPointIndex"];

GetUseTilePointIndex[] := $CurrentContext["UseTilePointIndex"];

(* IndexOptions setters - global versions with validation *)
SetIndependentVarlistIndex[val_] :=
  Module[{},
    If[!ValidateContextModeValue["IndependentVarlistIndex", val],
      Throw @ Message[SetIndependentVarlistIndex::EInvalidValue, val]
    ];
    $CurrentContext = Append[$CurrentContext, "IndependentVarlistIndex" -> val]
  ];

SetIndependentVarlistIndex::EInvalidValue = "Invalid IndependentVarlistIndex value: `1`";

SetWithoutGridPointIndex[val_] :=
  Module[{},
    If[!ValidateContextModeValue["WithoutGridPointIndex", val],
      Throw @ Message[SetWithoutGridPointIndex::EInvalidValue, val]
    ];
    $CurrentContext = Append[$CurrentContext, "WithoutGridPointIndex" -> val]
  ];

SetWithoutGridPointIndex::EInvalidValue = "Invalid WithoutGridPointIndex value: `1`";

SetUseTilePointIndex[val_] :=
  Module[{},
    If[!ValidateContextModeValue["UseTilePointIndex", val],
      Throw @ Message[SetUseTilePointIndex::EInvalidValue, val]
    ];
    $CurrentContext = Append[$CurrentContext, "UseTilePointIndex" -> val]
  ];

SetUseTilePointIndex::EInvalidValue = "Invalid UseTilePointIndex value: `1`";

Protect[GetIndependentVarlistIndex];
Protect[GetWithoutGridPointIndex];
Protect[GetUseTilePointIndex];
Protect[SetIndependentVarlistIndex];
Protect[SetWithoutGridPointIndex];
Protect[SetUseTilePointIndex];

End[];

EndPackage[];
