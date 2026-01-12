(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`", {"Generato`Context`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`ParseMode`, {2024, 7, 06}"];
  System`Print["------------------------------------------------------------"];
];

(* Core Functions *)
WithMode::usage = "WithMode[settings, body] sets modes, evaluates body, then restores previous mode state.";

(* Phase Helpers *)
GetPhase::usage = "GetPhase[ctx] returns phase from context.\nGetPhase[] returns current phase from global mode.";
SetPhase::usage = "SetPhase[ctx, phase] returns new context with updated phase.";
InSetCompPhase::usage = "InSetCompPhase[ctx] returns True if context is in SetComp phase.\nInSetCompPhase[] returns True if in SetComp phase.";
InPrintCompPhase::usage = "InPrintCompPhase[ctx] returns True if context is in PrintComp phase.\nInPrintCompPhase[] returns True if in PrintComp phase.";

(* PrintComp Type Helpers *)
GetPrintCompType::usage = "GetPrintCompType[ctx] returns print comp type from context.\nGetPrintCompType[] returns current print comp type.";
SetPrintCompType::usage = "SetPrintCompType[ctx, type] returns new context with updated print comp type.";
InInitializationsMode::usage = "InInitializationsMode[ctx] returns True if context is in Initializations mode.\nInInitializationsMode[] returns True if in Initializations mode.";
InEquationsMode::usage = "InEquationsMode[ctx] returns True if context is in Equations mode.\nInEquationsMode[] returns True if in Equations mode.";

(* Initializations Mode Helpers *)
GetInitializationsMode::usage = "GetInitializationsMode[ctx] returns initialization mode from context.\nGetInitializationsMode[] returns current initialization mode.";
SetInitializationsMode::usage = "SetInitializationsMode[ctx, mode] returns new context with updated initialization mode.";
GetTensorType::usage = "GetTensorType[ctx] returns tensor type from context.\nGetTensorType[] returns current tensor type.";
SetTensorType::usage = "SetTensorType[ctx, type] returns new context with updated tensor type.";
GetStorageType::usage = "GetStorageType[ctx] returns storage type from context.\nGetStorageType[] returns current storage type.";
SetStorageType::usage = "SetStorageType[ctx, type] returns new context with updated storage type.";
GetDerivsOrder::usage = "GetDerivsOrder[ctx] returns derivative order from context.\nGetDerivsOrder[] returns derivative order.";
SetDerivsOrder::usage = "SetDerivsOrder[ctx, order] returns new context with updated derivative order.";
GetDerivsAccuracy::usage = "GetDerivsAccuracy[ctx] returns derivative accuracy from context.\nGetDerivsAccuracy[] returns derivative accuracy.";
SetDerivsAccuracy::usage = "SetDerivsAccuracy[ctx, accuracy] returns new context with updated derivative accuracy.";

(* Equations Mode Helper *)
GetEquationsMode::usage = "GetEquationsMode[ctx] returns equation mode from context.\nGetEquationsMode[] returns current equation mode.";
SetEquationsMode::usage = "SetEquationsMode[ctx, mode] returns new context with updated equation mode.";

(* IndexOptions Helpers *)
GetIndependentVarlistIndex::usage = "GetIndependentVarlistIndex[ctx] returns IndependentVarlistIndex from context.\nGetIndependentVarlistIndex[] returns IndependentVarlistIndex setting.";
SetIndependentVarlistIndex::usage = "SetIndependentVarlistIndex[ctx, val] returns new context with updated IndependentVarlistIndex.";
GetWithoutGridPointIndex::usage = "GetWithoutGridPointIndex[ctx] returns WithoutGridPointIndex from context.\nGetWithoutGridPointIndex[] returns WithoutGridPointIndex setting.";
SetWithoutGridPointIndex::usage = "SetWithoutGridPointIndex[ctx, val] returns new context with updated WithoutGridPointIndex.";
GetUseTilePointIndex::usage = "GetUseTilePointIndex[ctx] returns UseTilePointIndex from context.\nGetUseTilePointIndex[] returns UseTilePointIndex setting.";
SetUseTilePointIndex::usage = "SetUseTilePointIndex[ctx, val] returns new context with updated UseTilePointIndex.";

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

(* Map nested paths to flat context keys *)
PathToFlatKey[path_List] := Switch[path,
  {"Phase"}, "Phase",
  {"IndexOptions", "IndependentVarlistIndex"}, "IndependentVarlistIndex",
  {"IndexOptions", "WithoutGridPointIndex"}, "WithoutGridPointIndex",
  {"IndexOptions", "UseTilePointIndex"}, "UseTilePointIndex",
  {"PrintComp", "Type"}, "PrintCompType",
  {"PrintComp", "Initializations", "Mode"}, "InitializationsMode",
  {"PrintComp", "Initializations", "TensorType"}, "TensorType",
  {"PrintComp", "Initializations", "StorageType"}, "StorageType",
  {"PrintComp", "Initializations", "DerivsOrder"}, "DerivsOrder",
  {"PrintComp", "Initializations", "DerivsAccuracy"}, "DerivsAccuracy",
  {"PrintComp", "Equations", "Mode"}, "EquationsMode",
  _, Throw @ Message[PathToFlatKey::EUnknownPath, path]
];

PathToFlatKey::EUnknownPath = "Unknown nested path: `1`";

(* Scoped mode context with automatic restore - global mode version *)
(* Converts nested path settings to flat keys and applies to $CurrentContext *)
(* Only saves/restores the specific keys that are modified, preserving other state changes *)
SetAttributes[WithMode, HoldRest];

WithMode[settings_List, body_] :=
  Module[{flatKeys, savedValues},
    (* Convert nested paths to flat keys *)
    flatKeys = PathToFlatKey[First[#]] & /@ settings;
    (* Save only the values for keys we're about to modify *)
    savedValues = $CurrentContext[#] & /@ flatKeys;
    (* Apply the new settings *)
    Scan[
      Function[setting,
        With[{flatKey = PathToFlatKey[First[setting]], value = Last[setting]},
          $CurrentContext = SetCtx[$CurrentContext, flatKey, value]
        ]
      ],
      settings
    ];
    (* Restore only the specific keys that were modified *)
    WithCleanup[
      body,
      Do[
        $CurrentContext = SetCtx[$CurrentContext, flatKeys[[i]], savedValues[[i]]],
        {i, Length[flatKeys]}
      ]
    ]
  ];

(* Context-aware WithMode - applies flat key-value settings to context *)
(* settings is a list of {key, value} pairs or key -> value rules *)
WithMode[ctx_Association, settings_List, body_] :=
  Module[{newCtx},
    newCtx = Fold[
      If[Head[#2] === Rule,
        SetCtx[#1, First[#2], Last[#2]],
        SetCtx[#1, #2[[1]], #2[[2]]]
      ] &,
      ctx,
      settings
    ];
    WithContext[newCtx, body]
  ];

Protect[WithMode];

(* ========================================= *)
(* Convenience Helpers *)
(* ========================================= *)

(* Phase helpers - context-aware versions *)
GetPhase[ctx_Association] := GetCtx[ctx, "Phase"];
GetPhase[] := $CurrentContext["Phase"];

InSetCompPhase[ctx_Association] := GetPhase[ctx] === "SetComp";
InSetCompPhase[] := $CurrentContext["Phase"] === "SetComp";

InPrintCompPhase[ctx_Association] := GetPhase[ctx] === "PrintComp";
InPrintCompPhase[] := $CurrentContext["Phase"] === "PrintComp";

(* Phase setters - context-aware versions return new context *)
SetPhase[ctx_Association, phase_] :=
  Module[{},
    If[!ValidateContextModeValue["Phase", phase],
      Throw @ Message[SetPhase::EInvalidValue, phase]
    ];
    SetCtx[ctx, "Phase", phase]
  ];

SetPhase::EInvalidValue = "Invalid phase value: `1`";

Protect[GetPhase];
Protect[InSetCompPhase];
Protect[InPrintCompPhase];
Protect[SetPhase];

(* PrintComp type helpers - context-aware versions *)
GetPrintCompType[ctx_Association] := GetCtx[ctx, "PrintCompType"];
GetPrintCompType[] := $CurrentContext["PrintCompType"];

InInitializationsMode[ctx_Association] := GetPrintCompType[ctx] === "Initializations";
InInitializationsMode[] := $CurrentContext["PrintCompType"] === "Initializations";

InEquationsMode[ctx_Association] := GetPrintCompType[ctx] === "Equations";
InEquationsMode[] := $CurrentContext["PrintCompType"] === "Equations";

(* PrintCompType setter - context-aware version *)
SetPrintCompType[ctx_Association, ptype_] :=
  Module[{},
    If[!ValidateContextModeValue["PrintCompType", ptype],
      Throw @ Message[SetPrintCompType::EInvalidValue, ptype]
    ];
    SetCtx[ctx, "PrintCompType", ptype]
  ];

SetPrintCompType::EInvalidValue = "Invalid PrintCompType value: `1`";

Protect[GetPrintCompType];
Protect[InInitializationsMode];
Protect[InEquationsMode];
Protect[SetPrintCompType];

(* Initializations mode helpers - context-aware versions *)
GetInitializationsMode[ctx_Association] := GetCtx[ctx, "InitializationsMode"];
GetInitializationsMode[] := $CurrentContext["InitializationsMode"];

GetTensorType[ctx_Association] := GetCtx[ctx, "TensorType"];
GetTensorType[] := $CurrentContext["TensorType"];

GetStorageType[ctx_Association] := GetCtx[ctx, "StorageType"];
GetStorageType[] := $CurrentContext["StorageType"];

GetDerivsOrder[ctx_Association] := GetCtx[ctx, "DerivsOrder"];
GetDerivsOrder[] := $CurrentContext["DerivsOrder"];

GetDerivsAccuracy[ctx_Association] := GetCtx[ctx, "DerivsAccuracy"];
GetDerivsAccuracy[] := $CurrentContext["DerivsAccuracy"];

(* Initializations mode setters - context-aware versions *)
SetInitializationsMode[ctx_Association, mode_] :=
  Module[{},
    If[!ValidateContextModeValue["InitializationsMode", mode],
      Throw @ Message[SetInitializationsMode::EInvalidValue, mode]
    ];
    SetCtx[ctx, "InitializationsMode", mode]
  ];

SetInitializationsMode::EInvalidValue = "Invalid InitializationsMode value: `1`";

SetTensorType[ctx_Association, ttype_] :=
  Module[{},
    If[!ValidateContextModeValue["TensorType", ttype],
      Throw @ Message[SetTensorType::EInvalidValue, ttype]
    ];
    SetCtx[ctx, "TensorType", ttype]
  ];

SetTensorType::EInvalidValue = "Invalid TensorType value: `1`";

SetStorageType[ctx_Association, stype_] :=
  Module[{},
    If[!ValidateContextModeValue["StorageType", stype],
      Throw @ Message[SetStorageType::EInvalidValue, stype]
    ];
    SetCtx[ctx, "StorageType", stype]
  ];

SetStorageType::EInvalidValue = "Invalid StorageType value: `1`";

SetDerivsOrder[ctx_Association, order_] :=
  Module[{},
    If[!ValidateContextModeValue["DerivsOrder", order],
      Throw @ Message[SetDerivsOrder::EInvalidValue, order]
    ];
    SetCtx[ctx, "DerivsOrder", order]
  ];

SetDerivsOrder::EInvalidValue = "Invalid DerivsOrder value: `1`";

SetDerivsAccuracy[ctx_Association, accuracy_] :=
  Module[{},
    If[!ValidateContextModeValue["DerivsAccuracy", accuracy],
      Throw @ Message[SetDerivsAccuracy::EInvalidValue, accuracy]
    ];
    SetCtx[ctx, "DerivsAccuracy", accuracy]
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

(* Equations mode helper - context-aware versions *)
GetEquationsMode[ctx_Association] := GetCtx[ctx, "EquationsMode"];
GetEquationsMode[] := $CurrentContext["EquationsMode"];

SetEquationsMode[ctx_Association, mode_] :=
  Module[{},
    If[!ValidateContextModeValue["EquationsMode", mode],
      Throw @ Message[SetEquationsMode::EInvalidValue, mode]
    ];
    SetCtx[ctx, "EquationsMode", mode]
  ];

SetEquationsMode::EInvalidValue = "Invalid EquationsMode value: `1`";

Protect[GetEquationsMode];
Protect[SetEquationsMode];

(* IndexOptions helpers - context-aware versions *)
GetIndependentVarlistIndex[ctx_Association] := GetCtx[ctx, "IndependentVarlistIndex"];
GetIndependentVarlistIndex[] := $CurrentContext["IndependentVarlistIndex"];

GetWithoutGridPointIndex[ctx_Association] := GetCtx[ctx, "WithoutGridPointIndex"];
GetWithoutGridPointIndex[] := $CurrentContext["WithoutGridPointIndex"];

GetUseTilePointIndex[ctx_Association] := GetCtx[ctx, "UseTilePointIndex"];
GetUseTilePointIndex[] := $CurrentContext["UseTilePointIndex"];

(* IndexOptions setters - context-aware versions *)
SetIndependentVarlistIndex[ctx_Association, val_] :=
  Module[{},
    If[!ValidateContextModeValue["IndependentVarlistIndex", val],
      Throw @ Message[SetIndependentVarlistIndex::EInvalidValue, val]
    ];
    SetCtx[ctx, "IndependentVarlistIndex", val]
  ];

SetIndependentVarlistIndex::EInvalidValue = "Invalid IndependentVarlistIndex value: `1`";

SetWithoutGridPointIndex[ctx_Association, val_] :=
  Module[{},
    If[!ValidateContextModeValue["WithoutGridPointIndex", val],
      Throw @ Message[SetWithoutGridPointIndex::EInvalidValue, val]
    ];
    SetCtx[ctx, "WithoutGridPointIndex", val]
  ];

SetWithoutGridPointIndex::EInvalidValue = "Invalid WithoutGridPointIndex value: `1`";

SetUseTilePointIndex[ctx_Association, val_] :=
  Module[{},
    If[!ValidateContextModeValue["UseTilePointIndex", val],
      Throw @ Message[SetUseTilePointIndex::EInvalidValue, val]
    ];
    SetCtx[ctx, "UseTilePointIndex", val]
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
