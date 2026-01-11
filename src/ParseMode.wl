(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`ParseMode`, {2024, 7, 06}"];
  System`Print["------------------------------------------------------------"];
];

(* Core Functions *)
GetMode::usage = "GetMode[path...] returns the mode value at the specified path.";
SetMode::usage = "SetMode[path... -> value] sets the mode value at the specified path.";
ResetMode::usage = "ResetMode[path...] resets mode(s) to default values.";
SetModes::usage = "SetModes[{path1 -> val1, path2 -> val2, ...}] sets multiple mode values at once.";
WithMode::usage = "WithMode[settings, body] sets modes, evaluates body, then restores previous mode state.";

(* Phase Helpers *)
InSetCompPhase::usage = "InSetCompPhase[] returns True if in SetComp phase.";
InPrintCompPhase::usage = "InPrintCompPhase[] returns True if in PrintComp phase.";

(* PrintComp Type Helpers *)
InInitMode::usage = "InInitMode[] returns True if in Initializations mode.";
InEqnMode::usage = "InEqnMode[] returns True if in Equations mode.";

(* Init Mode Helpers *)
GetInitMode::usage = "GetInitMode[] returns current initialization mode.";
GetTensorType::usage = "GetTensorType[] returns current tensor type.";
GetStorageType::usage = "GetStorageType[] returns current storage type.";
GetDerivsOrder::usage = "GetDerivsOrder[] returns derivative order.";
GetDerivsAccuracy::usage = "GetDerivsAccuracy[] returns derivative accuracy.";

(* Eqn Mode Helper *)
GetEqnMode::usage = "GetEqnMode[] returns current equation mode.";

(* SetComp Helpers *)
GetIndependentVarlistIndex::usage = "GetIndependentVarlistIndex[] returns IndependentVarlistIndex setting.";
GetWithoutGridPointIndex::usage = "GetWithoutGridPointIndex[] returns WithoutGridPointIndex setting.";
GetUseTilePointIndex::usage = "GetUseTilePointIndex[] returns UseTilePointIndex setting.";

Begin["`Private`"];

(* Valid values for each path *)
$ModeValidValues = <|
  {"Phase"} -> {None, "SetComp", "PrintComp"},
  {"SetComp", "IndependentVarlistIndex"} -> {True, False},
  {"SetComp", "WithoutGridPointIndex"} -> {True, False},
  {"SetComp", "UseTilePointIndex"} -> {True, False},
  {"PrintComp", "Type"} -> {None, "Initializations", "Equations"},
  {"PrintComp", "Init", "Mode"} -> {None, "MainOut", "MainIn", "Derivs", "Derivs1st", "Derivs2nd", "MoreInOut", "Temp"},
  {"PrintComp", "Init", "TensorType"} -> {None, "Scal", "Vect", "Smat"},
  {"PrintComp", "Init", "StorageType"} -> {None, "GF", "Tile"},
  {"PrintComp", "Init", "DerivsOrder"} -> _Integer,
  {"PrintComp", "Init", "DerivsAccuracy"} -> _Integer,
  {"PrintComp", "Eqn", "Mode"} -> {None, "NewVar", "Main", "AddToMain"}
|>;

(* Default mode structure *)
$ModeDefaults = <|
  "Phase" -> None,
  "SetComp" -> <|
    "IndependentVarlistIndex" -> False,
    "WithoutGridPointIndex" -> False,
    "UseTilePointIndex" -> False
  |>,
  "PrintComp" -> <|
    "Type" -> None,
    "Init" -> <|
      "Mode" -> None,
      "TensorType" -> None,
      "StorageType" -> None,
      "DerivsOrder" -> 0,
      "DerivsAccuracy" -> 0
    |>,
    "Eqn" -> <|
      "Mode" -> None
    |>
  |>
|>;

(* The single mode state *)
$Mode = $ModeDefaults;

(* ========================================= *)
(* Core Functions *)
(* ========================================= *)

(* Get value at nested path *)
GetMode[] := $Mode;

GetMode[key_String] :=
  If[KeyExistsQ[$Mode, key], $Mode[key], None];

GetMode[keys__String] :=
  Module[{path = {keys}, result = $Mode},
    Do[
      If[AssociationQ[result] && KeyExistsQ[result, path[[i]]],
        result = result[[path[[i]]]]
        ,
        Return[None]
      ]
      ,
      {i, Length[path]}
    ];
    result
  ];

Protect[GetMode];

(* Validate value for path *)
ValidateModeValue[path_List, value_] :=
  Module[{validValues},
    If[!KeyExistsQ[$ModeValidValues, path],
      (* No validation for this path - allow any value *)
      True
      ,
      validValues = $ModeValidValues[path];
      If[ListQ[validValues],
        MemberQ[validValues, value]
        ,
        (* It's a pattern (e.g., _Integer) - use pattern matching *)
        MatchQ[value, validValues]
      ]
    ]
  ];

(* Set value at nested path with validation *)
SetMode[key_String -> value_] :=
  Module[{},
    If[!ValidateModeValue[{key}, value],
      Throw @ Message[SetMode::EInvalidValue, key, value]
    ];
    $Mode[key] = value
  ];

SetMode[keys__String, lastKey_String -> value_] :=
  Module[{path = {keys, lastKey}},
    If[!ValidateModeValue[path, value],
      Throw @ Message[SetMode::EInvalidValue, StringRiffle[path, "."], value]
    ];
    $Mode[[Sequence @@ Most[path], Last[path]]] = value
  ];

SetMode::EInvalidValue = "Invalid mode value for '`1`': `2`";

Protect[SetMode];

(* Reset mode to defaults *)
ResetMode[] :=
  Module[{},
    $Mode = $ModeDefaults
  ];

ResetMode[key_String] :=
  Module[{},
    If[KeyExistsQ[$ModeDefaults, key],
      $Mode[key] = $ModeDefaults[key]
    ]
  ];

ResetMode[keys__String] :=
  Module[{path = {keys}},
    $Mode[[Sequence @@ Most[path], Last[path]]] =
      $ModeDefaults[[Sequence @@ Most[path], Last[path]]]
  ];

Protect[ResetMode];

(* Batch set multiple modes *)
SetModes[rules_List] :=
  Scan[
    Function[rule,
      With[{path = First[rule], value = Last[rule]},
        If[Length[path] === 1,
          SetMode[First[path] -> value],
          SetMode[Sequence @@ Most[path], Last[path] -> value]
        ]
      ]
    ],
    rules
  ];

Protect[SetModes];

(* Scoped mode context with automatic restore *)
SetAttributes[WithMode, HoldRest];

WithMode[settings_List, body_] :=
  Module[{savedMode = $Mode},
    SetModes[settings];
    WithCleanup[
      body,
      $Mode = savedMode
    ]
  ];

Protect[WithMode];

(* ========================================= *)
(* Convenience Helpers *)
(* ========================================= *)

(* Phase helpers *)
InSetCompPhase[] := GetMode["Phase"] === "SetComp";
InPrintCompPhase[] := GetMode["Phase"] === "PrintComp";

Protect[InSetCompPhase];
Protect[InPrintCompPhase];

(* PrintComp type helpers *)
InInitMode[] := GetMode["PrintComp", "Type"] === "Initializations";
InEqnMode[] := GetMode["PrintComp", "Type"] === "Equations";

Protect[InInitMode];
Protect[InEqnMode];

(* Init mode helpers *)
GetInitMode[] := GetMode["PrintComp", "Init", "Mode"];
GetTensorType[] := GetMode["PrintComp", "Init", "TensorType"];
GetStorageType[] := GetMode["PrintComp", "Init", "StorageType"];
GetDerivsOrder[] := GetMode["PrintComp", "Init", "DerivsOrder"];
GetDerivsAccuracy[] := GetMode["PrintComp", "Init", "DerivsAccuracy"];

Protect[GetInitMode];
Protect[GetTensorType];
Protect[GetStorageType];
Protect[GetDerivsOrder];
Protect[GetDerivsAccuracy];

(* Eqn mode helper *)
GetEqnMode[] := GetMode["PrintComp", "Eqn", "Mode"];

Protect[GetEqnMode];

(* SetComp helpers *)
GetIndependentVarlistIndex[] := GetMode["SetComp", "IndependentVarlistIndex"];
GetWithoutGridPointIndex[] := GetMode["SetComp", "WithoutGridPointIndex"];
GetUseTilePointIndex[] := GetMode["SetComp", "UseTilePointIndex"];

Protect[GetIndependentVarlistIndex];
Protect[GetWithoutGridPointIndex];
Protect[GetUseTilePointIndex];

End[];

EndPackage[];
