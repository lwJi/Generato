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

Begin["`Private`"];

(* Data *)

$ParseModeAssociation = <||>;

$ParseSetCompModeAssociation = <||>;

$ParsePrintCompModeAssociation = <||>;

$ParsePrintCompInitModeAssociation = <||>;

$ParsePrintCompEQNModeAssociation = <||>;

$ParsePrintCompInitTensorTypeAssociation = <||>;

$ParsePrintCompInitStorageTypeAssociation = <||>;

(* Function *)

GetParseMode[key_] :=
  Return[
    If[KeyExistsQ[$ParseModeAssociation, key],
      $ParseModeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParseMode];

SetParseMode[assoc_] :=
  Module[{},
    AppendTo[$ParseModeAssociation, assoc]
  ];

Protect[SetParseMode];

SetParseModeAllToFalse[] :=
  Module[{},
    AppendTo[$ParseModeAssociation, <|SetComp -> False, PrintComp -> False|>]
  ];

Protect[SetParseModeAllToFalse];

CleanParseMode[] :=
  Module[{},
    $ParseModeAssociation = <||>
  ];

Protect[CleanParseMode];

GetParseSetCompMode[key_] :=
  Return[
    If[KeyExistsQ[$ParseSetCompModeAssociation, key],
      $ParseSetCompModeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParseSetCompMode];

SetParseSetCompMode[assoc_] :=
  Module[{},
    AppendTo[$ParseSetCompModeAssociation, assoc]
  ];

Protect[SetParseSetCompMode];

SetParseSetCompModeAllToFalse[] :=
  Module[{},
    AppendTo[$ParseSetCompModeAssociation, <|IndependentVarlistIndex -> False, WithoutGridPointIndex -> False, UseTilePointIndex -> False|>]
  ];

Protect[SetParseSetCompModeAllToFalse];

CleanParseSetCompMode[] :=
  Module[{},
    $ParseSetCompModeAssociation = <||>
  ];

Protect[CleanParseSetCompMode];

GetParsePrintCompMode[key_] :=
  Return[
    If[KeyExistsQ[$ParsePrintCompModeAssociation, key],
      $ParsePrintCompModeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParsePrintCompMode];

SetParsePrintCompMode[assoc_] :=
  Module[{},
    AppendTo[$ParsePrintCompModeAssociation, assoc]
  ];

Protect[SetParsePrintCompMode];

SetParsePrintCompModeAllToFalse[] :=
  Module[{},
    AppendTo[$ParsePrintCompModeAssociation, <|Initializations -> False, Equations -> False|>]
  ];

Protect[SetParsePrintCompModeAllToFalse];

CleanParsePrintCompMode[] :=
  Module[{},
    $ParsePrintCompModeAssociation = <||>
  ];

Protect[CleanParsePrintCompMode];

GetParsePrintCompInitMode[key_] :=
  Return[
    If[KeyExistsQ[$ParsePrintCompInitModeAssociation, key],
      $ParsePrintCompInitModeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParsePrintCompInitMode];

SetParsePrintCompInitMode[assoc_] :=
  Module[{},
    AppendTo[$ParsePrintCompInitModeAssociation, assoc]
  ];

Protect[SetParsePrintCompInitMode];

CleanParsePrintCompInitMode[] :=
  Module[{},
    $ParsePrintCompInitModeAssociation = <||>
  ];

Protect[CleanParsePrintCompInitMode];

GetParsePrintCompEQNMode[key_] :=
  Return[
    If[KeyExistsQ[$ParsePrintCompEQNModeAssociation, key],
      $ParsePrintCompEQNModeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParsePrintCompEQNMode];

SetParsePrintCompEQNMode[assoc_] :=
  Module[{},
    AppendTo[$ParsePrintCompEQNModeAssociation, assoc]
  ];

Protect[SetParsePrintCompEQNMode];

CleanParsePrintCompEQNMode[] :=
  Module[{},
    $ParsePrintCompEQNModeAssociation = <||>
  ];

Protect[CleanParsePrintCompEQNMode];

GetParsePrintCompInitTensorType[key_] :=
  Return[
    If[KeyExistsQ[$ParsePrintCompInitTensorTypeAssociation, key],
      $ParsePrintCompInitTensorTypeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParsePrintCompInitTensorType];

SetParsePrintCompInitTensorType[assoc_] :=
  Module[{},
    AppendTo[$ParsePrintCompInitTensorTypeAssociation, assoc]
  ];

Protect[SetParsePrintCompInitTensorType];

CleanParsePrintCompInitTensorType[] :=
  Module[{},
    $ParsePrintCompInitTensorTypeAssociation = <||>
  ];

Protect[CleanParsePrintCompInitTensorType];

GetParsePrintCompInitStorageType[key_] :=
  Return[
    If[KeyExistsQ[$ParsePrintCompInitStorageTypeAssociation, key],
      $ParsePrintCompInitStorageTypeAssociation[key]
      ,
      False
    ]
  ];

Protect[GetParsePrintCompInitStorageType];

SetParsePrintCompInitStorageType[assoc_] :=
  Module[{},
    AppendTo[$ParsePrintCompInitStorageTypeAssociation, assoc]
  ];

Protect[SetParsePrintCompInitStorageType];

CleanParsePrintCompInitStorageType[] :=
  Module[{},
    $ParsePrintCompInitStorageTypeAssociation = <||>
  ];

Protect[CleanParsePrintCompInitStorageType];

End[];

EndPackage[];
