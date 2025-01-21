(* ::Package:: *)

(* (c) Liwei Ji, 07/2024 *)

BeginPackage["Generato`ParseMode`"];

Print["------------------------------------------------------------"];

Print["Package Generato`ParseMode`, {2024, 7, 06}"];

Print["------------------------------------------------------------"];

GetParseMode::usage = "GetParseMode[key] returns the mode correspond to the key";

SetParseMode::usage = "SetParseMode[key] add/update the mode correspond to the key";

SetParseModeAllToFalse::usage = "SetParseModeAllToFalse[] set all the modes to false";

SetComp::usage = "ParseMode option.";

PrintComp::usage = "ParseMode option.";

Protect[SetComp];

Protect[PrintComp];

GetParseSetCompMode::usage = "GetParseSetCompMode[key] returns the mode correspond to the key";

SetParseSetCompMode::usage = "SetParseSetCompMode[key] add/update the mode correspond to the key";

SetParseSetCompModeAllToFalse::usage = "SetParseSetCompModeAllToFalse[] set all the modes to false";

IndependentVarlistIndex::usage = "ParseSetCompMode option.";

WithoutGridPointIndex::usage = "ParseSetCompMode option.";

Protect[IndependentVarlistIndex];

Protect[WithoutGridPointIndex];

GetParsePrintCompMode::usage = "GetParsePrintCompMode[key] returns the mode correspond to the key";

SetParsePrintCompMode::usage = "SetParsePrintCompMode[key] add/update the mode correspond to the key";

SetParsePrintCompModeAllToFalse::usage = "SetParsePrintCompModeAllToFalse[] set all the modes to false";

Initializations::usage = "ParsePrintCompMode option.";

Equations::usage = "ParsePrintCompMode option.";

Protect[Initializations];

Protect[Equations];

GetParsePrintCompInitMode::usage = "GetParsePrintCompInitMode[key] returns the mode correspond to the key";

SetParsePrintCompInitMode::usage = "SetParsePrintCompInitMode[key] add/update the mode correspond to the key";

CleanParsePrintCompInitMode::usage = "CleanParsePrintCompInitMode[] empty the association";

GetParsePrintCompEQNMode::usage = "GetParsePrintCompEQNMode[key] returns the mode correspond to the key";

SetParsePrintCompEQNMode::usage = "SetParsePrintCompEQNMode[key] add/update the mode correspond to the key";

CleanParsePrintCompEQNMode::usage = "CleanParsePrintCompEQNMode[] empty the association";

GetParsePrintCompInitTensorType::usage = "GetParsePrintCompInitTensorType[key] returns the mode correspond to the key";

SetParsePrintCompInitTensorType::usage = "SetParsePrintCompInitTensorType[key] add/update the mode correspond to the key";

CleanParsePrintCompInitTensorType::usage = "CleanParsePrintCompInitTensorType[] empty the association";

GetParsePrintCompInitStorageType::usage = "GetParsePrintCompInitStorageType[key] returns the mode correspond to the key";

SetParsePrintCompInitStorageType::usage = "SetParsePrintCompInitStorageType[key] add/update the mode correspond to the key";

CleanParsePrintCompInitStorageType::usage = "CleanParsePrintCompInitStorageType[] empty the association";

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
    AppendTo[$ParseSetCompModeAssociation, <|IndependentVarlistIndex -> False, WithoutGridPointIndex -> False|>]
  ];

Protect[SetParseSetCompModeAllToFalse];

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

End[];

EndPackage[];