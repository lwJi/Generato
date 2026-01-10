(* ::Package:: *)

(* ParseModeTests.wl *)
(* Unit tests for Generato`ParseMode module *)

If[Environment["QUIET"] =!= "1", Print["Loading ParseModeTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: ParseModeState structure *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    InitializeModeState[];
    AssociationQ[Generato`ParseMode`Private`$ParseModeState],
    True,
    TestID -> "ParseModeState-IsAssociation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    InitializeModeState[];
    KeyExistsQ[Generato`ParseMode`Private`$ParseModeState, "Phase"],
    True,
    TestID -> "ParseModeState-HasPhaseKey"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    InitializeModeState[];
    KeyExistsQ[Generato`ParseMode`Private`$ParseModeState, "SetComp"],
    True,
    TestID -> "ParseModeState-HasSetCompKey"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    InitializeModeState[];
    KeyExistsQ[Generato`ParseMode`Private`$ParseModeState, "PrintComp"],
    True,
    TestID -> "ParseModeState-HasPrintCompKey"
  ]
];

(* ========================================= *)
(* Test: ParseMode (Level 1) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    GetParseMode[SetComp],
    True,
    TestID -> "ParseMode-SetComp-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|PrintComp -> True|>];
    GetParseMode[PrintComp],
    True,
    TestID -> "ParseMode-PrintComp-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    GetParseMode[NonExistentKey],
    False,
    TestID -> "ParseMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True, PrintComp -> True|>];
    SetParseModeAllToFalse[];
    {GetParseMode[SetComp], GetParseMode[PrintComp]},
    {False, False},
    TestID -> "ParseMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: Mode Mutual Exclusivity *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    SetParseMode[<|PrintComp -> True|>];
    (* After setting PrintComp, SetComp should be False *)
    {GetParseMode[SetComp], GetParseMode[PrintComp]},
    {False, True},
    TestID -> "ParseMode-MutualExclusivity"
  ]
];

(* ========================================= *)
(* Test: ParseSetCompMode (Level 2) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|IndependentVarlistIndex -> True|>];
    GetParseSetCompMode[IndependentVarlistIndex],
    True,
    TestID -> "ParseSetCompMode-IndependentVarlistIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|WithoutGridPointIndex -> True|>];
    GetParseSetCompMode[WithoutGridPointIndex],
    True,
    TestID -> "ParseSetCompMode-WithoutGridPointIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|UseTilePointIndex -> True|>];
    GetParseSetCompMode[UseTilePointIndex],
    True,
    TestID -> "ParseSetCompMode-UseTilePointIndex-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    GetParseSetCompMode[NonExistentKey],
    False,
    TestID -> "ParseSetCompMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|IndependentVarlistIndex -> True, WithoutGridPointIndex -> True, UseTilePointIndex -> True|>];
    SetParseSetCompModeAllToFalse[];
    {GetParseSetCompMode[IndependentVarlistIndex], GetParseSetCompMode[WithoutGridPointIndex], GetParseSetCompMode[UseTilePointIndex]},
    {False, False, False},
    TestID -> "ParseSetCompMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompMode (Level 3) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True|>];
    GetParsePrintCompMode[Initializations],
    True,
    TestID -> "ParsePrintCompMode-Initializations-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Equations -> True|>];
    GetParsePrintCompMode[Equations],
    True,
    TestID -> "ParsePrintCompMode-Equations-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    GetParsePrintCompMode[NonExistentKey],
    False,
    TestID -> "ParsePrintCompMode-NonExistentKey-ReturnsFalse"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True, Equations -> True|>];
    SetParsePrintCompModeAllToFalse[];
    {GetParsePrintCompMode[Initializations], GetParsePrintCompMode[Equations]},
    {False, False},
    TestID -> "ParsePrintCompMode-SetAllToFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitMode (Level 4) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|MainOut -> True|>];
    GetParsePrintCompInitMode[MainOut],
    True,
    TestID -> "ParsePrintCompInitMode-MainOut-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|DerivsOrder -> 2|>];
    GetParsePrintCompInitMode[DerivsOrder],
    2,
    TestID -> "ParsePrintCompInitMode-DerivsOrder"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|DerivsAccuracy -> 8|>];
    GetParsePrintCompInitMode[DerivsAccuracy],
    8,
    TestID -> "ParsePrintCompInitMode-DerivsAccuracy"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    GetParsePrintCompInitMode[NonExistentKey],
    False,
    TestID -> "ParsePrintCompInitMode-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompEQNMode (Level 5) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|NewVar -> True|>];
    GetParsePrintCompEQNMode[NewVar],
    True,
    TestID -> "ParsePrintCompEQNMode-NewVar"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|Main -> True|>];
    GetParsePrintCompEQNMode[Main],
    True,
    TestID -> "ParsePrintCompEQNMode-Main"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    GetParsePrintCompEQNMode[NonExistentKey],
    False,
    TestID -> "ParsePrintCompEQNMode-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitTensorType (Level 6) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|Scal -> True|>];
    GetParsePrintCompInitTensorType[Scal],
    True,
    TestID -> "ParsePrintCompInitTensorType-Scal"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|Vect -> True|>];
    GetParsePrintCompInitTensorType[Vect],
    True,
    TestID -> "ParsePrintCompInitTensorType-Vect"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    GetParsePrintCompInitTensorType[NonExistentKey],
    False,
    TestID -> "ParsePrintCompInitTensorType-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: ParsePrintCompInitStorageType (Level 7) *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|GF -> True|>];
    GetParsePrintCompInitStorageType[GF],
    True,
    TestID -> "ParsePrintCompInitStorageType-GF"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|Tile -> True|>];
    GetParsePrintCompInitStorageType[Tile],
    True,
    TestID -> "ParsePrintCompInitStorageType-Tile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    GetParsePrintCompInitStorageType[NonExistentKey],
    False,
    TestID -> "ParsePrintCompInitStorageType-NonExistentKey-ReturnsFalse"
  ]
];

(* ========================================= *)
(* Test: New Cleaner API Functions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    SetCompPhaseQ[],
    True,
    TestID -> "NewAPI-SetCompPhaseQ-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|PrintComp -> True|>];
    PrintCompPhaseQ[],
    True,
    TestID -> "NewAPI-PrintCompPhaseQ-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    GetCurrentPhase[],
    SetComp,
    TestID -> "NewAPI-GetCurrentPhase-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParseSetCompMode[];
    SetParseSetCompMode[<|IndependentVarlistIndex -> True|>];
    IndependentVarlistIndexQ[],
    True,
    TestID -> "NewAPI-IndependentVarlistIndexQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True|>];
    InitializationsPhaseQ[],
    True,
    TestID -> "NewAPI-InitializationsPhaseQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Equations -> True|>];
    EquationsPhaseQ[],
    True,
    TestID -> "NewAPI-EquationsPhaseQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|MainOut -> True|>];
    MainOutModeQ[],
    True,
    TestID -> "NewAPI-MainOutModeQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|Derivs -> True|>];
    DerivsModeQ[],
    True,
    TestID -> "NewAPI-DerivsModeQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|DerivsOrder -> 2|>];
    GetDerivsOrder[],
    2,
    TestID -> "NewAPI-GetDerivsOrder"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|DerivsAccuracy -> 6|>];
    GetDerivsAccuracy[],
    6,
    TestID -> "NewAPI-GetDerivsAccuracy"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|Scal -> True|>];
    ScalTensorTypeQ[],
    True,
    TestID -> "NewAPI-ScalTensorTypeQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|Vect -> True|>];
    GetTensorType[],
    Vect,
    TestID -> "NewAPI-GetTensorType"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|GF -> True|>];
    GFStorageTypeQ[],
    True,
    TestID -> "NewAPI-GFStorageTypeQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|Tile -> True|>];
    GetStorageType[],
    Tile,
    TestID -> "NewAPI-GetStorageType"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|NewVar -> True|>];
    NewVarModeQ[],
    True,
    TestID -> "NewAPI-NewVarModeQ"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|Main -> True|>];
    GetEqnMode[],
    Main,
    TestID -> "NewAPI-GetEqnMode"
  ]
];

(* ========================================= *)
(* Test: Legacy and New API Equivalence *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    CleanParseMode[];
    SetParseMode[<|SetComp -> True|>];
    GetParseMode[SetComp] === SetCompPhaseQ[],
    True,
    TestID -> "Equivalence-ParseMode-SetComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompMode[];
    SetParsePrintCompMode[<|Initializations -> True|>];
    GetParsePrintCompMode[Initializations] === InitializationsPhaseQ[],
    True,
    TestID -> "Equivalence-ParsePrintCompMode-Initializations"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitMode[];
    SetParsePrintCompInitMode[<|MainOut -> True|>];
    GetParsePrintCompInitMode[MainOut] === MainOutModeQ[],
    True,
    TestID -> "Equivalence-ParsePrintCompInitMode-MainOut"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitTensorType[];
    SetParsePrintCompInitTensorType[<|Scal -> True|>];
    GetParsePrintCompInitTensorType[Scal] === ScalTensorTypeQ[],
    True,
    TestID -> "Equivalence-ParsePrintCompInitTensorType-Scal"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompInitStorageType[];
    SetParsePrintCompInitStorageType[<|GF -> True|>];
    GetParsePrintCompInitStorageType[GF] === GFStorageTypeQ[],
    True,
    TestID -> "Equivalence-ParsePrintCompInitStorageType-GF"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    CleanParsePrintCompEQNMode[];
    SetParsePrintCompEQNMode[<|NewVar -> True|>];
    GetParsePrintCompEQNMode[NewVar] === NewVarModeQ[],
    True,
    TestID -> "Equivalence-ParsePrintCompEQNMode-NewVar"
  ]
];

(* ========================================= *)
(* Test: ResetModeState *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    SetParseMode[<|SetComp -> True|>];
    SetParsePrintCompMode[<|Equations -> True|>];
    ResetModeState[];
    {GetCurrentPhase[], GetCurrentSubPhase[]},
    {None, None},
    TestID -> "ResetModeState-ClearsAll"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

CleanParseMode[];
CleanParseSetCompMode[];
CleanParsePrintCompMode[];
CleanParsePrintCompInitMode[];
CleanParsePrintCompEQNMode[];
CleanParsePrintCompInitTensorType[];
CleanParsePrintCompInitStorageType[];

If[Environment["QUIET"] =!= "1", Print["ParseModeTests.wl completed."]];
