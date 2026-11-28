(* ::Package:: *)

(* ContextTests.wl *)
(* Unit tests for Generato`Context module: the user configuration. Each key has a default, a getter and a setter *)
(* that takes the values of the key and stops at any other; the context holds these keys and nothing else *)

If[Environment["QUIET"] =!= "1", Print["Loading ContextTests.wl..."]];

(* Load Generato which includes Context *)
If[!MemberQ[$Packages, "Generato`Context`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

(*
  The keys, each as {key, getter, setter, default, another value it takes, a
  value it does not take}, in the order they are declared in Context.wl.
*)
contextTestKeys = {
  {"CheckInputEquations", GetCheckInputEquations, SetCheckInputEquations, False, True, "invalid"},
  {"PVerbose", GetPVerbose, SetPVerbose, False, True, 1},
  {"QuietMode", GetQuietMode, SetQuietMode, Environment["QUIET"] === "1", Environment["QUIET"] =!= "1", "1"},
  {"PrintDate", GetPrintDate, SetPrintDate, True, False, "no"},
  {"PrintHeaderMacro", GetPrintHeaderMacro, SetPrintHeaderMacro, True, False, None},
  {"GridPointIndex", GetGridPointIndex, SetGridPointIndex, "", "[[ijk]]", 123},
  {"TilePointIndex", GetTilePointIndex, SetTilePointIndex, "", "(ti,tj,tk)", contextTestSymbol},
  {"SuffixUnprotected", GetSuffixUnprotected, SetSuffixUnprotected, "$Upt", "_test", 0},
  {"Project", GetProject, SetProject, "TEST", "TestProject", True},
  {"SimplifyEquation", GetSimplifyEquation, SetSimplifyEquation, True, False, "yes"},
  {"UseLetterForTensorComponent", GetUseLetterForTensorComponent, SetUseLetterForTensorComponent, False, True, 0},
  {"TempVariableType", GetTempVariableType, SetTempVariableType, "double", "CCTK_REAL", 123},
  {"InterfaceWithNonCoordBasis", GetInterfaceWithNonCoordBasis, SetInterfaceWithNonCoordBasis, False, True, "True"},
  {"PrefixDt", GetPrefixDt, SetPrefixDt, "dt", "ddt", False}
};

(* ========================================= *)
(* Test: the context is the user configuration *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* the context holds the keys of the user configuration, each at its default, and no other key: *)
    (* no mode, no output file, no main content and no registry *)
    ResetContext[];
    GetConfiguration[],
    Association[#[[1]] -> #[[4]]& /@ contextTestKeys],
    TestID -> "Context-DefaultsOfEveryKey"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the configuration is the context, with its keys in the order they are declared *)
    ResetContext[];
    {GetConfiguration[] === $CurrentContext, Keys[GetConfiguration[]]},
    {True, contextTestKeys[[All, 1]]},
    TestID -> "Context-ConfigurationIsTheContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a key is a getter and a setter of the package, and the package has no accessor that is not of a key *)
    Complement[
      Last[StringSplit[#, "`"]]& /@ Join[Names["Generato`Context`Get*"], Names["Generato`Context`Set*"]],
      SymbolName /@ Flatten[contextTestKeys[[All, 2 ;; 3]]],
      {"GetConfiguration"}
    ],
    {},
    TestID -> "Context-NoAccessorButThoseOfTheKeys"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the usage of a getter and of a setter is written from the declaration of the key *)
    {GetGridPointIndex::usage, SetGridPointIndex::usage, SetPVerbose::usage},
    {
      "GetGridPointIndex[] returns the index appended to a grid variable's C name.",
      "SetGridPointIndex[value] sets the index appended to a grid variable's C name. value is a String.",
      "SetPVerbose[value] sets whether progress messages are printed. value is True or False."
    },
    TestID -> "Context-UsageFromTheDeclaration"
  ]
];

(* ========================================= *)
(* Test: each key - its setter takes a value of the key, and stops at any other *)
(* ========================================= *)

Scan[
  Function[case,
    With[
      {
        key = case[[1]], getter = case[[2]], setter = case[[3]], default = case[[4]], other = case[[5]], invalid = case[[6]],
        idSet = "Context-" <> case[[1]] <> "-SetAndGet", idInvalid = "Context-" <> case[[1]] <> "-InvalidValue-Stops"
      },
      AppendTo[$AllTests,
        VerificationTest[
          (* the setter sets the key, and the getter reads it *)
          ResetContext[];
          {getter[], setter[other]; getter[], GetConfiguration[][key], setter[default]; getter[]},
          {default, other, other, default},
          TestID -> idSet
        ]
      ];
      AppendTo[$AllTests,
        VerificationTest[
          (* a value the key does not take stops the run, and the key keeps its value *)
          ResetContext[];
          setter[other];
          {Catch[setter[invalid]; "NoThrow", GeneratoFailure], getter[]},
          {$Failed, other},
          {MessageName[setter, "EInvalidValue"]},
          TestID -> idInvalid
        ]
      ]
    ]
  ],
  contextTestKeys
];

AppendTo[$AllTests,
  VerificationTest[
    (* the message says what was given and what the key takes *)
    {
      StringTemplate[SetPVerbose::EInvalidValue][1, "True or False"],
      Catch[SetPVerbose[1]; "NoThrow", GeneratoFailure],
      Catch[SetGridPointIndex[123]; "NoThrow", GeneratoFailure]
    },
    {"Invalid value: 1. Expected True or False.", $Failed, $Failed},
    {SetPVerbose::EInvalidValue, SetGridPointIndex::EInvalidValue},
    TestID -> "Context-InvalidValue-MessageText"
  ]
];

(* ========================================= *)
(* Test: ResetContext *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* every key is set back to its default, whatever it was set to *)
    ResetContext[];
    Scan[#[[3]][#[[5]]]&, contextTestKeys];
    {
      GetConfiguration[] === Association[#[[1]] -> #[[5]]& /@ contextTestKeys],
      ResetContext[];
      GetConfiguration[] === Association[#[[1]] -> #[[4]]& /@ contextTestKeys]
    },
    {True, True},
    TestID -> "ResetContext-EveryKeyBackToItsDefault"
  ]
];

(* ========================================= *)
(* Test: PrintVerbose *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* its arguments are printed when PVerbose is set, and not otherwise *)
    ResetContext[];
    contextTestPrinted = {};
    Block[{Print = (AppendTo[contextTestPrinted, {##}]&)},
      PrintVerbose["not printed"];
      SetPVerbose[True];
      PrintVerbose["printed ", 1];
      SetPVerbose[False];
      PrintVerbose["not printed either"]
    ];
    contextTestPrinted,
    {{"printed ", 1}},
    TestID -> "PrintVerbose-PrintsWhenPVerboseIsSet"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ContextTests.wl completed."]];
