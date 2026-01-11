(* ::Package:: *)

(* (c) Liwei Ji, 01/2026 *)

BeginPackage["Generato`BackendCommon`"];

Needs["Generato`Basic`"];
Needs["Generato`ParseMode`"];
Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`BackendCommon`, {2026, 1, 10}"];
  System`Print["------------------------------------------------------------"];
];

GetGFIndexName::usage = "GetGFIndexName[index] converts integer index to GF index name (p1, m1, c0, etc.).";
ComputeRHSValue::usage = "ComputeRHSValue[coordinate, compname, extrareplacerules] computes the RHS value for an equation.";
PrintEquationByMode::usage = "PrintEquationByMode[compToValue, rhssToValue, mainFormatter] prints equation based on current EquationsMode.";

Begin["`Private`"];

(*
  GetGFIndexName - Converts integer index to GF index name
  Previously duplicated in: AMReX.wl, Carpet.wl, CarpetXGPU.wl, CarpetXPointDesc.wl
*)
GetGFIndexName[index_?IntegerQ] :=
  Module[{gfindex},
    gfindex =
      Which[
        index > 0, "p" <> ToString[index],
        index < 0, "m" <> ToString[Abs[index]],
        True, "c0"
      ];
    ToExpression[gfindex]
  ];

Protect[GetGFIndexName];

(*
  ComputeRHSValue - Computes the RHS value for an equation
  Previously duplicated in all 6 backends
*)
ComputeRHSValue[coordinate_, compname_, extrareplacerules_] :=
  Module[{rhssToValue},
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], GetSuffixName[]]}) //
      DummyToBasis[coordinate] // TraceBasisDummy // ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    If[Length[extrareplacerules] > 0,
      rhssToValue = (rhssToValue // ToValues) /. extrareplacerules
    ];
    rhssToValue
  ];

Protect[ComputeRHSValue];

(*
  PrintEquationByMode - Prints equation based on current EquationsMode
  mainFormatter is a function that takes (compToValue, rhssToValue) and prints the Main mode output
  This handles the common Temp and AddToMainOut cases; backends provide custom Main formatting
*)
PrintEquationByMode[compToValue_, rhssToValue_, mainFormatter_] :=
  Module[{outputfile = GetOutputFile[]},
    Which[
      GetEquationsMode[] === "Temp",
        Module[{},
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetEquationsMode[] === "MainOut",
        mainFormatter[compToValue, rhssToValue]
      ,
      GetEquationsMode[] === "AddToMainOut",
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["+="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      True,
        Throw @ Message[PrintEquationByMode::EMode]
    ]
  ];

PrintEquationByMode::EMode = "PrintEquationByMode: EquationsMode unrecognized!";

Protect[PrintEquationByMode];

End[];

EndPackage[];
