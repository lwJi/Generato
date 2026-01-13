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
GetGFIndexNameMix2nd::usage = "GetGFIndexNameMix2nd[index1, index2] creates mixed 2nd derivative GF index name.";
GetInterfaceName::usage = "GetInterfaceName[compname] returns the interface name for a component.";
ComputeRHSValue::usage = "ComputeRHSValue[coordinate, compname, extrareplacerules] computes the RHS value for an equation.";
PrintEquationByMode::usage = "PrintEquationByMode[compToValue, rhssToValue, mainFormatter, tempFormatter] prints equation based on current EquationsMode. tempFormatter is optional (defaults to Automatic for standard Temp output).";
GetTensorIndexSubbuf::usage = "GetTensorIndexSubbuf[len, varlistindex] computes subbuf string for tensor component indexing.";
ExtractComponentInfo::usage = "ExtractComponentInfo[varinfo, compname] extracts {varlistindex, compToValue, varname, symmetry, len} for component initialization.";

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
  GetGFIndexNameMix2nd - Creates mixed 2nd derivative GF index name
  Previously duplicated in: Carpet.wl, CarpetXGPU.wl, CarpetXPointDesc.wl
*)
GetGFIndexNameMix2nd[index1_?IntegerQ, index2_?IntegerQ] :=
  Module[{gfindex},
    gfindex = ToString[GetGFIndexName[index1]]
           <> ToString[GetGFIndexName[index2]];
    ToExpression[gfindex]
  ];

Protect[GetGFIndexNameMix2nd];

(*
  GetInterfaceName - Returns the interface name for a component
  Previously duplicated in: Carpet.wl, CarpetXGPU.wl, CarpetXPointDesc.wl
*)
GetInterfaceName[compname_] :=
  Module[{intfname = ToString[compname[[0]]], colist = {"t", "x", "y", "z"}},
    Do[
      coindex = compname[[icomp]][[1]];
      intfname = intfname <> colist[[coindex + 1]]
      ,
      {icomp, 1, Length[compname]}
    ];
    intfname = ToString[CForm[ToExpression[intfname <> GetGridPointIndex[]]]];
    Return[intfname];
  ];

Protect[GetInterfaceName];

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

  Calling conventions:
  1. PrintEquationByMode[compToValue, rhssToValue, mainFormatter] - callback for MainOut,
     default Temp (no const, CForm[compToValue]), shared AddToMainOut
  2. PrintEquationByMode[compToValue, rhssToValue, mainFormatter, tempFormatter] - callbacks
     for both MainOut and Temp, shared AddToMainOut

  The tempFormatter function takes (compToValue, rhssToValue) and prints the Temp mode output.
  If tempFormatter is Automatic (default), uses standard: GetTempVariableType[] + CForm[compToValue]
*)
PrintEquationByMode[compToValue_, rhssToValue_, mainFormatter_] :=
  PrintEquationByMode[compToValue, rhssToValue, mainFormatter, Automatic];

PrintEquationByMode[compToValue_, rhssToValue_, mainFormatter_, tempFormatter_] :=
  Module[{outputfile = GetOutputFile[]},
    Which[
      GetEquationsMode[] === "Temp",
        If[tempFormatter === Automatic,
          (* Default Temp handling: no const prefix, CForm[compToValue] *)
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
          ,
          (* Custom Temp handling via callback *)
          tempFormatter[compToValue, rhssToValue]
        ]
      ,
      GetEquationsMode[] === "MainOut",
        mainFormatter[compToValue, rhssToValue]
      ,
      GetEquationsMode[] === "AddToMainOut",
        (* Shared AddToMainOut - identical across all backends *)
        PutAppend[CForm[compToValue], outputfile];
        Global`pr["+="];
        PutAppend[CForm[rhssToValue], outputfile];
        Global`pr[";\n"]
      ,
      True,
        Throw @ Message[PrintEquationByMode::EMode]
    ]
  ];

PrintEquationByMode::EMode = "PrintEquationByMode: EquationsMode unrecognized!";

Protect[PrintEquationByMode];

(*
  GetTensorIndexSubbuf - Computes subbuf string for tensor component indexing
  This handles the common Scal/Vect/Smat pattern using varlistindex with
  bracket notation. Used by backends like CarpetXGPU and AMReX.
  Note: Some backends (CarpetX, Nmesh) have different indexing schemes
  and do not use this function.

  For "Scal" TensorType, any tensor length is handled with simple [varlistindex]
  because TensorType=Scal means each component is treated as independent scalar.
*)
GetTensorIndexSubbuf[len_Integer, varlistindex_Integer] :=
  Module[{tensorType = GetTensorType[]},
    Which[
      tensorType === "Scal",
        (* Scal treats all components as independent scalars *)
        If[len == 0, "", "[" <> ToString[varlistindex] <> "]"],
      tensorType === "Vect",
        Which[
          len == 1, "[" <> ToString[varlistindex] <> "]",
          len == 2,
            "[" <> ToString[Quotient[varlistindex, 3]] <> "]["
                <> ToString[Mod[varlistindex, 3]] <> "]",
          len == 3,
            "[" <> ToString[Quotient[varlistindex, 3]] <> "]["
                <> ToString[Mod[varlistindex, 3]] <> "]",
          True, Message[GetTensorIndexSubbuf::ELength]; ""
        ],
      tensorType === "Smat",
        Which[
          len == 2, "[" <> ToString[varlistindex] <> "]",
          len == 3,
            "[" <> ToString[Quotient[varlistindex, 6]] <> "]["
                <> ToString[Mod[varlistindex, 6]] <> "]",
          len == 4,
            "[" <> ToString[Quotient[varlistindex, 6]] <> "]["
                <> ToString[Mod[varlistindex, 6]] <> "]",
          True, Message[GetTensorIndexSubbuf::ELength]; ""
        ],
      True,
        Message[GetTensorIndexSubbuf::ETensorType]; ""
    ]
  ];

GetTensorIndexSubbuf::ELength = "GetTensorIndexSubbuf: Unsupported tensor rank for tensor type.";
GetTensorIndexSubbuf::ETensorType = "GetTensorIndexSubbuf: Unknown tensor type.";

Protect[GetTensorIndexSubbuf];

(*
  ExtractComponentInfo - Extracts common component information for initialization
  Returns: {varlistindex, compToValue, varname, symmetry, len}

  Previously duplicated pattern in all 6 backends:
    varlistindex = GetMapComponentToVarlist[][compname];
    compToValue = compname // ToValues;
    {varname, symmetry} = varinfo;
    len = Length[varname];
*)
ExtractComponentInfo[varinfo_, compname_] :=
  Module[{varlistindex, compToValue, varname, symmetry, len},
    varlistindex = GetMapComponentToVarlist[][compname];
    compToValue = compname // ToValues;
    {varname, symmetry} = varinfo;
    len = Length[varname];
    {varlistindex, compToValue, varname, symmetry, len}
  ];

Protect[ExtractComponentInfo];

(*
  Standard backend error messages - shared by all backends
  Previously duplicated in: CarpetX.wl, CarpetXGPU.wl, CarpetXPointDesc.wl,
  Carpet.wl, AMReX.wl, Nmesh.wl
*)
PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";
PrintComponentInitialization::EVarLength = "PrintComponentInitialization variable's tensor type unsupported!";

End[];

EndPackage[];
