(* ::Package:: *)

(* (c) Liwei Ji, 01/2026 *)

BeginPackage["Generato`BackendCommon`"];

Needs["Generato`Failure`"];
Needs["Generato`Basic`"];
Needs["Generato`ParseMode`"];
Needs["Generato`Component`"];
Needs["Generato`Emit`"];

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
IndexSuffix::usage = "IndexSuffix[variable, component, base] returns the index a component has in the storage of its variable, as C text: \"\" for a scalar, \"[v]\" for its slot v, and \"[v mod N][v div N]\" for a derivative of a variable with N components. base is the record DerivativeOf names.";
BaseComponent::usage = "BaseComponent[component, base] returns the component record of base that a component of one of its derivatives differentiates: the one at the trailing indices.";
RequireRelation::usage = "RequireRelation[variable, relation, mode] stops the run, with a pointer to the rule to add, if the variable record declares no relation of that name. mode is the mode it is loaded with.";

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
  Module[{intfname = ToString[compname[[0]]], colist = {"t", "x", "y", "z"}, coindex},
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
  Which[
    GetEquationsMode[] === "Temp",
      If[tempFormatter === Automatic,
        (* Default Temp handling: no const prefix, CForm[compToValue] *)
        Global`pr[GetTempVariableType[] <> " "];
        Global`pr[CText[compToValue]];
        Global`pr["="];
        Global`pr[CText[rhssToValue]];
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
      Global`pr[CText[compToValue]];
      Global`pr["+="];
      Global`pr[CText[rhssToValue]];
      Global`pr[";\n"]
    ,
    True,
      Stop[PrintEquationByMode::EMode]
  ];

PrintEquationByMode::EMode = "PrintEquationByMode: EquationsMode unrecognized!";

Protect[PrintEquationByMode];

(*
  IndexSuffix - the layout rule: where a component is in the storage of its variable.

    v = the component's slot, N = the number of independent components of
    the variable it is a derivative of

    no DerivativeOf:   ""  for a scalar, [v] otherwise
    DerivativeOf:      [v] when N is 1, [v mod N][v div N] otherwise

  The derivative slots lead, so v = N d + c for the derivative index d and
  the component c of the differentiated variable, and the index is [c][d].
  A time derivative has the components of its variable and is laid out like
  it, so TimeDerivativeOf plays no part here.
*)
IndexSuffix[variable_Association, component_Association, base_] :=
  With[{slot = component["Slot"]},
    If[variable["DerivativeOf"] === None,
      If[variable["Slots"] === {},
        ""
        ,
        "[" <> ToString[slot] <> "]"
      ]
      ,
      With[{count = Length[base["Components"]]},
        If[count == 1,
          "[" <> ToString[slot] <> "]"
          ,
          "[" <> ToString[Mod[slot, count]] <> "][" <> ToString[Quotient[slot, count]] <> "]"
        ]
      ]
    ]
  ];

Protect[IndexSuffix];

(*
  BaseComponent - the component a derivative component differentiates.
  The slots of the differentiated variable are the trailing ones, so its
  component is the one at the trailing indices: dgamt123 differentiates
  gamt23. A derivative declared with less symmetry than its variable has
  components for which there is none, say dgamt132.
*)
BaseComponent[component_Association, base_Association] :=
  Module[{indices = Take[component["Indices"], -Length[base["Slots"]]], basecomponent},
    basecomponent = SelectFirst[base["Components"], #["Indices"] === indices&, None];
    If[basecomponent === None,
      Stop[BaseComponent::EComponent, component["Name"], base["Head"], indices]
    ];
    basecomponent
  ];

BaseComponent::EComponent = "`1` is a derivative of the component of `2` at the indices `3`, and `2` has no independent component there. Declare the derivative with the symmetry of `2`.";

Protect[BaseComponent];

(*
  RequireRelation - a load that reads a relation needs it declared.
  A relation is never read out of a name, so a variable that is loaded as a
  derivative and declares none stops the run.
*)
$RelationPhrase = <|"DerivativeOf" -> "derivative", "TimeDerivativeOf" -> "time derivative"|>;

RequireRelation[variable_Association, relation_String, mode_String] :=
  If[Lookup[variable, relation, None] === None,
    Stop[RequireRelation::EMissing, variable["Head"], mode, relation, $RelationPhrase[relation]]
  ];

RequireRelation::EMissing = "`1` is loaded with \"`2`\" but declares no `3`. Add `3` -> v to its declaration, where v is the variable `1` is the `4` of.";

Protect[RequireRelation];

End[];

EndPackage[];
