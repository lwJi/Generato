(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Varlist`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Varlist`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

ParseVarlist::usage = "ParseVarlist[varlist, chartname] processes a varlist, setting or printing components for each tensor.\nParseVarlist[{ExtraReplaceRules->rules}, varlist, chartname] processes with additional replacement rules.";

ParseVar::usage = "ParseVar[var] parses a variable specification and returns {varname, symmetry, printname}.";

DefineTensor::usage = "DefineTensor[varname, symmetry, printname] defines a tensor using DefTensor with the given symmetry and print name.";

Begin["`Private`"];

(* Data *)

(* Function *)

Options[ParseVarlist] :=
  {ExtraReplaceRules -> {}};

ParseVarlist[OptionsPattern[], varlist_?ListQ, chartname_] :=
  Module[{iMin, iMax = 3, var, varinfo, varname, symmetry, printname, symname, symindex, parseComponentValue, extrareplacerules},
    {extrareplacerules} = OptionValue[{ExtraReplaceRules}];
    PrintVerbose["ParseVarlist..."];
    PrintVerbose["  Dim = ", GetDim[], ", Chart = ", chartname];
    PrintVerbose["  List: ", varlist];
    If[GetDim[] == 3,
      iMin = 1
      ,
      iMin = 0
    ];
    $CurrentContext = SetProcessNewVarlist[$CurrentContext, True];
    Do[
      var = varlist[[ivar]];
      {varname, symmetry, printname} = ParseVar[var];
      varinfo = {varname, symmetry};
      If[symmetry =!= Null,
        symname = symmetry[[0]];
        symindex = symmetry[[1]]
      ];
      If[!xTensorQ[varname[[0]]],
        If[InSetCompPhase[$CurrentContext],
          DefineTensor[varname, symmetry, printname]
          ,
          Throw @ Message[ParseVarlist::ETensorNonExist, ivar, varname]
        ]
        ,
        If[!MemberQ[Keys[GetMapComponentToVarlist[$CurrentContext]][[All, 0]], varname[[0]]],
          Throw @ Message[ParseVarlist::ETensorExistOutside, ivar, varname]
        ]
      ];
      parseComponentValue[compindexlist_] := ParseComponent[varinfo, compindexlist, chartname, extrareplacerules];
      Switch[Length[varname],
        0(* ZERO INDEX CASE: *),
          parseComponentValue[{}]
        ,
        1(* ONE INDEX CASE: *),
          Do[parseComponentValue[{ia}], {ia, iMin, iMax}]
        ,
        2(* TWO INDEXES CASE: *),
          If[symmetry =!= Null,
            Switch[symname,
              Symmetric,
                Do[parseComponentValue[{ia, ib}], {ia, iMin, iMax}, {ib, ia, iMax}]
              ,
              Antisymmetric,
                Do[parseComponentValue[{ia, ib}], {ia, iMin, iMax}, {ib, ia + 1, iMax}]
              ,
              _,
                Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
            ];
            varname //
            ToBasis[chartname] //
            ComponentArray //
            ComponentValue
            ,
            Do[parseComponentValue[{ia, ib}], {ia, iMin, iMax}, {ib, iMin, iMax}]
          ]
        ,
        3(* THREE INDEXES CASE *),
          If[symmetry =!= Null,
            Which[
              (symindex[[1]] === varname[[2]]) && (symindex[[2]] === varname[[3]])(*c(ab) or c[ab]*),
                Switch[symname,
                  Symmetric,
                    Do[parseComponentValue[{ic, ia, ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}]
                  ,
                  Antisymmetric,
                    Do[parseComponentValue[{ic, ia, ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}]
                  ,
                  _,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              (symindex[[1]] === varname[[1]]) && (symindex[[2]] === varname[[2]])(*(ab)c or [ab]c*),
                Switch[symname,
                  Symmetric,
                    Do[parseComponentValue[{ia, ib, ic}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax}]
                  ,
                  Antisymmetric,
                    Do[parseComponentValue[{ia, ib, ic}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax}]
                  ,
                  _,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              True,
                Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
            ];
            varname //
            ToBasis[chartname] //
            ComponentArray //
            ComponentValue
            ,
            Do[parseComponentValue[{ic, ia, ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
          ]
        ,
        4(* FOUR INDEXES CASE *),
          If[symmetry =!= Null,
            Which[
              symname === GenSet(*(cd)(ab) or [cd][ab]*),
                Which[
                  (symmetry[[1]] === Cycles[{1, 2}]) && (symmetry[[2]] === Cycles[{3, 4}]),
                    Do[parseComponentValue[{ic, id, ia, ib}], {ic, iMin, iMax}, {id, ic, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}]
                  ,
                  (symmetry[[1]] === -Cycles[{1, 2}]) && (symmetry[[2]] === -Cycles[{3, 4}]),
                    Do[parseComponentValue[{ic, id, ia, ib}], {ic, iMin, iMax}, {id, ic + 1, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}]
                  ,
                  True,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              (symindex[[1]] === varname[[3]]) && (symindex[[2]] === varname[[4]])(*cd(ab) or cd[ab]*),
                Switch[symname,
                  Symmetric,
                    Do[parseComponentValue[{ic, id, ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}]
                  ,
                  Antisymmetric,
                    Do[parseComponentValue[{ic, id, ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}]
                  ,
                  _,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              (symindex[[1]] === varname[[1]]) && (symindex[[2]] === varname[[2]])(*(ab)cd or [ab]cd*),
                Switch[symname,
                  Symmetric,
                    Do[parseComponentValue[{ia, ib, ic, id}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax}, {id, iMin, iMax}]
                  ,
                  Antisymmetric,
                    Do[parseComponentValue[{ia, ib, ic, id}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax}, {id, iMin, iMax}]
                  ,
                  _,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              (symindex[[1]] === varname[[2]]) && (symindex[[2]] === varname[[3]])(*c(ab)d or c[ab]d*),
                Switch[symname,
                  Symmetric,
                    Do[parseComponentValue[{ic, ia, ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}, {id, iMin, iMax}]
                  ,
                  Antisymmetric,
                    Do[parseComponentValue[{ic, ia, ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}, {id, iMin, iMax}]
                  ,
                  _,
                    Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
                ]
              ,
              True,
                Throw @ Message[ParseVarlist::ESymmetryType, ivar, varname]
            ];
            varname //
            ToBasis[chartname] //
            ComponentArray //
            ComponentValue
            ,
            Do[parseComponentValue[{ic, id, ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
          ]
        ,
        _(* OTHER NUM OF INDEXES *),
          Throw @ Message[ParseVarlist::ETensorType, ivar, varname]
      ]
      ,
      {ivar, 1, Length[varlist]}
    ];
    PrintVerbose[];
  ];

ParseVarlist::ETensorNonExist = "Tensor of the `1`-th var, `2`, in varlist not exist and can't be defined since SetComp is false. Please check that this type of tensor can be handled by Generato!";

ParseVarlist::ETensorExistOutside = "Tensor of the `1`-th var, `2`, in varlist already exists outside the global varlist, please try a different name!";

ParseVarlist::ETensorType = "Tensor type of the `1`-th var, `2`, in varlist unsupported yet!";

ParseVarlist::ESymmetryType = "Symmetry type of the `1`-th var, `2`, in varlist unsupported yet!";

Protect[ParseVarlist];

DefineTensor[varname_, symmetry_, printname_] :=
  Module[{manifold = GetDefaultManifold[]},
    If[symmetry =!= Null && StringLength[printname] > 0,
      DefTensor[varname, manifold, symmetry, PrintAs -> printname]
      ,
      If[symmetry =!= Null,
        DefTensor[varname, manifold, symmetry]
        ,
        If[StringLength[printname] > 0,
          DefTensor[varname, manifold, PrintAs -> printname]
          ,
          DefTensor[varname, manifold]
        ]
      ]
    ]
  ];

Protect[DefineTensor];

ParseVar[var_] :=
  Module[{vfeature, varname = Null, symmetry = Null, printname = ""},
    Do[
      vfeature = var[[ifeature]];
      If[Head @ vfeature === Rule,
        printname = vfeature[[2]]
        ,
        If[Head @ Head @ vfeature === Symbol,
          If[Length[vfeature] > 0 && AnyTrue[{Symmetric, Antisymmetric, GenSet}, # === Head @ vfeature&],
            symmetry = vfeature
            ,
            varname = vfeature
          ]
          ,
          Throw @ Message[ParseVar::EVar, vfeature, var]
        ]
      ]
      ,
      {ifeature, 1, Length[var]}
    ];
    {varname, symmetry, printname}
  ];

ParseVar::EVar = "Var feature `1` in `2` unsupported yet!";

Protect[ParseVar];

End[];

EndPackage[];
