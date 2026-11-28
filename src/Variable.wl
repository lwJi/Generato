(* ::Package:: *)

(* Generato`Variable`, declare: a var spec becomes a variable record and an xAct tensor, and the store of the records *)

(* (c) Liwei Ji, 11/2026 *)

(*
  xAct is loaded here, by the first file that reads it, and its banners are
  not printed in quiet mode. The contexts are named in BeginPackage, so the
  script that loads Generato finds the symbols of xAct too.
*)

If[GetQuietMode[],
  Block[{Print},
    BeginPackage["Generato`Variable`", {"xAct`xPerm`", "xAct`xTensor`", "xAct`xCoba`", "Generato`Failure`", "Generato`Context`"}]
  ]
  ,
  BeginPackage["Generato`Variable`", {"xAct`xPerm`", "xAct`xTensor`", "xAct`xCoba`", "Generato`Failure`", "Generato`Context`"}]
];

If[!GetQuietMode[],
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Variable`, {2026, 11, 28}"];
  System`Print["------------------------------------------------------------"];
];

GetDefaultManifold::usage = "GetDefaultManifold[] returns the first defined manifold, the one variables are declared on. It stops the run if no manifold has been defined.";

GetDefaultChart::usage = "GetDefaultChart[] returns the first chart of the default manifold, the one variables are bound in.";

GetDim::usage = "GetDim[] returns the dimension of the default manifold.";

ParseVar::usage = "ParseVar[var] returns the variable record of a var spec, with the fields \"Head\", \"Name\", \"Slots\", \"Symmetry\", \"Pairs\", \"PrintAs\", \"DerivativeOf\" and \"TimeDerivativeOf\".";

DerivativeOf::usage = "DerivativeOf -> head, a rule of a var spec, declares the variable as a spatial derivative of the declared variable head. The derivative indices are the leading ones.";

TimeDerivativeOf::usage = "TimeDerivativeOf -> head, a rule of a var spec, declares the variable as the time derivative of the declared variable head.";

DefineTensor::usage = "DefineTensor[variable] defines the xAct tensor of a variable record, stores the record and returns it. It stops the run if the head is a tensor already, or if a relation of the record names a variable that is not declared or whose rank does not fit.";

DefTensors::usage = "DefTensors[var1, var2, ...] declares tensors without binding them to storage, and returns the var specs.";

DeclaredVariable::usage = "DeclaredVariable[head] returns the record of a declared variable, with \"Storage\" and \"Components\" once it is bound, or Missing[\"NotDeclared\", head].";

RecordBinding::usage = "RecordBinding[head, storage, components] adds \"Storage\" and \"Components\" to the record of a declared variable. It stops the run if the head is not declared or is bound already.";

Begin["`Private`"];

(*
  The manifold a script defined first is the one its variables are declared
  on, and the first chart of that manifold is the one they are bound in.
*)
GetDefaultManifold[] :=
  If[Length[$Manifolds] === 0,
    Stop[GetDefaultManifold::NoManifolds]
    ,
    $Manifolds[[1]]
  ];

GetDefaultManifold::NoManifolds = "No manifolds have been defined. Use DefManifold to define a manifold.";

GetDefaultChart[] :=
  ChartsOfManifold[GetDefaultManifold[]][[1]];

GetDim[] :=
  DimOfManifold[GetDefaultManifold[]];

Protect[GetDefaultManifold, GetDefaultChart, GetDim];

(*
  Declaring a variable is ParseVar followed by DefineTensor.

  A var spec is a list of features, in any order:
    the tensor with its slots     dgamt[-k, -i, -j]
    at most one symmetry          Symmetric[{-i, -j}]
    at most one print name        PrintAs -> "dgamt"
    at most one relation          DerivativeOf -> gamt, or TimeDerivativeOf -> gamt

  ParseVar turns it into the variable record, the facts a declaration states:
    <|"Head" -> dgamt, "Name" -> "dgamt", "Slots" -> {-k, -i, -j},
      "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{2, 3, 1}}, "PrintAs" -> "dgamt",
      "DerivativeOf" -> gamt, "TimeDerivativeOf" -> None|>

  "Name" is the head as C code names it: without the suffix that keeps a head
  apart from a system symbol (Pi$Upt is "Pi").
  "Symmetry" is the symmetry as the spec states it, which is what xAct is
  given. "Pairs" is the same fact in the one form the enumeration of
  components reads: {first slot, second slot, 1 symmetric | -1 antisymmetric}.
  "DerivativeOf" and "TimeDerivativeOf" are the head of the variable this one
  is a derivative of. A spatial derivative has its derivative slots first and
  then the slots of that variable; a time derivative has the same slots.
  Nothing else says what a variable differentiates: its name carries no meaning.
  A field the spec does not state is None.

  The store: head -> record, one per declared variable. It is private to
  this file and has two writers, one per stage. DefineTensor writes the
  record as ParseVar gives it. RecordBinding, which the binding in
  Component.wl calls, adds to that same record:
      "Storage" -> "Grid",
      "Components" -> {<|"Comp" -> gam[{1, -cart}, {1, -cart}], "Indices" -> {1, 1}, "Slot" -> 0,
                         "Name" -> "gam11", "Expr" -> gam11[[ijk]]|>, ...}
  So a variable that is declared and not bound has a record without
  "Storage". DeclaredVariable is the one reader.
*)

$Variables = <||>;

DeclaredVariable[head_Symbol] :=
  Lookup[$Variables, head, Missing["NotDeclared", head]];

Protect[DeclaredVariable];

RecordBinding[head_Symbol, storage_String, components_List] :=
  Module[{},
    If[!KeyExistsQ[$Variables, head] || KeyExistsQ[$Variables[head], "Storage"],
      Stop[RecordBinding::EState, head]
    ];
    $Variables[head] = Join[$Variables[head], <|"Storage" -> storage, "Components" -> components|>]
  ];

RecordBinding::EState = "Variable `1` cannot be given storage: it is not declared, or it is bound already.";

Protect[RecordBinding];

FeatureKind[feature_] :=
  Which[
    MatchQ[feature, _Rule | _RuleDelayed],
      "Rule"
    ,
    MatchQ[feature, (Symmetric | Antisymmetric | GenSet)[___]],
      "Symmetry"
    ,
    MatchQ[feature, _Symbol[___]],
      "Tensor"
    ,
    True,
      None
  ];

(*
  SymmetryPairs - a symmetry as pairs of slots, or $Failed if it is not a set
  of disjoint pairs of the tensor's slots.
    Symmetric[{-i, -j}] on dgamt[-k, -i, -j]    ->  {{2, 3, 1}}
    GenSet[Cycles[{1, 2}], Cycles[{3, 4}]]      ->  {{1, 2, 1}, {3, 4, 1}}
    GenSet[-Cycles[{1, 2}], -Cycles[{3, 4}]]    ->  {{1, 2, -1}, {3, 4, -1}}
*)
SymmetryPairs[None, slots_List] := {};

SymmetryPairs[(kind : Symmetric | Antisymmetric)[{first_, second_}], slots_List] :=
  Module[{positions},
    positions = Flatten[Position[slots, Verbatim[#], {1}, Heads -> False]]& /@ {first, second};
    If[MatchQ[positions, {{_}, {_}}] && positions[[1]] =!= positions[[2]],
      {Append[Sort[Flatten[positions]], If[kind === Symmetric, 1, -1]]}
      ,
      $Failed
    ]
  ];

SymmetryPairs[GenSet[cycles__], slots_List] :=
  Module[{pairs},
    pairs =
      Replace[
        {cycles}
        ,
        {
          Cycles[{first_Integer, second_Integer}] :> {Min[first, second], Max[first, second], 1},
          -Cycles[{first_Integer, second_Integer}] :> {Min[first, second], Max[first, second], -1},
          _ -> $Failed
        }
        ,
        {1}
      ];
    If[FreeQ[pairs, $Failed] && DuplicateFreeQ[Flatten[pairs[[All, 1 ;; 2]]]] && SubsetQ[Range[Length[slots]], Flatten[pairs[[All, 1 ;; 2]]]],
      Sort[pairs]
      ,
      $Failed
    ]
  ];

SymmetryPairs[_, _List] := $Failed;

ParseVar[var_List] :=
  Module[{kinds, rules, tensor, symmetry, pairs},
    kinds = FeatureKind /@ var;
    Scan[Stop[ParseVar::EVar, #, var]&, Pick[var, kinds, None]];
    (* a rule is told apart by its key: the print name, or one of the two relations *)
    rules = Pick[var, kinds, "Rule"];
    Scan[
      If[!MatchQ[#, (PrintAs -> _String) | ((DerivativeOf | TimeDerivativeOf) -> _Symbol)],
        Stop[ParseVar::EKey, #, var]
      ]&
      ,
      rules
    ];
    If[Count[kinds, "Tensor"] =!= 1 || Count[kinds, "Symmetry"] > 1 || Count[rules, PrintAs -> _] > 1 || Count[rules, (DerivativeOf | TimeDerivativeOf) -> _] > 1,
      Stop[ParseVar::EShape, var]
    ];
    tensor = First[Pick[var, kinds, "Tensor"]];
    symmetry = First[Pick[var, kinds, "Symmetry"], None];
    If[Length[tensor] > 4,
      Stop[ParseVar::ERank, tensor, Length[tensor]]
    ];
    pairs = SymmetryPairs[symmetry, List @@ tensor];
    If[pairs === $Failed,
      Stop[ParseVar::ESymmetryType, symmetry, tensor]
    ];
    <|
      "Head" -> Head[tensor],
      "Name" -> StringTrim[ToString[Head[tensor]], GetSuffixUnprotected[]],
      "Slots" -> List @@ tensor,
      "Symmetry" -> symmetry,
      "Pairs" -> pairs,
      "PrintAs" -> FirstCase[rules, (PrintAs -> printas_) :> printas, None],
      "DerivativeOf" -> FirstCase[rules, (DerivativeOf -> base_) :> base, None],
      "TimeDerivativeOf" -> FirstCase[rules, (TimeDerivativeOf -> base_) :> base, None]
    |>
  ];

ParseVar[var_] :=
  Stop[ParseVar::EShape, var];

ParseVar::EVar = "Var feature `1` in `2` unsupported yet!";

ParseVar::EKey = "Rule `1` in var `2` is not understood. The rules a var takes are PrintAs -> \"name\", DerivativeOf -> variable and TimeDerivativeOf -> variable.";

ParseVar::EShape = "Var `1` must be a list of one tensor, at most one symmetry, at most one PrintAs and at most one of DerivativeOf and TimeDerivativeOf.";

ParseVar::ERank = "Tensor `1` has `2` indices. A tensor with more than 4 is unsupported yet!";

ParseVar::ESymmetryType = "Symmetry `1` of `2` is unsupported yet! A symmetry must be a set of disjoint pairs of the tensor's indices.";

Protect[ParseVar, DerivativeOf, TimeDerivativeOf];

(*
  CheckRelation - a relation is checked where it is declared.
  The variable it names must be declared already, and its rank must fit:
  fits[rank, rank of that variable] says whether it does. A record that
  states no such relation passes.
*)
CheckRelation[variable_Association, relation_String, fits_] :=
  With[{head = variable["Head"], basehead = Lookup[variable, relation, None], rank = Length[variable["Slots"]], tag = "E" <> relation},
    If[basehead =!= None,
      With[{base = DeclaredVariable[basehead]},
        If[MissingQ[base],
          Stop[DefineTensor::EBase, head, relation, basehead]
        ];
        If[!fits[rank, Length[base["Slots"]]],
          Stop[MessageName[DefineTensor, tag], head, basehead, rank, Length[base["Slots"]]]
        ]
      ]
    ]
  ];

(*
  DefineTensor - gives xAct the tensor of a variable record, and stores the record.
  The relations are checked first, so a record that fails leaves no tensor behind.
  An empty PrintAs is not handed on: xAct then prints the tensor by its head.
*)
DefineTensor[variable_Association] :=
  Module[{head = variable["Head"], symmetry = variable["Symmetry"], printas = variable["PrintAs"]},
    If[xTensorQ[head],
      Stop[DefineTensor::EExists, head]
    ];
    (* a spatial derivative has more slots than the variable it differentiates, a time derivative as many *)
    CheckRelation[variable, "DerivativeOf", Greater];
    CheckRelation[variable, "TimeDerivativeOf", Equal];
    DefTensor @@
      Join[
        {head @@ variable["Slots"], GetDefaultManifold[]},
        If[symmetry === None, {}, {symmetry}],
        If[StringQ[printas] && StringLength[printas] > 0, {PrintAs -> printas}, {}]
      ];
    $Variables[head] = variable
  ];

DefineTensor::EExists = "`1` is a tensor already. A variable is declared once, please try a different name!";

DefineTensor::EBase = "`1` declares `2` -> `3`, but `3` is not a declared variable. Declare `3` before `1`.";

DefineTensor::EDerivativeOf = "`1` declares DerivativeOf -> `2`, but `1` has `3` indices and `2` has `4`. A derivative has more indices than the variable it differentiates: the derivative indices come first.";

DefineTensor::ETimeDerivativeOf = "`1` declares TimeDerivativeOf -> `2`, but `1` has `3` indices and `2` has `4`. A time derivative has as many indices as the variable it is the time derivative of.";

Protect[DefineTensor];

DefTensors[vars__] :=
  Module[{arglist = List[vars]},
    Scan[DefineTensor[ParseVar[#]]&, arglist];
    arglist
  ];

Protect[DefTensors];

End[];

EndPackage[];
