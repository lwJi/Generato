(* ::Package:: *)

(* Generato`Variable`, declare: a var spec becomes a variable record and an xAct tensor, and the store of the records *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Variable`", {"Generato`Failure`", "Generato`Context`"}];

Needs["Generato`Basic`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Variable`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

ParseVar::usage = "ParseVar[var] returns the variable record of a var spec, with the fields \"Head\", \"Name\", \"Slots\", \"Symmetry\", \"Pairs\" and \"PrintAs\".";

DefineTensor::usage = "DefineTensor[variable] defines the xAct tensor of a variable record, stores the record and returns it. It stops the run if the head is a tensor already.";

DefTensors::usage = "DefTensors[var1, var2, ...] declares tensors without binding them to storage, and returns the var specs.";

DeclaredVariable::usage = "DeclaredVariable[head] returns the record of a declared variable, with \"Storage\" and \"Components\" once it is bound, or Missing[\"NotDeclared\", head].";

RecordBinding::usage = "RecordBinding[head, storage, components] adds \"Storage\" and \"Components\" to the record of a declared variable. It stops the run if the head is not declared or is bound already.";

Begin["`Private`"];

(*
  Declaring a variable is ParseVar followed by DefineTensor.

  A var spec is a list of features, in any order:
    the tensor with its slots     gam[-i, -j]
    at most one symmetry          Symmetric[{-i, -j}]
    at most one rule              PrintAs -> "gam"

  ParseVar turns it into the variable record, the facts a declaration states:
    <|"Head" -> gam, "Name" -> "gam", "Slots" -> {-i, -j},
      "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "gam"|>

  "Name" is the head as C code names it: without the suffix that keeps a head
  apart from a system symbol (Pi$Upt is "Pi").
  "Symmetry" is the symmetry as the spec states it, which is what xAct is
  given. "Pairs" is the same fact in the one form the enumeration of
  components reads: {first slot, second slot, 1 symmetric | -1 antisymmetric}.
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
  Module[{kinds, tensor, symmetry, rule, pairs},
    kinds = FeatureKind /@ var;
    Scan[Stop[ParseVar::EVar, #, var]&, Pick[var, kinds, None]];
    (* a rule is told apart by its key, and PrintAs is the one key *)
    Scan[
      If[!MatchQ[#, PrintAs -> _String],
        Stop[ParseVar::EKey, #, var]
      ]&
      ,
      Pick[var, kinds, "Rule"]
    ];
    If[Count[kinds, "Tensor"] =!= 1 || Count[kinds, "Symmetry"] > 1 || Count[kinds, "Rule"] > 1,
      Stop[ParseVar::EShape, var]
    ];
    tensor = First[Pick[var, kinds, "Tensor"]];
    symmetry = First[Pick[var, kinds, "Symmetry"], None];
    rule = First[Pick[var, kinds, "Rule"], None];
    If[Length[tensor] > 4,
      Stop[ParseVar::ETensorType, tensor, Length[tensor]]
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
      "PrintAs" -> If[rule === None, None, Last[rule]]
    |>
  ];

ParseVar[var_] :=
  Stop[ParseVar::EShape, var];

ParseVar::EVar = "Var feature `1` in `2` unsupported yet!";

ParseVar::EKey = "Rule `1` in var `2` is not understood. The one rule a var takes is PrintAs -> \"name\".";

ParseVar::EShape = "Var `1` must be a list of one tensor, at most one symmetry and at most one rule.";

ParseVar::ETensorType = "Tensor `1` has `2` indices. A tensor with more than 4 is unsupported yet!";

ParseVar::ESymmetryType = "Symmetry `1` of `2` is unsupported yet! A symmetry must be a set of disjoint pairs of the tensor's indices.";

Protect[ParseVar];

(*
  DefineTensor - gives xAct the tensor of a variable record, and stores the record.
  An empty PrintAs is not handed on: xAct then prints the tensor by its head.
*)
DefineTensor[variable_Association] :=
  Module[{head = variable["Head"], symmetry = variable["Symmetry"], printas = variable["PrintAs"]},
    If[xTensorQ[head],
      Stop[DefineTensor::EExists, head]
    ];
    DefTensor @@
      Join[
        {head @@ variable["Slots"], GetDefaultManifold[]},
        If[symmetry === None, {}, {symmetry}],
        If[StringQ[printas] && StringLength[printas] > 0, {PrintAs -> printas}, {}]
      ];
    $Variables[head] = variable
  ];

DefineTensor::EExists = "`1` is a tensor already. A variable is declared once, please try a different name!";

Protect[DefineTensor];

DefTensors[vars__] :=
  Module[{arglist = List[vars]},
    Scan[DefineTensor[ParseVar[#]]&, arglist];
    arglist
  ];

Protect[DefTensors];

End[];

EndPackage[];
