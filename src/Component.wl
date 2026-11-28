(* ::Package:: *)

(* Generato`Component`, bind: a declared variable gets storage and one record per independent component *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`", {"Generato`Failure`", "Generato`Context`"}];

Needs["Generato`Basic`"];

Needs["Generato`Variable`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Component`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

IndependentIndices::usage = "IndependentIndices[rank, pairs, {iMin, iMax}] returns the index tuples of the independent components of a tensor, first slot slowest. A pair is {first slot, second slot, 1 symmetric | -1 antisymmetric}.";

GridTensors::usage = "GridTensors[var1, var2, ...] declares tensors, binds them to grid storage (C names carry the grid point index), and returns the varlist.";

TileTensors::usage = "TileTensors[var1, var2, ...] declares tensors, binds them to tile storage (C names carry the tile point index), and returns the varlist.";

TempTensors::usage = "TempTensors[var1, var2, ...] declares tensors, binds them as temporaries (C names carry no point index), and returns the varlist.";

VariableRecord::usage = "VariableRecord[head] returns the record of a bound variable.\nVariableRecord[var] returns the record of the bound variable a var spec names. It stops the run if the variable is not bound.";

PrintComponent::usage = "PrintComponent[coordinate, variable, component, extrareplacerules] prints one component record of a bound variable through the backend, as an initialization or as an equation.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[index] returns True if the abstract index represents a 3D spatial index (first letter >= 'i').";

Begin["`Private`"];

(*
  IndependentIndices - the independent components of a tensor, as index tuples.
  Tuples gives every tuple with the first slot slowest, and a pair keeps the
  tuples whose two entries are in order: equal entries pass a symmetric pair
  and fail an antisymmetric one.
    IndependentIndices[2, {{1, 2, 1}}, {1, 3}]  ->  {1,1} {1,2} {1,3} {2,2} {2,3} {3,3}
    IndependentIndices[3, {{2, 3, 1}}, {1, 3}]  ->  18 tuples, {1,1,1} {1,1,2} ... {3,3,3}
*)
IndependentIndices[rank_Integer?NonNegative, pairs_List, {iMin_Integer, iMax_Integer}] :=
  Select[Tuples[Range[iMin, iMax], rank], Function[indices, AllTrue[pairs, OrderedPairQ[indices, #]&]]];

OrderedPairQ[indices_List, {first_Integer, second_Integer, 1}] :=
  indices[[first]] <= indices[[second]];

OrderedPairQ[indices_List, {first_Integer, second_Integer, -1}] :=
  indices[[first]] < indices[[second]];

Protect[IndependentIndices];

(*
  The record of a bound variable. The store of the records is in
  Variable.wl: a declaration writes the record there, and BindVariable adds
  "Storage" and "Components" to it through RecordBinding. No second store is
  kept here.

    <|"Head" -> gam, "Name" -> "gam", "Slots" -> {-i, -j},                    declared
      "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "gam",
      "Storage" -> "Grid",                                                     bound
      "Components" -> {<|"Comp" -> gam[{1, -cart}, {1, -cart}], "Indices" -> {1, 1}, "Slot" -> 0,
                         "Name" -> "gam11", "Expr" -> gam11[[ijk]]|>, ...}|>

  "Comp" is the component in xAct's notation. "Slot" is its position in the
  list, counted from 0 as C does. "Name" is the bare C identifier and "Expr"
  is what an equation refers to, which carries the point index of the storage.

  VariableRecord is what a print call reads. A variable that is declared and
  not bound has a record without "Storage", and nothing to print.
*)
BoundQ[variable_] :=
  AssociationQ[variable] && KeyExistsQ[variable, "Storage"];

VariableRecord[head_Symbol] :=
  With[{variable = DeclaredVariable[head]},
    If[!BoundQ[variable],
      Stop[VariableRecord::EUnbound, head]
    ];
    variable
  ];

VariableRecord[var_] :=
  VariableRecord[ParseVar[var]["Head"]];

VariableRecord::EUnbound = "Variable `1` is not bound. Bind it with GridTensors, TileTensors or TempTensors before it is used.";

Protect[VariableRecord];

(* The range a component index runs over: 1 to 3 in three dimensions, 0 to dim - 1 otherwise *)
ComponentIndexRange[] :=
  With[{dim = GetDim[]},
    If[dim == 3, {1, 3}, {0, dim - 1}]
  ];

(*
    compname: component expr in Mathematica kernel, say Pi[{1,-cart},{2,-cart}]
*)
ComponentName[variable_Association, indices_List, coordinate_] :=
  variable["Head"] @@ MapThread[{#2, If[DownIndexQ[#1], -coordinate, coordinate]}&, {variable["Slots"], indices}];

(* The point index an equation writes after the C name, which is what storage decides *)
PointIndex[storage_String] :=
  Which[
    storage === "Temp" || GetInterfaceWithNonCoordBasis[],
      ""
    ,
    storage === "Tile",
      GetTilePointIndex[]
    ,
    storage === "Grid",
      GetGridPointIndex[]
  ];

(*
    name: the C identifier, say Pi12, ignoring the information about covariant/contravariant
    expr: what is printed to C code, or lhs, say Pi12[[ijk]]
*)
ComponentRecord[variable_Association, storage_String, coordinate_, indices_List, slot_Integer] :=
  Module[{name, colist = {"t", "x", "y", "z"}},
    name =
      variable["Name"] <>
        StringJoin[
          If[GetUseLetterForTensorComponent[],
            colist[[# + 1]]
            ,
            ToString[#]
          ]& /@ indices
        ];
    <|
      "Comp" -> ComponentName[variable, indices, coordinate],
      "Indices" -> indices,
      "Slot" -> slot,
      "Name" -> name,
      "Expr" -> ToExpression[name <> PointIndex[storage]]
    |>
  ];

(*
  The slots of a 3D index that hold the component 0. A tensor of a 4D
  manifold whose index letter is spatial has no such component, so the
  index tuple is dropped.
*)
ZeroSpatialSlots[slots_List, indices_List] :=
  Pick[slots, MapThread[#2 == 0 && Is3DAbstractIndex[#1]&, {slots, indices}]];

(* A component that is dropped is the number 0 when its 0 is on an up slot and on no down slot *)
ZeroValuedQ[slots_List, indices_List] :=
  With[{zeroslots = ZeroSpatialSlots[slots, indices]},
    AnyTrue[zeroslots, UpIndexQ] && NoneTrue[zeroslots, DownIndexQ]
  ];

(*
  BindVariable - declares one var, gives it storage and its component records.
  A var whose head is declared already cannot be bound yet: DefineTensor
  stops at a tensor that exists.
  The independent components are one filter over the index tuples, and the
  tuples with a 0 on a spatial slot are a second filter on the same list.
  xAct keeps one copy of what the records say: ToValues needs the rule from
  a component to its C expression when a right-hand side is evaluated. The
  rules are written here, from the records, and nothing reads a fact back
  out of them.
*)
BindVariable[binding_Symbol, storage_String, var_] :=
  Module[{variable = ParseVar[var], head, slots, coordinate, indices, components},
    head = variable["Head"];
    slots = variable["Slots"];
    If[BoundQ[DeclaredVariable[head]],
      Stop[MessageName[binding, "EBound"], head, DeclaredVariable[head]["Storage"]]
    ];
    DefineTensor[variable];
    coordinate = GetDefaultChart[];
    indices = IndependentIndices[Length[slots], variable["Pairs"], ComponentIndexRange[]];
    components =
      MapIndexed[
        ComponentRecord[variable, storage, coordinate, #1, First[#2] - 1]&
        ,
        Select[indices, ZeroSpatialSlots[slots, #] === {}&]
      ];
    PrintVerbose["  Bind ", head, " as ", storage, " in ", coordinate, ": ", Length[components], " components"];
    Scan[ComponentValue[ComponentName[variable, #, coordinate], 0]&, Select[indices, ZeroValuedQ[slots, #]&]];
    Scan[ComponentValue[#["Comp"], #["Expr"]]&, components];
    If[variable["Pairs"] =!= {},
      head @@ slots //
      ToBasis[coordinate] //
      ComponentArray //
      ComponentValue
    ];
    RecordBinding[head, storage, components];
  ];

(*
  The three bindings. Each returns its var specs, so a varlist stays a plain
  list that a script may slice. When input equations are only being checked,
  the tensors are declared and nothing is bound.
*)
BindVarlist[binding_Symbol, storage_String, varlist_List] :=
  Module[{},
    If[GetCheckInputEquations[],
      DefTensors @@ varlist
      ,
      Scan[BindVariable[binding, storage, #]&, varlist]
    ];
    varlist
  ];

(* Original API: GridTensors[var1, var2, ...] *)
GridTensors[vars__] :=
  BindVarlist[GridTensors, "Grid", List[vars]];

(* Original API: TileTensors[var1, var2, ...] *)
TileTensors[vars__] :=
  BindVarlist[TileTensors, "Tile", List[vars]];

(* Original API: TempTensors[var1, var2, ...] *)
TempTensors[vars__] :=
  BindVarlist[TempTensors, "Temp", List[vars]];

(* The message is on the binding the script called *)
Scan[
  Function[binding,
    MessageName[binding, "EBound"] = "Variable `1` is bound already, as `2` storage. A variable is bound once."
  ]
  ,
  {GridTensors, TileTensors, TempTensors}
];

Protect[GridTensors, TileTensors, TempTensors];

(*
  PrintComponent - hands one component record to the backend.
  The records of a variable are its components in the chart it was bound
  in. A print call that names another chart has no record to print, except
  for a scalar, whose component carries no chart.
*)
PrintComponent[coordinate_, variable_Association, component_Association, extrareplacerules_] :=
  Module[{compname = component["Comp"]},
    If[ComponentName[variable, component["Indices"], coordinate] =!= compname,
      Stop[PrintComponent::EChart, variable["Head"], coordinate]
    ];
    Which[
      InInitializationsMode[],
        PrintVerbose["    PrintComponentInitialization ", compname, "..."];
        Global`PrintComponentInitialization[{variable["Head"] @@ variable["Slots"], variable["Symmetry"]}, compname]
      ,
      InEquationsMode[],
        PrintVerbose["    PrintComponentEquation ", compname, "..."];
        Global`PrintComponentEquation[coordinate, compname, extrareplacerules]
      ,
      True,
        Stop[PrintComponent::EMode]
    ]
  ];

PrintComponent::EMode = "PrintMode unrecognized!";

PrintComponent::EChart = "Variable `1` has no components in chart `2`: it is bound in the default chart.";

Protect[PrintComponent];

Is3DAbstractIndex[idx_] :=
  Module[{},
    LetterNumber[StringPart[ToString[idx /. {Times[-1, x_] :> x}], 1]] >= LetterNumber["i"]
  ];

End[];

EndPackage[];
