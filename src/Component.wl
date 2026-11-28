(* ::Package:: *)

(* Generato`Component`, bind: a declared variable gets storage and one record per independent component *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`", {"xAct`xPerm`", "xAct`xTensor`", "xAct`xCoba`", "Generato`Failure`", "Generato`Context`", "Generato`Variable`"}];

If[!GetQuietMode[],
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Component`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

IndependentIndices::usage = "IndependentIndices[rank, pairs, {iMin, iMax}] returns the index tuples of the independent components of a tensor, first slot slowest. A pair is {first slot, second slot, 1 symmetric | -1 antisymmetric}.";

GridTensors::usage = "GridTensors[var1, var2, ...] declares tensors, binds them to grid storage (C names carry the grid point index), and returns the varlist.\nGridTensors[varlist] takes the var specs as one list, say the one DefTensors returned in a module. A variable that is declared already is bound without being declared again.";

TileTensors::usage = "TileTensors[var1, var2, ...] declares tensors, binds them to tile storage (C names carry the tile point index), and returns the varlist.\nTileTensors[varlist] takes the var specs as one list, say the one DefTensors returned in a module. A variable that is declared already is bound without being declared again.";

TempTensors::usage = "TempTensors[var1, var2, ...] declares tensors, binds them as temporaries (C names carry no point index), and returns the varlist.\nTempTensors[varlist] takes the var specs as one list, say the one DefTensors returned in a module. A variable that is declared already is bound without being declared again.";

BoundQ::usage = "BoundQ[variable] returns True if variable is the record of a bound variable: what DeclaredVariable returns for a head once a binding has given it \"Storage\". It is False for the record of a variable that is declared and not bound, and for what DeclaredVariable returns for a head that is not declared.";

VariableRecord::usage = "VariableRecord[head] returns the record of a bound variable.\nVariableRecord[var] returns the record of the bound variable a var spec names. It stops the run if the variable is not bound.";

FindComponent::usage = "FindComponent[variable, comp] returns the component record the record of a bound variable has for a component written in xAct's notation, say gam[{1, -cart}, {2, -cart}], or Missing[\"NotIndependent\", comp] if comp is not one of its independent components.";

ComponentRecord::usage = "ComponentRecord[comp] returns the record of a component written in xAct's notation, say gam[{1, -cart}, {2, -cart}]. It stops the run if comp is not an independent component of a bound variable.";

BaseRecord::usage = "BaseRecord[variable] returns the record of the bound variable that a variable record declares a relation to, by DerivativeOf or by TimeDerivativeOf, or None if it declares none.";

RequireChart::usage = "RequireChart[variable, component, coordinate] stops the run if a component record of the variable is not a component in the chart coordinate. The records of a variable are its components in the chart it was bound in; the component of a scalar carries no chart.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[index] returns True if the abstract index represents a 3D spatial index (first letter >= 'i').";

GetInterfaceName::usage = "GetInterfaceName[compname] returns the interface name for a component.";

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
      "DerivativeOf" -> None, "TimeDerivativeOf" -> None,
      "Storage" -> "Grid",                                                     bound
      "Components" -> {<|"Comp" -> gam[{1, -cart}, {1, -cart}], "Indices" -> {1, 1}, "Slot" -> 0,
                         "Name" -> "gam11", "Expr" -> gam11[[ijk]]|>, ...}|>

  "Comp" is the component in xAct's notation. "Slot" is its position in the
  list, counted from 0 as C does. "Name" is the bare C identifier and "Expr"
  is what an equation refers to, which carries the point index of the storage.

  VariableRecord is what the lowering of a program reads. A variable that is
  declared and not bound has a record without "Storage", and nothing to
  write; BoundQ tells the two apart without stopping.
  FindComponent is the one statement of how a component in xAct's notation
  is found among the records of a variable: it is the "Comp" of one of them.
  The mirror of a symmetric pair is the "Comp" of none. ComponentRecord
  finds the record through the head of the component, and stops where
  FindComponent finds none; whoever checks a program before anything is
  computed asks FindComponent. BaseRecord follows a declared relation to the
  record of the variable it names.
*)
BoundQ[variable_] :=
  AssociationQ[variable] && KeyExistsQ[variable, "Storage"];

Protect[BoundQ];

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

FindComponent[variable_Association, comp_] :=
  SelectFirst[variable["Components"], #["Comp"] === comp&, Missing["NotIndependent", comp]];

Protect[FindComponent];

ComponentRecord[comp_] :=
  Module[{variable = VariableRecord[Head[comp]], component},
    component = FindComponent[variable, comp];
    If[MissingQ[component],
      Stop[ComponentRecord::EComponent, comp, variable["Head"]]
    ];
    component
  ];

ComponentRecord::EComponent = "`1` is not an independent component of the bound variable `2`.";

Protect[ComponentRecord];

(* A var spec states at most one relation, so a variable has at most one such record *)
BaseRecord[variable_Association] :=
  With[{head = FirstCase[Lookup[variable, {"DerivativeOf", "TimeDerivativeOf"}, None], Except[None], None]},
    If[head === None,
      None
      ,
      VariableRecord[head]
    ]
  ];

Protect[BaseRecord];

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
NewComponentRecord[variable_Association, storage_String, coordinate_, indices_List, slot_Integer] :=
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
  DeclaredForBinding - the declared record of the variable a var names.
  A var whose head is not declared is declared here, so a script declares
  and binds in one call. A head that is declared already is the variable of
  a module, which declares with DefTensors and names no storage: it is bound
  without being declared again, and the record the declaration wrote stands.
  The var is the spec the declaration was given, as DefTensors returned it;
  one that states the variable differently stops the run, since the two
  could not both be true. A variable is bound once.
  A head that is a tensor and has no record was defined outside Generato:
  DefineTensor stops at it.
*)
DeclaredForBinding[binding_Symbol, var_] :=
  Module[{variable = ParseVar[var], declared},
    declared = DeclaredVariable[variable["Head"]];
    Which[
      MissingQ[declared],
        DefineTensor[variable]
      ,
      BoundQ[declared],
        Stop[MessageName[binding, "EBound"], variable["Head"], declared["Storage"]]
      ,
      declared =!= variable,
        Stop[MessageName[binding, "EDeclared"], variable["Head"], var]
      ,
      True,
        declared
    ]
  ];

(*
  BindVariable - gives the variable of one var its storage and its component
  records, declaring it first if it is not declared.
  The independent components are one filter over the index tuples, and the
  tuples with a 0 on a spatial slot are a second filter on the same list.
  xAct keeps one copy of what the records say: ToValues needs the rule from
  a component to its C expression when a right-hand side is evaluated. The
  rules are written here, from the records, and nothing reads a fact back
  out of them.
*)
BindVariable[binding_Symbol, storage_String, var_] :=
  Module[{variable = DeclaredForBinding[binding, var], head, slots, coordinate, indices, components},
    head = variable["Head"];
    slots = variable["Slots"];
    coordinate = GetDefaultChart[];
    indices = IndependentIndices[Length[slots], variable["Pairs"], ComponentIndexRange[]];
    components =
      MapIndexed[
        NewComponentRecord[variable, storage, coordinate, #1, First[#2] - 1]&
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
      Scan[DeclaredForBinding[binding, #]&, varlist]
      ,
      Scan[BindVariable[binding, storage, #]&, varlist]
    ];
    varlist
  ];

(*
  VarSpecs - the var specs a binding was given: one by one, or as one list,
  which is how a script hands on the list DefTensors returned in a module.
  The two are told apart by the elements: a var spec holds its tensor, which
  is no list, so a list of lists is a list of var specs.
*)
VarSpecs[varlist : {__List}] := varlist;

VarSpecs[vars__] := {vars};

(* Original API: GridTensors[var1, var2, ...] *)
GridTensors[vars__] :=
  BindVarlist[GridTensors, "Grid", VarSpecs[vars]];

(* Original API: TileTensors[var1, var2, ...] *)
TileTensors[vars__] :=
  BindVarlist[TileTensors, "Tile", VarSpecs[vars]];

(* Original API: TempTensors[var1, var2, ...] *)
TempTensors[vars__] :=
  BindVarlist[TempTensors, "Temp", VarSpecs[vars]];

(* The messages are on the binding the script called *)
Scan[
  Function[binding,
    MessageName[binding, "EBound"] = "Variable `1` is bound already, as `2` storage. A variable is bound once.";
    MessageName[binding, "EDeclared"] = "Variable `1` is declared already, and the var `2` states it differently. A variable is declared once: bind it with the var it was declared with, as DefTensors returned it."
  ]
  ,
  {GridTensors, TileTensors, TempTensors}
];

Protect[GridTensors, TileTensors, TempTensors];

(*
  RequireChart - a node of a program is about the records of a variable in
  one chart. The records are the components in the chart the variable was
  bound in. A node that names another chart has no record to write, except
  for a scalar, whose component carries no chart.
*)
RequireChart[variable_Association, component_Association, coordinate_] :=
  If[ComponentName[variable, component["Indices"], coordinate] =!= component["Comp"],
    Stop[RequireChart::EChart, variable["Head"], coordinate]
  ];

RequireChart::EChart = "Variable `1` has no components in chart `2`: it is bound in the default chart.";

Protect[RequireChart];

Is3DAbstractIndex[idx_] :=
  Module[{},
    LetterNumber[StringPart[ToString[idx /. {Times[-1, x_] :> x}], 1]] >= LetterNumber["i"]
  ];

(*
  GetInterfaceName names a component at an interface. It was shared by
  backends that are gone: nothing in src/ or codes/ calls it, and no test
  exercises it.
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

End[];

EndPackage[];
