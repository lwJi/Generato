(* ::Package:: *)

(* Generato`Stencil`, finite-difference stencils: their sample points, their coefficients, and the names of grid functions at stencil points *)

(* (c) Liwei Ji, 01/2025 *)

BeginPackage["Generato`Stencil`", {"Generato`Context`"}];

If[!GetQuietMode[],
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Stencil`, {2025, 1, 21}"];
  System`Print["------------------------------------------------------------"];
];

GetFiniteDifferenceCoefficients::usage = "GetFiniteDifferenceCoefficients[sample, order] returns the finite difference coefficients for the given sample points and derivative order, as rules Subscript[StencilCoefficient, point] -> value.";

GetCenteringStencils::usage = "GetCenteringStencils[order] returns the centering stencil points for the given accuracy order.";

GetUpwindCoefficients::usage = "GetUpwindCoefficients[sample] returns the symmetric and antisymmetric upwind coefficients for the given sample points, as rules for Subscript[UpwindSymmetricCoefficient, i] and Subscript[UpwindAntisymmetricCoefficient, i].";

StencilCoefficient::usage = "Subscript[StencilCoefficient, point] is the coefficient of a sample point in the rules GetFiniteDifferenceCoefficients returns.";

UpwindSymmetricCoefficient::usage = "Subscript[UpwindSymmetricCoefficient, i] is the i-th coefficient of the symmetric part in the rules GetUpwindCoefficients returns, i from 1.";

UpwindAntisymmetricCoefficient::usage = "Subscript[UpwindAntisymmetricCoefficient, i] is the i-th coefficient of the antisymmetric part in the rules GetUpwindCoefficients returns, i from 0.";

GetGFIndexName::usage = "GetGFIndexName[index] converts integer index to GF index name (p1, m1, c0, etc.).";

GetGFIndexNameMix2nd::usage = "GetGFIndexNameMix2nd[index1, index2] creates mixed 2nd derivative GF index name.";

Begin["`Private`"];

(*
  The coefficients are symbols with a subscript. A caller reads a result by
  the three public heads; the value of the function at a sample point,
  Subscript[f, point], is used here only.
*)

(* Data *)

$CenteringStencils = <|Table[ord -> Table[i, {i, -ord/2, ord/2}], {ord, 2, 12, 2}]|>;

(* Function *)

GetCenteringStencils[ord_?IntegerQ] :=
  Return[$CenteringStencils[ord]];

GetCoefficients[sample_?ListQ, values_?ListQ] := Module[{numPoints, equations, coeffs},
  (* Number of points in the sample *)
  numPoints = Length[sample];

  (* Create coefficient symbols c1, c2, ..., cn *)
  coeffs = Table[Subscript[StencilCoefficient, sample[[i]]], {i, 1, numPoints}];

  (* Generate the system of equations based on the sample and values *)
  equations = Table[Sum[coeffs[[i]] If[j==0, 1, sample[[i]]^j], {i, 1, numPoints}] == values[[j+1]], {j, 0, numPoints-1}];

  (* Solve for the coeffs, return the first solution *)
  Solve[equations, coeffs][[1]]
];

Protect[GetCoefficients];

GetFiniteDifferenceCoefficients[sample_?ListQ, order_?IntegerQ] := Module[{numPoints, values},
  (* Number of points in the sample *)
  numPoints = Length[sample];

  (* Ensure the sample has enough points for the desired order.
     $Failed is the answer for a sample that is too short, not a failure of
     generation, so this site does not go through Stop. *)
  If[numPoints < order + 1,
    Message[GetFiniteDifferenceCoefficients::shortSample];
    Return[$Failed];
  ];

  (* Generate values array for finite difference coefficients *)
  values = Join[ConstantArray[0, order], {order!}, ConstantArray[0, numPoints - order - 1]];

  GetCoefficients[sample, values]
];

GetFiniteDifferenceCoefficients::shortSample = "The 'sample' list must have at least two elements.";

Protect[GetFiniteDifferenceCoefficients];

(* calculate the coefficients used in Derivs thorn when using upwind derivative:
   https://github.com/EinsteinToolkit/CarpetX/blob/main/Derivs/src/derivs.hxx
*)
GetUpwindCoefficients[sample_?ListQ] := Module[{numPoints, numCoeff, upwind, dnwind, symmCoeffs, antiCoeffs, symm, anti, eqns, coeffs},
  (* Determine the number of points in the sample *)
  numPoints = Length[sample];

  (* Calculate upwind and downwind stencils using finite difference coefficients *)
  upwind = Sum[Subscript[StencilCoefficient, sample[[i]]] Subscript[f, sample[[i]]], {i, 1, numPoints}] /. GetFiniteDifferenceCoefficients[sample, 1];
  dnwind = Sum[Subscript[StencilCoefficient, -sample[[i]]] Subscript[f, -sample[[i]]], {i, 1, numPoints}] /. GetFiniteDifferenceCoefficients[-sample, 1];

  (* Calculate the number of coefficients (assuming symmetric samples) *)
  numCoeff = (numPoints + 1)/2;

  (* Define symmetric and anti-symmetric coefficient variables *)
  symmCoeffs = Table[Subscript[UpwindSymmetricCoefficient, i], {i, 1, numCoeff}];
  antiCoeffs = Table[Subscript[UpwindAntisymmetricCoefficient, i], {i, 0, numCoeff}];

  (* Build symmetric and anti-symmetric expressions *)
  symm = Sum[symmCoeffs[[i]] (Subscript[f, i] - Subscript[f, -i]), {i, 1, numCoeff}];
  anti = antiCoeffs[[1]] Subscript[f, 0] + Sum[antiCoeffs[[i + 1]] (Subscript[f, i] + Subscript[f, -i]), {i, 1, numCoeff}];

  (* Set up the system of equations by comparing coefficients *)
  eqns = Join[
    Table[Coefficient[symm - anti - upwind, Subscript[f, sample[[i]]]] == 0, {i, 1, numPoints}],
    Table[Coefficient[symm + anti - dnwind, Subscript[f, sample[[i]]]] == 0, {i, 1, numPoints}]
  ];

  (* Solve for the symmetric and anti-symmetric coefficients, suppress underdetermined system warning *)
  coeffs = Join[symmCoeffs, antiCoeffs];
  Quiet[Solve[eqns, coeffs][[1]], {Solve::svars}]
];

Protect[GetUpwindCoefficients];

Protect[StencilCoefficient, UpwindSymmetricCoefficient, UpwindAntisymmetricCoefficient];

(*
  The two functions below name a grid function at a stencil point. They were
  shared by backends that are gone: nothing in src/ or codes/ calls them,
  and no test exercises them.
*)

(* GetGFIndexName - Converts integer index to GF index name *)
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

(* GetGFIndexNameMix2nd - Creates mixed 2nd derivative GF index name *)
GetGFIndexNameMix2nd[index1_?IntegerQ, index2_?IntegerQ] :=
  Module[{gfindex},
    gfindex = ToString[GetGFIndexName[index1]]
           <> ToString[GetGFIndexName[index2]];
    ToExpression[gfindex]
  ];

Protect[GetGFIndexNameMix2nd];

End[];

EndPackage[];
