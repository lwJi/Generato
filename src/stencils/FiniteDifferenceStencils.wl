(* ::Package:: *)

(* stencils.wl *)

(* (c) Liwei Ji, 01/2025 *)

BeginPackage["Generato`FiniteDifferenceStencils`"];

Print["------------------------------------------------------------"];

Print["Package Generato`FiniteDifferenceStencils`, {2025, 1, 21}"];

Print["------------------------------------------------------------"];

GetFiniteDifferenceCoefficients::usage = "GetFiniteDifferenceCoefficients[sample, order] return coeffients of finite difference stencils.";

GetCenteringStencils::usage = "GetCenteringStencils[ord] return the centering finite difference stencils of order ord.";

Begin["`Private`"];

(* Data *)

$CenteringStencils = <|
  2 -> {-1, 0, 1},
  4 -> {-2, -1, 0, 1, 2},
  6 -> {-3, -2, -1, 0, 1, 2, 3},
  8 -> {-4, -3, -2, -1, 0, 1, 2, 3, 4}
|>;

(* Function *)

GetCenteringStencils[ord_?IntegerQ] :=
  Return[$CenteringStencils[ord]];

GetCoefficients[sample_?ListQ, values_?ListQ] := Module[{numPoints, equations, coeffs},
  (* Number of points in the sample *)
  numPoints = Length[sample];

  (* Create coefficient symbols c1, c2, ..., cn *)
  coeffs = Table[Subscript[Global`c, sample[[i]]], {i, 1, numPoints}];

  (* Generate the system of equations based on the sample and values *)
  equations = Table[Sum[coeffs[[i]] If[j==0, 1, sample[[i]]^j], {i, 1, numPoints}] == values[[j+1]], {j, 0, numPoints-1}];

  (* Solve for the coeffs, return the first solution *)
  Solve[equations, coeffs][[1]]
];

Protect[GetCoefficients];

GetFiniteDifferenceCoefficients[sample_?ListQ, order_?IntegerQ] := Module[{numPoints, values},
  (* Number of points in the sample *)
  numPoints = Length[sample];

  (* Ensure the sample has enough points for the desired order *)
  If[numPoints < order + 1,
    Return[Message[GetFiniteDifferenceCoefficients::shortSample]];
  ];

  (* Generate values array for finite difference coefficients *)
  values = Join[ConstantArray[0, order], {order!}, ConstantArray[0, numPoints - order - 1]];

  GetCoefficients[sample, values]
];

GetFiniteDifferenceCoefficients::shortSample = "The 'sample' list must have at least two elements.";

Protect[GetFiniteDifferenceCoefficients];

(* calculate the coefficients used in Derivs thorn when use upwind derivative:
   https://github.com/EinsteinToolkit/CarpetX/blob/main/Derivs/src/derivs.hxx
*)
GetUpwindCoefficients[sample_?ListQ] := Module[{numPoints, numCoeff, upwind, dnwind, symmCoeffs, antiCoeffs, symm, anti, eqns, coeffs},
  (* Determine the number of points in the sample *)
  numPoints = Length[sample];

  (* Calculate upwind and downwind stencils using finite difference coefficients *)
  upwind = Sum[Subscript[c, sample[[i]]] Subscript[f, sample[[i]]], {i, 1, numPoints}] /. GetFiniteDifferenceCoefficients[sample, 1];
  dnwind = Sum[Subscript[c, -sample[[i]]] Subscript[f, -sample[[i]]], {i, 1, numPoints}] /. GetFiniteDifferenceCoefficients[-sample, 1];

  (* Calculate the number of coefficients (assuming symmetric samples) *)
  numCoeff = (numPoints + 1)/2;

  (* Define symmetric and anti-symmetric coefficient variables *)
  symmCoeffs = Table[Subscript[cs, i], {i, 1, numCoeff}];
  antiCoeffs = Table[Subscript[ca, i], {i, 0, numCoeff}];

  (* Build symmetric and anti-symmetric expressions *)
  symm = Sum[symmCoeffs[[i]] (Subscript[f, i] - Subscript[f, -i]), {i, 1, numCoeff}];
  anti = antiCoeffs[[1]] Subscript[f, 0] + Sum[antiCoeffs[[i + 1]] (Subscript[f, i] + Subscript[f, -i]), {i, 1, numCoeff}];

  (* Set up the system of equations by comparing coefficients *)
  eqns = Join[
    Table[Coefficient[symm - anti - upwind, Subscript[f, sample[[i]]]] == 0, {i, 1, numPoints}],
    Table[Coefficient[symm + anti - dnwind, Subscript[f, sample[[i]]]] == 0, {i, 1, numPoints}]
  ];

  (* Solve for the symmetric and anti-symmetric coefficients *)
  coeffs = Join[symmCoeffs, antiCoeffs];
  Solve[eqns, coeffs][[1]]
];

Protect[GetUpwindCoefficients];

End[];

EndPackage[];
