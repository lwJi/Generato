(* ::Package:: *)

(* FiniteDifferenceStencilsTests.wl *)
(* Unit tests for Generato`FiniteDifferenceStencils module *)

Print["Loading FiniteDifferenceStencilsTests.wl..."];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* ========================================= *)
(* Test: GetCenteringStencils *)
(* ========================================= *)

VerificationTest[
  (* Order 2 should return {-1, 0, 1} *)
  GetCenteringStencils[2],
  {-1, 0, 1},
  TestID -> "GetCenteringStencils-Order2"
];

VerificationTest[
  (* Order 4 should return {-2, -1, 0, 1, 2} *)
  GetCenteringStencils[4],
  {-2, -1, 0, 1, 2},
  TestID -> "GetCenteringStencils-Order4"
];

VerificationTest[
  (* Order 6 should return {-3, -2, -1, 0, 1, 2, 3} *)
  GetCenteringStencils[6],
  {-3, -2, -1, 0, 1, 2, 3},
  TestID -> "GetCenteringStencils-Order6"
];

VerificationTest[
  (* Order 8 should return {-4, -3, -2, -1, 0, 1, 2, 3, 4} *)
  GetCenteringStencils[8],
  {-4, -3, -2, -1, 0, 1, 2, 3, 4},
  TestID -> "GetCenteringStencils-Order8"
];

VerificationTest[
  (* Order 12 should have 13 points *)
  Length[GetCenteringStencils[12]],
  13,
  TestID -> "GetCenteringStencils-Order12-Length"
];

(* ========================================= *)
(* Test: GetFiniteDifferenceCoefficients *)
(* ========================================= *)

VerificationTest[
  (* First derivative, order 2: coefficients should be -1/2, 0, 1/2 *)
  coeffs = GetFiniteDifferenceCoefficients[{-1, 0, 1}, 1];
  {Subscript[c, -1], Subscript[c, 0], Subscript[c, 1]} /. coeffs,
  {-1/2, 0, 1/2},
  TestID -> "GetFDCoefficients-FirstDeriv-Order2"
];

VerificationTest[
  (* Second derivative, order 2: coefficients should be 1, -2, 1 *)
  coeffs = GetFiniteDifferenceCoefficients[{-1, 0, 1}, 2];
  {Subscript[c, -1], Subscript[c, 0], Subscript[c, 1]} /. coeffs,
  {1, -2, 1},
  TestID -> "GetFDCoefficients-SecondDeriv-Order2"
];

VerificationTest[
  (* First derivative, order 4: known coefficients *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 1];
  {Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs,
  {1/12, -2/3, 0, 2/3, -1/12},
  TestID -> "GetFDCoefficients-FirstDeriv-Order4"
];

VerificationTest[
  (* Second derivative, order 4: known coefficients *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 2];
  {Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs,
  {-1/12, 4/3, -5/2, 4/3, -1/12},
  TestID -> "GetFDCoefficients-SecondDeriv-Order4"
];

VerificationTest[
  (* Coefficients should be returned as a list of rules *)
  coeffs = GetFiniteDifferenceCoefficients[{-1, 0, 1}, 1];
  MatchQ[coeffs, {__Rule}],
  True,
  TestID -> "GetFDCoefficients-ReturnsRules"
];

VerificationTest[
  (* Order 6 first derivative center coefficient should be 0 *)
  coeffs = GetFiniteDifferenceCoefficients[{-3, -2, -1, 0, 1, 2, 3}, 1];
  Subscript[c, 0] /. coeffs,
  0,
  TestID -> "GetFDCoefficients-Order6-CenterIsZero"
];

VerificationTest[
  (* Coefficients should sum to 0 for first derivative *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 1];
  Total[{Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs],
  0,
  TestID -> "GetFDCoefficients-FirstDeriv-SumToZero"
];

VerificationTest[
  (* Coefficients should sum to 0 for second derivative *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 2];
  Total[{Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs],
  0,
  TestID -> "GetFDCoefficients-SecondDeriv-SumToZero"
];

VerificationTest[
  (* First derivative coefficients should be antisymmetric *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 1];
  cm2 = Subscript[c, -2] /. coeffs;
  cp2 = Subscript[c, 2] /. coeffs;
  cm2 == -cp2,
  True,
  TestID -> "GetFDCoefficients-FirstDeriv-Antisymmetric"
];

VerificationTest[
  (* Second derivative coefficients should be symmetric *)
  coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 2];
  cm2 = Subscript[c, -2] /. coeffs;
  cp2 = Subscript[c, 2] /. coeffs;
  cm2 == cp2,
  True,
  TestID -> "GetFDCoefficients-SecondDeriv-Symmetric"
];

Print["FiniteDifferenceStencilsTests.wl completed."];
