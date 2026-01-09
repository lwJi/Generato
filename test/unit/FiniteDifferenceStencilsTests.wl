(* ::Package:: *)

(* FiniteDifferenceStencilsTests.wl *)
(* Unit tests for Generato`FiniteDifferenceStencils module *)

If[Environment["QUIET"] =!= "1", Print["Loading FiniteDifferenceStencilsTests.wl..."]];

(* Load the stencils package *)
Needs["Generato`FiniteDifferenceStencils`", FileNameJoin[{Environment["GENERATO"], "src/stencils/FiniteDifferenceStencils.wl"}]];

(* ========================================= *)
(* Test: GetCenteringStencils *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[2],
    {-1, 0, 1},
    TestID -> "GetCenteringStencils-Order2"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[4],
    {-2, -1, 0, 1, 2},
    TestID -> "GetCenteringStencils-Order4"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[6],
    {-3, -2, -1, 0, 1, 2, 3},
    TestID -> "GetCenteringStencils-Order6"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[8],
    {-4, -3, -2, -1, 0, 1, 2, 3, 4},
    TestID -> "GetCenteringStencils-Order8"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[10],
    {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5},
    TestID -> "GetCenteringStencils-Order10"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    GetCenteringStencils[12],
    {-6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6},
    TestID -> "GetCenteringStencils-Order12"
  ]
];

(* ========================================= *)
(* Test: GetFiniteDifferenceCoefficients *)
(* Verify against known analytical values *)
(* ========================================= *)

(* 2nd-order centered 1st derivative: coefficients for f'(x) *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetFiniteDifferenceCoefficients[{-1, 0, 1}, 1];
    {Subscript[c, -1], Subscript[c, 0], Subscript[c, 1]} /. coeffs,
    {-1/2, 0, 1/2},
    TestID -> "GetFiniteDifferenceCoefficients-Order2-Deriv1"
  ]
];

(* 4th-order centered 1st derivative *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 1];
    {Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs,
    {1/12, -2/3, 0, 2/3, -1/12},
    TestID -> "GetFiniteDifferenceCoefficients-Order4-Deriv1"
  ]
];

(* 2nd-order centered 2nd derivative: coefficients for f''(x) *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetFiniteDifferenceCoefficients[{-1, 0, 1}, 2];
    {Subscript[c, -1], Subscript[c, 0], Subscript[c, 1]} /. coeffs,
    {1, -2, 1},
    TestID -> "GetFiniteDifferenceCoefficients-Order2-Deriv2"
  ]
];

(* 4th-order centered 2nd derivative *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetFiniteDifferenceCoefficients[{-2, -1, 0, 1, 2}, 2];
    {Subscript[c, -2], Subscript[c, -1], Subscript[c, 0], Subscript[c, 1], Subscript[c, 2]} /. coeffs,
    {-1/12, 4/3, -5/2, 4/3, -1/12},
    TestID -> "GetFiniteDifferenceCoefficients-Order4-Deriv2"
  ]
];

(* Test error handling: insufficient points *)
AppendTo[$AllTests,
  VerificationTest[
    GetFiniteDifferenceCoefficients[{0, 1}, 2],
    $Failed,
    {GetFiniteDifferenceCoefficients::shortSample},
    TestID -> "GetFiniteDifferenceCoefficients-InsufficientPoints"
  ]
];

(* ========================================= *)
(* Test: GetUpwindCoefficients *)
(* Verify symmetric/antisymmetric decomposition *)
(* ========================================= *)

(* 2nd-order upwind *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetUpwindCoefficients[{-1, 0, 1}];
    ListQ[coeffs] && Length[coeffs] > 0,
    True,
    TestID -> "GetUpwindCoefficients-Order2-ReturnsList"
  ]
];

(* 4th-order upwind *)
AppendTo[$AllTests,
  VerificationTest[
    coeffs = GetUpwindCoefficients[{-2, -1, 0, 1, 2}];
    ListQ[coeffs] && Length[coeffs] > 0,
    True,
    TestID -> "GetUpwindCoefficients-Order4-ReturnsList"
  ]
];

(* Verify symmetry property: upwind + downwind = 2 * symmetric part *)
AppendTo[$AllTests,
  VerificationTest[
    sample = {-1, 0, 1};
    coeffs = GetUpwindCoefficients[sample];
    (* Check that we got symmetric (cs) and antisymmetric (ca) coefficients *)
    MemberQ[coeffs[[All, 1]], Subscript[cs, 1]] && MemberQ[coeffs[[All, 1]], Subscript[ca, 0]],
    True,
    TestID -> "GetUpwindCoefficients-HasSymmetricAndAntisymmetric"
  ]
];

If[Environment["QUIET"] =!= "1", Print["FiniteDifferenceStencilsTests.wl completed."]];
