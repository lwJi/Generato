(* ::Package:: *)

(* ADM_var.wl *)

(* (c) Liwei Ji, 03/2026 *)

(*
    The file declares and names no storage. A script reads it after it has
    loaded Generato and defined a manifold with the indices i, j and k, and
    hands the lists below, or parts of them, to the binding of the storage
    it keeps them in.
*)

(*****************)

(* ADM Variables *)

(*****************)

ADMVars =
  DefTensors[
    {ADMgam[-i, -j], Symmetric[{-i, -j}], PrintAs -> "\!\(\*SuperscriptBox[\(\[Gamma]\), \((ADM)\)]\)"},
    {ADMexK[-i, -j], Symmetric[{-i, -j}], PrintAs -> "\!\(\*SuperscriptBox[\(K\), \((ADM)\)]\)"},
    {ADMalpha[], PrintAs -> "\!\(\*SuperscriptBox[\(\[Alpha]\), \((ADM)\)]\)"},
    {ADMbeta[i], PrintAs -> "\!\(\*SuperscriptBox[\(\[Beta]\), \((ADM)\)]\)"}
  ];

(****************************************)

(* Spatial Derivatives of ADM Variables *)

(****************************************)

ADMdVars =
  DefTensors[
    {ADMdgam[-i, -j, -k], Symmetric[{-i, -j}], PrintAs -> "\[PartialD]\[Gamma]"},
    {ADMdalpha      [-k], PrintAs -> "\[PartialD]\[Alpha]"},
    {ADMdbeta    [i, -k], PrintAs -> "\[PartialD]\[Beta]"}
  ];

(*************************************)

(* Time Derivatives of ADM Variables *)

(*************************************)

ADMdtVars =
  DefTensors[
    {ADMdtalpha[], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Alpha]\), \((ADM)\)]\)"},
    {ADMdtbeta[i], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Beta]\), \((ADM)\)]\)"}
  ];

