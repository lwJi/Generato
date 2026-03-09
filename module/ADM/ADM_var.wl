(* ::Package:: *)

(* ADM_var.wl *)

(* (c) Liwei Ji, 03/2026 *)

(*****************)

(* ADM Variables *)

(*****************)

ADMVarlist =
  GridTensors[
    {ADMgam[-i, -j], Symmetric[{-i, -j}], PrintAs -> "\!\(\*SuperscriptBox[\(\[Gamma]\), \((ADM)\)]\)"},
    {ADMexK[-i, -j], Symmetric[{-i, -j}], PrintAs -> "\!\(\*SuperscriptBox[\(K\), \((ADM)\)]\)"},
    {ADMalpha[], PrintAs -> "\!\(\*SuperscriptBox[\(\[Alpha]\), \((ADM)\)]\)"},
    {ADMbeta[i], PrintAs -> "\!\(\*SuperscriptBox[\(\[Beta]\), \((ADM)\)]\)"}
  ];

(*************************************)

(* Time Derivatives of ADM Variables *)

(*************************************)

ADMdtVarlist =
  GridTensors[
    {ADMdtalpha[], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Alpha]\), \((ADM)\)]\)"},
    {ADMdtbeta[i], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Beta]\), \((ADM)\)]\)"}
  ];

