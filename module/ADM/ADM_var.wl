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

(****************************************)

(* Spatial Derivatives of ADM Variables *)

(****************************************)

ADMdVarlist =
  TempTensors[
    {ADMdalpha      [-k], PrintAs -> "\[PartialD]\[Alpha]"},
    {ADMdbeta    [i, -k], PrintAs -> "\[PartialD]\[Beta]"},
    {ADMdgam[-i, -j, -k], Symmetric[{-i, -j}], PrintAs -> "\[PartialD]\[Gamma]"}
  ];

(*************************************)

(* Time Derivatives of ADM Variables *)

(*************************************)

ADMdtVarlist =
  GridTensors[
    {ADMdtalpha[], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Alpha]\), \((ADM)\)]\)"},
    {ADMdtbeta[i], PrintAs -> "\!\(\*SubscriptBox[\(\[PartialD]\), \(t\)]\)\!\(\*SuperscriptBox[\(\[Beta]\), \((ADM)\)]\)"}
  ];

