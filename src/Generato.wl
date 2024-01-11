(* ::Package:: *)

(* Generato.wl, load all packages *)

(* (c) Liwei Ji, 01/2024 *)

(*****************)

(* Load Packages *)

(*****************)

<<Basic.wl

Print["------------------------------------------------------------"];
Print["Package Generato`Basic`, {2024, 1, 11}"];
Print["------------------------------------------------------------"];

<<Component.wl

Print["------------------------------------------------------------"];
Print["Package Generato`Component`, {2024, 1, 11}"];
Print["------------------------------------------------------------"];

<<Varlist.wl

Print["------------------------------------------------------------"];
Print["Package Generato`Varlist`, {2024, 1, 11}"];
Print["------------------------------------------------------------"];

<<Interface.wl

Print["------------------------------------------------------------"];
Print["Package Generato`Interface`, {2024, 1, 11}"];
Print["------------------------------------------------------------"];

Print[""];

(***********)

(* Options *)

(***********)

(* Setup for xAct official modules  *)

$DefInfoQ = False;

$CommuteCovDsOnScalars = True;

$ExtrinsicKSign = -1;

$AccelerationSign = -1;

$CVVerbose = False;

$PrePrint = ScreenDollarIndices;(* replace internal dummies by new non-dollar dummies for output *)

SetOptions[ToCanonical, UseMetricOnVBundle -> None]; (* do not use metric when ToCanonical *)

(* Turn off error message *)

Off[Part::pkspec1];

(*Off[Part::partd];*)
