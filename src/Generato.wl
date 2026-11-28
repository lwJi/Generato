(* ::Package:: *)

(* Generato.wl, load all packages *)

(* (c) Liwei Ji, 01/2024 *)

(*****************)

(* Load Packages *)

(*****************)

<<Failure.wl

<<Context.wl

<<Variable.wl

<<Component.wl

<<Equation.wl

<<Backend.wl

<<Emit.wl

<<Program.wl

<<Stencil.wl

If[!GetQuietMode[],
  System`Print[]
];

(***********)

(* Options *)

(***********)

(* Setup for xAct official modules  *)

$DefInfoQ = False;

$CommuteCovDsOnScalars = True;

$ExtrinsicKSign = -1;

$AccelerationSign = -1;

$CVVerbose = False;

System`$PrePrint = ScreenDollarIndices;(* replace internal dummies by new non-dollar dummies for output *)

SetOptions[ToCanonical, UseMetricOnVBundle -> None]; (* do not use metric when ToCanonical *)

(* Turn off error message *)

Off[Part::pkspec1];

(*Off[Part::partd];*)
