(* ::Package:: *)

(* Generato`Failure`, the one error path: a message with text, then a tagged throw *)

(* (c) Liwei Ji, 11/2026 *)

(*
  This file is loaded first, because the first caller of Stop is the setter
  validation in Context.wl. It prints no banner: it reads no setting, and the
  quiet setting is read by the files that load after it.
*)

BeginPackage["Generato`Failure`"];

Stop::usage = "Stop[symbol::tag, args...] issues the message symbol::tag with args, then ends the run by throwing $Failed to the tag GeneratoFailure.";

GeneratoFailure::usage = "GeneratoFailure is the tag of the throw Stop raises. Catch[expr, GeneratoFailure] gives $Failed when expr stops.";

Begin["`Private`"];

(*
  Stop - the one error function.
  Every error site in src/ and codes/ raises through it, so a failure is
  always a message followed by one kind of throw. Nothing here ends the
  kernel: src/Driver.wl catches the throw and sets the exit status, and a
  unit test catches it with Catch[expr, GeneratoFailure].
  The first argument is held, because a message name that has text
  evaluates to that text.
*)
SetAttributes[Stop, HoldFirst];

Stop[msg_MessageName, args___] := (Message[msg, args]; Throw[$Failed, GeneratoFailure]);

Protect[Stop, GeneratoFailure];

End[];

EndPackage[];
