BeginPackage["Organizer`Utils`"]


(*
    With[..] is used to embed this expression directly within the palette expression. This
    ensures that the check does not rely on any function from the Organizer` paclet itself.
*)
$HeldLoadOrFail = Hold[
    If[FailureQ @ PacletObject["Organizer"] || FailureQ @ Needs["Organizer`"],
        MessageDialog["The Organizer` paclet is either not installed, or failed to load."];
        Abort[];
    ];
]


EndPackage[]