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

(**************************************)
(* Attached Cells                     *)
(**************************************)

(*
    Create an attached cell popup menu which is attached to the box which is currently
    being evaluated.
*)
AttachedPopupMenu[
    label_,
    contentsFunction_,
    parentPosition_ : {2, {Left, Top}},
    childPosition_ : {Right, Top},
    buttonOpts : OptionsPattern[]
] := With[{
    loadOrFail = $HeldLoadOrFail,
    po = Unevaluated@PersistentValue["CG:Organizer:AttachedCommandDropdown", "FrontEndSession"]
},
    Print["CALLED AttachedPopupMenu"];

    Button[
        label,
        (
            ReleaseHold[loadOrFail];

            If[MatchQ[po, _CellObject] && !FailureQ[NotebookRead[po]],
                NotebookDelete[po];
                po = None;
                ,
                po = makePopupAttachedCell[
                    contentsFunction[(NotebookDelete[po]; po = None) &],
                    parentPosition,
                    childPosition
                ];
            ];
        ),
        Method -> "Queued",
        Alignment -> Center,
        Background -> Lighter@Orange,
        ImageSize -> Full
    ]
]

makePopupAttachedCell[contents_, parentPosition_, childPosition_] := With[{
    parent = EvaluationBox[]
},
    FrontEndExecute[FrontEnd`AttachCell[
        (*thing you're attaching to*)
        parent,
        (* cell expression *)
        Cell[
            BoxData @ ToBoxes @ contents,
            "DialogLabelText",
            Background -> GrayLevel[0.9]
        ],
        (* distance, position on parent *)
        parentPosition,
        (* position on child *)
        childPosition,
        "ClosingActions" -> {"EvaluatorQuit", "OutsideMouseClick", "ParentChanged"}
    ]]
];



EndPackage[]