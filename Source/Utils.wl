BeginPackage["Organizer`Utils`"]

AttachedPopupMenu
NotebookProcess

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

Begin["`Private`"]

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
    loadOrFail = $HeldLoadOrFail
},
    Button[
        label,
        (
            ReleaseHold[loadOrFail];

            toggleAttachedPopupCell[
                "CG:Organizer:AttachedCommandDropdown",
                label,
                contentsFunction,
                parentPosition,
                childPosition
            ];
        ),
        Method -> "Queued",
        Alignment -> Center,
        Background -> Lighter@Orange,
        ImageSize -> Full
    ]
]

toggleAttachedPopupCell[
    tag_?StringQ,
    label_,
    contentsFunction_,
    parentPosition_,
    childPosition_
] := With[{
    po = Unevaluated[PersistentValue[tag, "FrontEndSession"]]
},
Module[{
    isOpen
},
    isOpen = MatchQ[Evaluate[po], CellObject[___]] && !FailureQ[NotebookRead[Evaluate[po]]];

    If[isOpen,
        NotebookDelete[Evaluate[po]];
        po = None;
        ,
        po = makePopupAttachedCell[
            contentsFunction[(NotebookDelete[Evaluate[po]]; po = None) &],
            parentPosition,
            childPosition
        ];
    ];
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
        "ClosingActions" -> {"EvaluatorQuit", (*"OutsideMouseClick",*) "ParentChanged"}
    ]]
];

(**************************************)
(* Notebook Processing                *)
(**************************************)

(* Open the notebook at `path` for processing using `callback`.

	If the notebook at `path` is not currently open, it will not become visible.
	If the notebook at `path` is currently open, it will remain visible.
*)
NotebookProcess[path_?StringQ, callback_] := Module[{
	nbObj,
	isAlreadyOpen,
	result
},
	isAlreadyOpen = notebookAtPathIsOpen[path];

	(* `Visible -> False` so we don't overload the user by opening a bunch of notebooks
	   they didn't actually want to see. This also prevents a subtle annoyance: if you
	   have multiple desktops (not multiple monitors, but multiple "virtual" desktop
	   spaces), when `nbObj` is already open on a different desktop, the screen will
	   quickly swipe over to it, and away from the All Queues notebook which is being
	   generated.
	*)
	nbObj = NotebookOpen[path, Visible -> False];
	If[FailureQ[nbObj],
		Return[nbObj];
	];

	(* If the user already had the notebook open, quickly make it visible again. This
	   happens quickly enough that I haven't noticed any visual "flickering". *)
	If[isAlreadyOpen,
		SetOptions[nbObj, Visible -> True]
	];

	(* Run the user code on the open notebook. *)
	result = callback[nbObj];

	(* Only close the notebook if it was not already open before the user pressed the
	"Show Queues" button. *)
	If[!isAlreadyOpen,
		NotebookClose[nbObj, Interactive -> True];
	];

	result
]

notebookAtPathIsOpen[path_?StringQ] := AnyTrue[
	Notebooks[],
	Function[nb, Module[{name},
		name = Association[NotebookInformation[nb]]["FileName"];
		If[MissingQ[name],
			Return[False, Module];
		];

		name = Replace[
			name,
			FrontEnd`FileName[{parts___}, name_, ___] :> FileNameJoin[{parts, name}]
		];
		name == path
	]]
]


End[]

EndPackage[]