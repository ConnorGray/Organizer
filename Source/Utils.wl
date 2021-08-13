BeginPackage["Organizer`Utils`"]

Try
FailureMessage
HandleUIFailure
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
(* Try                                *)
(**************************************)

Attributes[Try] = {HoldFirst}

Try[expr_] := Enclose[
	expr,
	err |-> If[MissingQ[err["Expression"]],
		err,
		err["Expression"]
	]
]

(**************************************)
(* FailureMessage                     *)
(**************************************)

Attributes[FailureMessage] = {HoldFirst}

(*
	Generate a message and a Failure object.

	`messageName` must be a message which requires only one template parameter. If
	`messageName` does not already have a definition, the definition `messageName = "``"`
	will be created automatically.
*)
FailureMessage[
	messageName_MessageName,
	formatStr_?StringQ,
	formatParams : _?ListQ | _?AssociationQ : {}
] := Try @ Module[{},
	If[!StringQ[messageName],
		messageName = "``";
	];

	(* Generate a message. Create and apply the template before generating the message,
	   because Message/StringForm does not support named template arguments from an
	   Association.

	   We don't use `string` in the returned Failure object, so that the
	   caller can pick the arguments out of the "MessageParameters" list programatically
	   if they desire.
	*)
	Module[{template, string},
		(* Use ToString instead of the default, which is TextString. TextString doesn't
		   have the behavior we want (try evaluating TextString[InputForm["Hello"]] or
		   TextString[Quantity[30, "Seconds"]]), which is to reproduce the input code
		   exactly, though also process wrappers like InputForm. *)
		template = StringTemplate[formatStr, InsertionFunction -> ToString];

		string = TemplateApply[template, formatParams];

		Message[messageName, string];
	];

	(* Return a Failure object. *)
	Failure[
		ToString[HoldForm[messageName]],
		<|
			"MessageTemplate" -> formatStr,
			"MessageParameters" -> formatParams
		|>
	]
]

(**************************************)
(* HandleUIFailure                    *)
(**************************************)

(*
	ErrorUIAbort
	UIErrorHandler
	ShowUIError
	HandleUIFailure
	EncloseErrorDialog
	ShowDialogOnError
*)

(*
	Handler for errors which bubble up to the root of a UI interaction evaluation.

	Aborts the evaluations defensively, as there is no higher-level construct to handle
	the error.
*)
HandleUIFailure[err_?FailureQ] := (
	errorDialog[err];
	Abort[];
)

HandleUIFailure[expr_] := expr

(*--------------------*)
(* errorDialog helper *)
(*--------------------*)

errorDialog[message_?StringQ] := MessageDialog[Row[{
	Style["Error: ", 14, Darker[Red]],
	message
}]]

errorDialog[form_StringForm] := errorDialog[ToString[form]]

errorDialog[
	Failure[_, KeyValuePattern[{
		"MessageTemplate" -> template_?StringQ,
		"MessageParameters" -> params_?ListQ
	}]]
] := errorDialog[StringForm[template, Sequence @@ params]]

errorDialog[err_?FailureQ] :=
	errorDialog[StringForm["An unknown Failure occurred: ``", InputForm[err]]]

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

Options[NotebookProcess] = {
	"ForceSave" -> False
}

(* Open the notebook at `path` for processing using `callback`.

	If the notebook at `path` is not currently open, it will not become visible.
	If the notebook at `path` is currently open, it will remain visible.
*)
NotebookProcess[path_?StringQ, callback_, OptionsPattern[]] := Module[{
	nbObj,
	isAlreadyOpen,
	forceSave = TrueQ[OptionValue["ForceSave"]],
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
		(* Use NotebookSave to avoid interactively prompting the user.

		   If NotebookProcess is being used to extract data from a notebook in a read-only
		   way, the default `"ForceSave" -> False` behavior will lead to an interactive
		   prompt being generated when NotebookClose is called, warning the user that
		   the notebook contents may have changed when they didn't intend them to.

		   However, if the caller knows they'll be editing the notebook, they can pass
		   `"ForceSave" -> True` to avoid the interactive prompt which would otherwise
		   appear when NotebookClose is called. *)
		If[forceSave,
			NotebookSave[nbObj];
		];
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