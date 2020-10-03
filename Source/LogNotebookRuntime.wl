(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["Organizer`LogNotebookRuntime`", {
	"Organizer`"
}]


(* ::Text:: *)


(* ::Subsubsection:: *)
(*TODOs*)


fCreateTodoCell[] := Module[{input, row},
	input = InputField["", String,
		Appearance -> "Frameless",
		ImageSize -> {600, Automatic},
		FieldHint -> "Empty TODO",
		BaseStyle -> {
			"Text",
			FontWeight -> Plain
		}
	];
	row = Row[{(*Checkbox[False]*)Checkbox[1, {1,2,3}], input}];

	Cell[BoxData @ ToBoxes @ row, "Text", CellMargins -> {{66, 0}, {0, 1}}]
]


fInsertTodoAfterSelection[] := Module[{newCell, nb},
	newCell = fCreateTodoCell[];
	nb = EvaluationNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


(* ::Subsubsection:: *)
(*Links*)


fCreateHyperlinkCell[] := Module[{filepath, label},
	filepath = SystemDialogInput["FileOpen"];
	If[!StringQ[filepath],
		Throw[StringForm["Invalid file path: ``", filepath]];
	];

	label = FileNameTake[filepath, -1];
	MessageDialog[{filepath, label}];

	Cell[
		BoxData @ ToBoxes @ Framed[
			Hyperlink[Style[label, Bold], filepath],
			RoundingRadius -> 5,
			Background -> LightBlue,
			FrameStyle -> Directive[Thick, Darker@Green]
		],
		CellMargins -> {{66, 0}, {0, 1}}
	]
]


fInsertLinkAfterSelection[] := Module[{newCell, nb},
	newCell = fCreateHyperlinkCell[];
	If[FailureQ @ newCell,
		Return[$Failed];
	];

	nb = EvaluationNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


fGetDraggedHyperlink[] := Module[{path, res, data, hyperlink},
	(* TODO: Make path cross-platform. *)
	If[$SystemID =!= "MacOSX-x86-64",
		Throw["Cannot get dragged link on non-MacOSX platforms."];
	];

	path = FileNameJoin[{$HomeDirectory, "Desktop/dragged_link.html"}];
	CreateFile[path];
	RunProcess[{"open", "-a", "TextEdit", path}];

	(* Block on a dialog window until the user clicks to proceed.
	prevents us from reading from `dragged_link.html` before the
	user has actually had time to drag/paste anything in. *)
	res = DialogInput[DialogNotebook[{
		Button[
			Style["Finished Dragged Link", 30],
			DialogReturn[1],
			Background -> LightGreen,
			ImageMargins -> {{20,20},{30,20}},
			FrameMargins -> 120
		]
	}]];

	data = Import[path, {{"Hyperlinks", "Plaintext"}}];

	DeleteFile[path];

	hyperlink = Replace[data, {
		{{link_}, label_} :> Hyperlink[Style[label, 12], URL[link]],
		_ :> Throw[StringForm[
			"Dragged link did not have the expected format after Import: ``",
			data
		]]
	}];

	Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"]
]


fInsertDraggedHyperlink[] := Module[{newCell, nb},
	newCell = fGetDraggedHyperlink[];

	nb = EvaluationNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


(* ::Chapter:: *)
(*New NB setup*)


fInstallLogNotebookDockedCells[nbObj_, projName_?StringQ] := Module[{
	buttonOptions, newTODObutton, newFileLinkButton, newDraggedLinkButton,
	openFolderButton, row, cell
},
	(* Options shared by all buttons in the toolbar *)
	buttonOptions = Sequence[
		Background -> Blend[{Darker@Orange,Red}],
		ContentPadding -> None,
		ImageMargins -> {{10,10},{10,10}},
		FrameMargins -> 10
	];

	newTODObutton = Button[
		"New TODO",
		fInsertTodoAfterSelection[],
		buttonOptions
	];

	newFileLinkButton = Button[
		"File Link",
		fInsertLinkAfterSelection[],
		buttonOptions,
		Method -> "Queued"
	];

	newDraggedLinkButton = Button[
		"Dragged Link",
		fInsertDraggedHyperlink[],
		buttonOptions,
		Method -> "Queued"
	];

	openFolderButton = Button[
		"Open Folder",
		Function[
			Switch[$SystemID,
				"MacOSX-x86-64",
					RunProcess[{"open", NotebookDirectory[]}],
				_,
					Throw[StringForm["Unhandled $SystemID: ``", $SystemID]]
			]
		],
		buttonOptions
	];

	row = Row[{
		Style[Pane[projName, ImageMargins -> 10], "Subchapter", White],
		newTODObutton,
		newFileLinkButton,
		newDraggedLinkButton,
		openFolderButton
	}];

	cell = Cell[
		BoxData[ToBoxes@row],
		Background -> Blend[{Darker@Orange,Red}]
	];

	SetOptions[nbObj, DockedCells->{cell}]
]


EndPackage[]
