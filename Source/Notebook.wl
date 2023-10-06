(* Utilities for creating and working with notebooks managed by Organizer. *)

BeginPackage["ConnorGray`Organizer`Notebook`"]


InstallNotebookStyles::usage = "InstallNotebookStyles[nb] will set the StyleDefinitions notebook option to the styles used by Organizer."
$OrganizerStylesheet

InsertCellAfterSelection::usage = "InsertCellAfterSelection[cell] will insert cell after the current selection point in the current evaluation notebook."

MakeOrganizerDockedCells
MakeTitleBarCellBoxes
MakeNewTodoButton

WriteTodoAndSelect

(* These functions are part of the DockedCells toolbar. Renaming them is a
   backwards-incompatible change. *)
InsertTodoAfterSelection::usage = "InsertTodoAfterSelection[] insters a new TODO cell after the current selection point in the current selected notebook."

SetNotebookTaggingRules

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Toolbar`"]

(*====================================*)
(* TODO insertion                     *)
(*====================================*)

(*-----------------*)
(* UI construction *)
(*-----------------*)

MakeNewTodoButton[] := With[{
	loadOrFail = $HeldLoadOrFail
},
	MakeToolbarButtonBoxes[
		GetIcon["Plus"],
		"Insert Todo",
		"Insert new TODO after current selection",
		Function[
			ReleaseHold[loadOrFail];
			InsertTodoAfterSelection[];
		]
	]
]

(*--------------*)
(* Runtime code *)
(*--------------*)

InsertTodoAfterSelection[] := Module[{nb},
	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];

	WriteTodoAndSelect[nb]
]

WriteTodoAndSelect[nb_NotebookObject] := (
	(* NOTE:
		The `Placeholder` here does NOT refer to the \[SelectionPlaceholder] used
		immediately previous — it refered to the Placeholder["Empty TODO"] value from
		createTodoCell[]. The two different placeholders are entirely unrelated.
		\[SelectionPlaceholder is part of how NotebookApply works. Ideally, we'd be able
		to do just:

			NotebookWrite[nb, createTodoCell[], Placeholder]

		and not even involve NotebookApply. This seems like it should work per
		NotebookWrite's documentation, but it doesn't.
	*)
	NotebookWrite[nb, createTodoCell[], All];
	NotebookApply[nb, "\[SelectionPlaceholder]", Placeholder];
)

(* NOTE:
	Old createTodoCell[] implementation. I used this for about two years, but it had
	serious flaws, not least of which being that it would sporadically crash the front end
	when cutting / pasting the TODO cells.

createTodoCell[] := Module[{input, row},
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
*)

(*
createTodoCell[] := Cell[
	BoxData[FormBox[
		RowBox[{ToBoxes@Checkbox[1, {1, 2, 3}], ToBoxes@Placeholder["Empty TODO"]}],
		"Text"
	]],
	"Text",
	CellMargins -> {{66, 0}, {0, 1}}
]
*)

(*
	This function must be kept in sync with replaceWithInertTodoCellAndSelect[]
*)
createTodoCell[] := Cell[
	BoxData @ ToBoxes @ Placeholder["Empty TODO"],
	"TODO",
	(* These cell event actions are triggered the first time a user interacts with the
	   cell. Their purpose is to:

		* Remove the Placeholder[..] BoxData content, and change the cell to be a
		  TextData[..]-type cell. This is necessary because the FE handles
		  Cell[BoxData[..]] and Cell[TextData[..]] very differently. Earlier versions of
		  the TODO cell attempted to combine box data and textual input, but they all had
		  weird minor issues, like spacing between apostrophes and other input characters
		  following the auto-whitespace behavior of "Input" cells.
		* Remove the event handlers themselves, so we're left behind with a very plain,
		  non-likely-to-crash cell which behaves like any other Text-style cell.
	*)
	CellEventActions -> {
		(* NOTE: "MouseDown" is used instead of "MouseClicked" because "MouseClicked"
		         is triggered by the click event from the "new todo" button being clicked,
				 causing the placeholder to be immediately replaced before the user has a
				 chance to see it. *)
		"MouseDown" :> replaceWithInertTodoCellAndSelect[""],
		"KeyDown" :> replaceWithInertTodoCellAndSelect[CurrentValue["EventKey"]]
	}
]

(*
	Replace the current EvaluationCell[] with an inert TODO cell, and move the cursor
	inside of the cell. This happens in response to click or key down event inside a
	non-inert TODO cell.

	This function must be kept in sync with createTodoCell[].
*)
replaceWithInertTodoCellAndSelect[content_?StringQ] := Module[{cell},
	cell = Cell[
		(* This LetterQ check prevents us from putting a non-printable character in the new
		cell, which can happen if the user types e.g. a down arrow immediately after
		creating the cell (CurrentValue["EventKey"] would be a non-printable character in
		that situtation). *)
		TextData[If[LetterQ[content], content, ""] ],
		(* This inherits styles from the StyleData["TODO", ..] in the Log.nb's
		StyleDefinitions property. *)
		"TODO"
	];

	NotebookWrite[EvaluationCell[], cell, All];
	SelectionMove[SelectedNotebook[], After, CellContents]
]

(*====================================*)

MakeOrganizerDockedCells[
	title_,
	documentType_?StringQ,
	background_
] := With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	toolbarRow,
	titleCell,
	toolbarCell
},
	toolbarRow = GridBox[{{
		MakeNewTodoButton[],
		Splice @ MakeLinkButtonRow[],
		ToBoxes @ MakeColorPickerButtonGrid[]
	}},
		GridBoxDividers -> {
			"Rows" -> {{None}},
			"ColumnsIndexed" -> {
				2 -> GrayLevel[0.7],
				5 -> GrayLevel[0.7]
			}
		},
		GridBoxSpacings -> {
			"Columns" -> {{0.2}},
			"ColumnsIndexed" -> {
				2 -> 1,
				5 -> 1
			}
		}
	];

	titleCell = Cell[
		BoxData @ MakeTitleBarCellBoxes[title, documentType],
		Background -> background
	];

	toolbarCell = Cell[
		BoxData[toolbarRow],
		Background -> GrayLevel[0.9],
		CellFrameMargins -> {{Inherited, Inherited}, {-1, 1}}
	];

	{
		titleCell,
		toolbarCell
	}
]]

(*====================================*)

MakeTitleBarCellBoxes[
	title_?StringQ,
	typeName_?StringQ,
	inserts : _?ListQ : {}
] := Module[{},
	GridBox[
		{{
			ItemBox[
				(*
					Make the title of every Organizer notebook a hidden button
					that opens the organizer palette. This is a quick and
					convenient way to access the palette without needing to keep
					it open all of the time.
				*)
				ToBoxes @ Button[
					Style[title, "Subchapter", White],
					(
						ReleaseHold[loadOrFail];
						OpenOrganizerPalette[]
					),
					Appearance -> None
				],
				(* Force this item to take up the maximum possible width. This
				   makes items that come after this appear aligned to the right
				   side of the notebook. *)
				ItemSize -> Fit
			],
			Splice[inserts],
			StyleBox[
				GridBox[
					{{
						StyleBox[
							"\"Organizer\"",
							FontFamily -> "Source Sans Pro",
							FontWeight -> "SemiBold",
							StripOnInput -> False
						],
						StyleBox[
							"\"" <> ToUpperCase[typeName] <> " NOTEBOOK\"",
							FontFamily -> "Source Sans Pro",
							FontTracking -> "SemiCondensed",
							FontVariations -> {"CapsType" -> "AllSmallCaps"},
							StripOnInput -> False
						]
					}},
					AutoDelete -> False,
					GridBoxDividers -> {
						"ColumnsIndexed" -> {2 -> RGBColor[1., 1., 1.]},
						"Rows" -> {{None}}
					},
					GridBoxItemSize -> {
						"Columns" -> {{Automatic}},
						"Rows" -> {{Automatic}}
					}
				],
				FontSize -> 12,
				FontColor -> GrayLevel[0.9]
			]
		}},
		GridBoxAlignment -> {
			"Columns" -> {{Left}},
			"ColumnsIndexed" -> {-1 -> Right},
			"Rows" -> {{Center}}
		}
	]
]

(*====================================*)

SetNotebookTaggingRules[
	nbObj_NotebookObject,
	documentType_?StringQ
] := Module[{},
	SetOptions[
		nbObj,
		TaggingRules -> {
			"CG:Organizer" -> {"DocumentType" -> documentType}
		}
	]
]

SetNotebookTaggingRules[args___] := Throw[Row[{
	"Unexpected arguments: ", {args}
}]]

(*====================================*)

InstallNotebookStyles[nb_NotebookObject] := With[{},
	SetOptions[nb,
		StyleDefinitions -> $OrganizerStylesheet
	];
]

$OrganizerStylesheet = FrontEnd`FileName[
	{"ConnorGray"},
	"Organizer.nb",
	CharacterEncoding -> "UTF-8"
]

(*====================================*)
(* Utilities                          *)
(*====================================*)

InsertCellAfterSelection[cell_] := Handle[_Failure] @ Module[{nb},
	If[cell === $Canceled,
		Return[$Canceled, Module];
	];

	If[FailureQ[cell] || !MatchQ[cell, Cell[__]],
		Raise[
			OrganizerError,
			"Error inserting cell after selection: ``",
			InputForm[cell]
		];
	];

	nb = EvaluationNotebook[];
	(* nb = SelectedNotebook[]; *)
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, cell];
]




End[]

EndPackage[]