(* Utilities for creating and working with notebooks managed by Organizer. *)

BeginPackage["ConnorGray`Organizer`Notebook`"]


InstallNotebookStyles::usage = "InstallNotebookStyles[nb] will set the StyleDefinitions notebook option to the styles used by Organizer."

InsertCellAfterSelection::usage = "InsertCellAfterSelection[cell] will insert cell after the current selection point in the current evaluation notebook."

MakeNewTodoButton

WriteTodoAndSelect

(* These functions are part of the DockedCells toolbar. Renaming them is a
   backwards-incompatible change. *)
InsertTodoAfterSelection::usage = "InsertTodoAfterSelection[] insters a new TODO cell after the current selection point in the current selected notebook."


Begin["`Private`"]

Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Toolbar`"]

(*====================================*)
(* TODO insertion                     *)
(*====================================*)

(*-----------------*)
(* UI construction *)
(*-----------------*)

MakeNewTodoButton[background_] := With[{
	loadOrFail = $HeldLoadOrFail
},
	Button[
		IconButtonContent[GetIcon["Plus"], "Insert new TODO after current selection"],
		(
			ReleaseHold[loadOrFail];
			InsertTodoAfterSelection[];
		),
		Background -> background,
		$ButtonBarOptions
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

InstallNotebookStyles[nb_NotebookObject] := With[{
	todoDefinitions = Sequence[
		TaggingRules -> {"CG:Organizer" -> {"TODOCompletedQ" -> False}},
		LineSpacing -> {0.95, 0},
		CellMargins -> {{66, 0}, {2, 2}},
		CellFrame -> {{2, 0}, {0, 0}},
		CellFrameColor -> GrayLevel[0.7],
		CellFrameMargins -> 5,
		CellFrameLabelMargins -> 3,
		CellFrameLabels -> {
			{CreateCheckboxCell[], None},
			{None, None}
		}
	]
},
	SetOptions[nb,
		StyleDefinitions -> Notebook[{
			Cell[StyleData[StyleDefinitions -> "Default.nb"] ],
			Cell[StyleData["TODO", StyleDefinitions -> StyleData["Text"] ],
				todoDefinitions,
				"ReturnCreatesNewCell" -> True,
				(* If the user presses the Tab key or the '*', convert this cell to a
				   "TODO:Item" cell. *)
				StyleKeyMapping -> {"Tab" -> "TODO:Item", "*" -> "TODO:Item"}
			],
			Cell[StyleData["TODO:Item", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Item" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.8, 0.043, 0.008]
				],
				CellMargins -> {{81, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Tab" -> "TODO:Subitem", "*" -> "TODO:Subitem", "Backspace" -> "TODO"},

				todoDefinitions
			],
			Cell[StyleData["TODO:Subitem", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Subitem" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.8, 0.043, 0.008]
				],
				CellMargins -> {{105, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Tab" -> "TODO:Subsubitem", "*" -> "TODO:Subsubitem", "Backspace" -> "TODO:Item"},

				todoDefinitions
			],
			Cell[StyleData["TODO:Subsubitem", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Subsubitem" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.6, 0.6, 0.6]
				],
				CellMargins -> {{129, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Backspace" -> "TODO:Subitem"},

				todoDefinitions
			]
		}]
	];
]

(*------------------------------------*)

(* NOTE: Setting an explicit `Background -> White` here is required because CellFrameLabels
         inherit their styling from the parent cell, and we don't want the checkbox cell
		 to have a colored background. *)
CreateCheckboxCell[] := Cell[
	BoxData @ ToBoxes @ Checkbox[
		Dynamic[
			Or[
				(* Note: Check for the legacy, un-namespaced "TODOCompletedQ" tagging rule. *)
				(* TODO:
					These should only be present in my personal log notebooks; this
				    legacy tagging rule is not part of any shared release of Organizer.
					Once I've cleaned these out of my personal notebooks, this workaround
					should be removed. *)
				TrueQ @ CurrentValue[
					ParentCell@EvaluationCell[],
					{TaggingRules, "TODOCompletedQ"}
				],
				TrueQ @ CurrentValue[
					ParentCell@EvaluationCell[],
					{TaggingRules, "CG:Organizer", "TODOCompletedQ"}
				]
			],
			Function[val, Module[{
				cell
			},
				cell = ParentCell[EvaluationCell[]];

				(* Remove the legacy, un-namespaced "TODOCompletedQ" tagging rule to ensure
				   it isn't ambiguous when compared to the namespaced tagging rule. *)
				(* See TODO above. *)
				SetOptions[cell, TaggingRules -> Replace[
					CurrentValue[cell, TaggingRules],
					{most___, "TODOCompletedQ" -> _, rest___} :> {most, rest}
				]];

				CurrentValue[cell, {TaggingRules, "CG:Organizer", "TODOCompletedQ"}] = val;
			]]
		]
	],
	Background -> White
]


(*====================================*)
(* Utilities                          *)
(*====================================*)

InsertCellAfterSelection[cell_] := Try @ Module[{nb},
	If[cell === $Canceled,
		Return[$Canceled, Module];
	];

	If[FailureQ[cell] || !MatchQ[cell, Cell[__]],
		Confirm @ FailureMessage[
			Organizer::error,
			"Error inserting cell after selection: ``",
			{InputForm[cell]}
		];
	];

	nb = EvaluationNotebook[];
	(* nb = SelectedNotebook[]; *)
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, cell];
]




End[]

EndPackage[]