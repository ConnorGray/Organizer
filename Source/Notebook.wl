(* Utilities for creating and working with notebooks managed by Organizer. *)

BeginPackage["ConnorGray`Organizer`Notebook`"]


InstallNotebookStyles::usage = "InstallNotebookStyles[nb] will set the StyleDefinitions notebook option to the styles used by Organizer."

InsertCellAfterSelection::usage = "InsertCellAfterSelection[cell] will insert cell after the current selection point in the current evaluation notebook."


Begin["`Private`"]

Needs["ConnorGray`Organizer`Utils`"]

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