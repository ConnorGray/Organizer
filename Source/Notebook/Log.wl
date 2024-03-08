(* Definitions for Organizer 'Log'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Log`"]


CreateLogNotebook::usage = "CreateLogNotebook[name] creates a new 'Log' style Organizer notebook. Log notebooks can contain a project description and related links, daily entries, and TODOs."

$LogNotebookBackground = Blend[{Darker@Orange, Red}]

InstallLogNotebookDockedCells
InstallLogNotebookStyles

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`LogNotebookRuntime`"]

CreateLogNotebook[projName_?StringQ] := Handle[_Failure] @ Module[{
	nb,
	nbObj
},
	nb = Notebook[{
		Cell[projName, "Title"],
		Cell[
			"Created " <> DateString[
				Now,
				{"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}
			],
			"Subtitle"
		],

		Cell["Context", "Chapter", Deletable -> False, Editable -> False],

		Cell["Daily", "Chapter", Deletable -> False, Editable -> False],
		Cell[DateString[Now, {"MonthName", " ", "Year"}], "Subsection"],
		Cell[DateString[Now, {"DayName", ", ", "MonthName", " ", "Day"}], "Subsubsection"],

		Cell["Queue", "Chapter", Deletable -> False, Editable -> False]
	},
		TaggingRules -> MakeNotebookTaggingRules["Log"]
	];

	nbObj = NotebookPut[nb, Visible -> False];

	RaiseConfirm @ InstallLogNotebookStyles[nbObj];
	RaiseConfirm @ InstallLogNotebookDockedCells[nbObj, projName];

	SetOptions[nbObj, Visible -> True];

	nbObj
]

(*----------------------------------------------------------------------------*)

(* This definition kept for backwards compatibility, and potential future customization. *)
InstallLogNotebookStyles[nb_NotebookObject] :=
	InstallNotebookStyles[nb]

(*----------------------------------------------------------------------------*)

InstallLogNotebookDockedCells[
	nbObj_,
	projName_?StringQ
] := Handle[_Failure] @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	openFolderButton,
	toolbarRow,
	titleCell,
	toolbarCell
},
	newTodayTodoButton = MakeToolbarButtonBoxes[
		GetIcon["CalendarWithPlus"],
		"Today",
		"Insert new TODO item for today",
		Function[
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoForToday[SelectedNotebook[]];
		]
	];

	newTodoAtTopOfQueueButton = MakeToolbarButtonBoxes[
		GetIcon["UnfinishedTodoList"],
		"Queue",
		"Insert new TODO item at the top of the Queue",
		Function[
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoAtTopOfQueue[SelectedNotebook[]];
		]
	];

	openFolderButton = MakeToolbarButtonBoxes[
		GetIcon["OpenFolder"],
		"Open Project Folder",
		"Open the folder containing the current Log notebook",
		Function[
			Switch[$OperatingSystem,
				"MacOSX",
					RunProcess[{"open", NotebookDirectory[]}],
				_,
					Raise[
						OrganizerError,
						"Unhandled $OperatingSystem for opening folder: ``",
						InputForm[$OperatingSystem]
					];
			]
		],
		Automatic,
		RGBColor["#F7C4A5"],
		GrayLevel[0.3]
	];

	newFileButton = MakeToolbarDropdownBoxes[
		(* FIXME: Use NewFile icon *)
		GetIcon["Plus"],
		"New File",
		"Create a new Organizer file associated with this project",
		<|
			"New Tasklist" :> (
				ReleaseHold[loadOrFail];
				HandleUIFailure @ HandleCreateNewFile[
					EvaluationNotebook[],
					"Tasklist"
				];
			),
			"New Design" :> (
				ReleaseHold[loadOrFail];
				HandleUIFailure @ HandleCreateNewFile[
					EvaluationNotebook[],
					"Design"
				]
			),
			"New Note" :> (
				ReleaseHold[loadOrFail];
				HandleUIFailure @ HandleCreateNewFile[
					EvaluationNotebook[],
					"Note"
				]
			),
			"New Bug Report" :> (
				ReleaseHold[loadOrFail];
				HandleUIFailure @ HandleCreateNewFile[
					EvaluationNotebook[],
					"BugReport"
				]
			)
		|>,
		"Queued",
		RGBColor["#F7C4A5"],
		GrayLevel[0.3]
	];

	toolbarRow = GridBox[{{
		MakeNewTodoButton[],
		newTodayTodoButton,
		newTodoAtTopOfQueueButton,
		Splice @ MakeLinkButtonRow[],
		ToBoxes @ MakeColorPickerButtonGrid[]
	}},
		GridBoxDividers -> {
			"Rows" -> {{None}},
			"ColumnsIndexed" -> {
				4 -> GrayLevel[0.7],
				7 -> GrayLevel[0.7]
			}
		},
		GridBoxSpacings -> {
			"Columns" -> {{0.2}},
			"ColumnsIndexed" -> {
				4 -> 1,
				7 -> 1
			}
		}
	];

	titleCell = Cell[
		BoxData @ MakeTitleBarCellBoxes[
			projName,
			"Log",
			{newFileButton, openFolderButton}
		],
		Background -> $LogNotebookBackground
	];

	toolbarCell = Cell[
		BoxData[toolbarRow],
		Background -> GrayLevel[0.9],
		CellFrameMargins -> {{Inherited, Inherited}, {-1, 1}}
	];

	SetOptions[nbObj, DockedCells -> {
		titleCell,
		toolbarCell	
	}]
]
]


End[]

EndPackage[]