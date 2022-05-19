(* Definitions for Organizer 'Log'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Log`"]


CreateLogNotebook::usage = "CreateLogNotebook[name] creates a new 'Log' style Organizer notebook. Log notebooks can contain a project description and related links, daily entries, and TODOs."

$LogNotebookBackground = Blend[{Darker@Orange, Red}]

InstallLogNotebookDockedCells
InstallLogNotebookStyles

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`LogNotebookRuntime`"]

CreateLogNotebook[projName_?StringQ] := Try @ Module[{
	logNB
},
	logNB = CreateNotebook[];

	NotebookWrite[logNB, Cell[projName, "Title"] ];
	NotebookWrite[
		logNB,
		Cell[
			"Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}],
			"Subtitle"
		]
	];

	NotebookWrite[logNB, Cell["Context", "Chapter"] ];

	NotebookWrite[logNB, Cell["Daily", "Chapter"] ];
	NotebookWrite[logNB, Cell[DateString[Now, {"MonthName", " ", "Year"}], "Subsection"] ];
	NotebookWrite[logNB, Cell[DateString[Now, {"DayName", ", ", "MonthName", " ", "Day"}], "Subsubsection"] ];

	NotebookWrite[logNB, Cell["Queue", "Chapter"] ];

	Confirm @ InstallLogNotebookStyles[logNB];
	Confirm @ InstallLogNotebookDockedCells[logNB, projName];

	SetOptions[
		nbObj,
		TaggingRules -> {
			"CG:Organizer" -> {"DocumentType" -> "Log"}
		}
	];

	nbObj
]

(*----------------------------------------------------------------------------*)

(* This definition kept for backwards compatibility, and potential future customization. *)
InstallLogNotebookStyles[nb_NotebookObject] :=
	InstallNotebookStyles[nb]

(*----------------------------------------------------------------------------*)

InstallLogNotebookDockedCells[nbObj_, projName_?StringQ] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	buttonOptions,
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	openFolderButton, row, cell
},
	(* Options shared by all buttons in the toolbar *)
	buttonOptions = Sequence[
		$ButtonBarOptions,
		Background -> $LogNotebookBackground,
		ImageMargins -> {{10,10},{10,10}}
	];

	newTodayTodoButton = Button[
		IconButtonContent[GetIcon["CalendarWithPlus"], "Insert new TODO item for today"],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoForToday[SelectedNotebook[]];
		),
		Background -> $LogNotebookBackground,
		$ButtonBarOptions
	];

	newTodoAtTopOfQueueButton = Button[
		IconButtonContent[
			GetIcon["UnfinishedTodoList"],
			"Insert new TODO item at the top of the Queue"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoAtTopOfQueue[SelectedNotebook[]];
		),
		Background -> $LogNotebookBackground,
		$ButtonBarOptions
	];

	openFolderButton = Button[
		IconButtonContent[
			GetIcon["OpenFolder"],
			"Open the folder containing the current Log notebook"
		],
		Function[
			Switch[$OperatingSystem,
				"MacOSX",
					RunProcess[{"open", NotebookDirectory[]}],
				_,
					Confirm @ FailureMessage[
						Organizer::error,
						"Unhandled $OperatingSystem for opening folder: ``",
						{InputForm[$OperatingSystem]}
					];
			]
		],
		buttonOptions
	];

	row = Row[{
		(* Make the Log.nb title a hidden button which opens the organizer palette. This
		is a quick and convenient way to access the palette without needing to keep
		it open all of the time. *)
		Button[
			Style[Pane[projName, ImageMargins -> 10], "Subchapter", White],
			(
				ReleaseHold[loadOrFail];
				OpenOrganizerPalette[]
			),
			Appearance -> None
		],
		Row[{
			MakeNewTodoButton[$LogNotebookBackground],
			newTodayTodoButton,
			newTodoAtTopOfQueueButton
		}, ImageMargins -> 10],
		Confirm @ MakeLinkButtonRow[Background -> $LogNotebookBackground],
		openFolderButton,
		Confirm @ MakeColorPickerButtonGrid[]
	}];

	cell = Cell[
		BoxData[ToBoxes@row],
		Background -> $LogNotebookBackground
	];

	SetOptions[nbObj, DockedCells->{cell}]
]
]


End[]

EndPackage[]