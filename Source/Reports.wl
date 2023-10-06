BeginPackage["ConnorGray`Organizer`Reports`"]

Begin["`Private`"]

Needs["ConnorGray`Organizer`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Palette`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]

(**************************************)
(* Show Queue's Report                *)
(**************************************)

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
CreateQueuesReport[] := Module[{
	projects,
	settings,
	nb, path,
	cells,
	timestamp,
	workspaceName
},
	(*--------------------------------------------------------------------*)
	(* Present the user with a DialogInput to select the projects to view *)
	(*--------------------------------------------------------------------*)

	projects = RaiseConfirm @ Projects[];

	RaiseConfirmMatch[projects, {___?StringQ}];

	settings = DialogInput[{
		selectedProjects = projects
	},
		Column[{
			Panel[
				Column[{
					CheckboxBar[
						Dynamic[selectedProjects],
						projects,
						Appearance -> "Vertical"
					]
				}],
				"Queue's Report Settings"
			],
			DefaultButton["Generate",
				DialogReturn[<|"Projects" -> selectedProjects|>]
			]
		}]
	];

	projects = Replace[settings, {
		$Canceled :> Return[Null, Module],
		_?AssociationQ :> settings["Projects"],
		_ :> (
			Raise[
				OrganizerError,
				"Unexpected value returned from settings panel: ``",
				InputForm[settings]
			];
		)
	}];

	(*-----------------------*)
	(* Populate the notebook *)
	(*-----------------------*)

	nb = CreateNotebook[];

	workspaceName = FileNameTake[RaiseConfirm @ WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day",
		" at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"
	}];

	NotebookWrite[nb, Cell["All Queues: " <> workspaceName, "Title"]];
	NotebookWrite[
		nb,
		Cell[
			"Created " <> timestamp,
			"Subtitle"
		]
	];

	(* Add style definitions so that copied TODO cells render properly. *)
	InstallLogNotebookStyles[nb];

	SetOptions[nb,
		(* Disable editing. If the user wants to edit these queues, they should do it in
		the source notebook. *)
		Editable -> False,
		(* Add a temporary docked cell warning the user that the notebook is still having
		content copied into it. This is removed later. *)
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Row[{
					Style["Generating: ", Italic, GrayLevel[0.2]],
					Style["All Queues: " <> workspaceName <> ": " <> timestamp]
				}],
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> Lighter[Orange]
			]
		}
	];

	RaiseConfirm @ SetNotebookTaggingRules[nb, "GeneratedQueueReport"];

	Scan[
		Function[proj,
			path = FileNameJoin[{
				RaiseConfirm @ CategoryDirectory[],
				proj,
				"Log.nb"
			}];

			cells = CellsFromChapterInNB[path, "Queue"];

			cells = Replace[
				cells,
				{Cell["Queue", "Chapter", props___], rest___} :> {Cell["Queue — " <> proj, "Chapter", props], rest}
			];

			NotebookWrite[nb, cells];
		],
		projects
	];

	(* Remove the warning docked cell -- the notebook is now complete. *)
	SetOptions[nb,
		(* Add a temporary docked cell warning the user that the notebook is still having
		content copied into it. This is removed later. *)
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Style["All Queues: " <> workspaceName <> ": " <> timestamp],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		}
	];

	SelectionMove[First[Cells[nb]], Before, Cell, AutoScroll -> True];
]


End[]

EndPackage[]