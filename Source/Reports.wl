BeginPackage["ConnorGray`Organizer`Reports`"]

Begin["`Private`"]

Needs["ConnorGray`Organizer`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Palette`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]

(**************************************)
(* Show Queue's Report                *)
(**************************************)

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
CreateQueuesReport[] := Module[{
	projects,
	settings,
	cells = {},
	nb, path,
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

	workspaceName = FileNameTake[RaiseConfirm @ WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day",
		" at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"
	}];

	AppendTo[cells, Cell["All Queues: " <> workspaceName, "Title"]];
	AppendTo[
		cells,
		Cell[
			"Created " <> timestamp,
			"Subtitle"
		]
	];

	cells = Join[cells, Map[
		Function[proj, Module[{
			queueChapterGroup
		},
			path = FileNameJoin[{
				RaiseConfirm @ CategoryDirectory[],
				proj,
				"Log.nb"
			}];

			queueChapterGroup = FirstCase[
				RaiseConfirmMatch[Get[path], _Notebook],
				Cell @ CellGroupData[
					{
						Cell["Queue", "Chapter", chapterOpts___],
						groupCellsRest___
					},
					_
				] :> (
					Cell @ CellGroupData[
						{
							Cell[
								(* Include the project name in the Queue chapter
								   name to distinguish it from the Queue chapter
								   of all the other included projects. *)
								"Queue — " <> proj,
								"Chapter",
								chapterOpts,
								WholeCellGroupOpener -> True
							],
							groupCellsRest
						},
						(* Close the cell group by default so the user can see
						   all the chapter in the report at once initially. *)
						Closed
					]
				),
				(* TODO: Issue a warning when a Log notebook doesn't contain
					a Queue chapter? *)
				Nothing,
				Infinity
			];

			queueChapterGroup
		]],
		projects
	]];

	nb = NotebookPut[
		Notebook[cells],
		(* Disable editing. If the user wants to edit these queues, they should do it in
		the source notebook. *)
		Editable -> False,
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Style["All Queues: " <> workspaceName <> ": " <> timestamp],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		},
		(* Add style definitions so that copied TODO cells render properly. *)
		StyleDefinitions -> $OrganizerStylesheet
	];

	RaiseConfirm @ SetNotebookTaggingRules[nb, "GeneratedQueueReport"];

	nb
]


End[]

EndPackage[]