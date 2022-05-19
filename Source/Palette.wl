BeginPackage["ConnorGray`Organizer`Palette`"]

OrganizerDirectory = NotebooksDirectory
WorkspaceDirectory
CategoryDirectory
RefreshOrganizerPalette
InitializeOrganizerPalette

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`LogNotebookRuntime`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]


NotebooksDirectory[] := Try @ Module[{dir, error},
    dir = PersistentValue["CG:Organizer:RootDirectory", "Local"];

    If[!DirectoryQ[dir],
		If[$SynchronousEvaluation,
			(* ChoiceDialog and SystemDialogInput don't work if this is a pre-emptive
			   evaluation, so skip trying to recover. This can happen when the hidden
			   button in the Log.nb docked title bar is clicked. *)
			Confirm @ FailureMessage[
				Organizer::error,
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				{dir}
			];
		];

		error = If[MissingQ[dir],
			"Setup: No Organizer root directory has been chosen. Please choose a directory to hold your Organizer notebooks."
			,
			ToString @ StringForm[
				"Error: saved Organizer root directory does not exist: ``. Would you like to pick a new root directory?",
				InputForm[dir]
			]
		];

		(* Ask the user if they would like to pick a new root directory. *)
		If[
			!ChoiceDialog[error],
			(* The user decided not to pick a root directory. Return an error. *)
			Confirm @ FailureMessage[
				Organizer::canceled,
				"Canceling operation. No Organizer Root directory is available."
			];
		];

		dir = SystemDialogInput["Directory", $HomeDirectory];

		If[!DirectoryQ[dir],
			If[dir === $Canceled,
				(* The user decided not to pick a root directory. Return an error. *)
				Confirm @ FailureMessage[
					Organizer::canceled,
					"Canceling operation. No Organizer Root directory is available."
				];
			];

			Confirm @ FailureMessage[
				Organizer::error,
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				{dir}
			];
		];

		(* If the user has chosen an empty directory as their root directory, offer to
		   create an initial/sample Organizer directory structure for them. This makes it
		   slightly easier for first-time users to jump straight into creating projects. *)
		If[FileNames[All, dir] === {},
			If[
				ChoiceDialog[Column[{
					ToString @ StringForm[
						"The chosen directory is empty: ``. Would you like to have an initial Organizer directory structure created automatically?",
						InputForm[dir]
					],
					"You may rename Workspace and Category directories at any time."
				}]],
				CreateDirectory[FileNameJoin[{dir, "Personal/Active/"}], CreateIntermediateDirectories -> True];
				CreateDirectory[FileNameJoin[{dir, "Work/Active/"}], CreateIntermediateDirectories -> True];

				PersistentValue["CG:Organizer:Workspace", "Local"] = "Work";
				PersistentValue["CG:Organizer:Category", "Local"] = "Active";
			];
		];

		PersistentValue["CG:Organizer:RootDirectory", "Local"] = dir;
    ];
    dir
]

(* Get the directory for the currently selected Workspace. *)
WorkspaceDirectory[] := Try @ Module[{organizerRoot, name, dir, workspaces},
	organizerRoot = Confirm @ NotebooksDirectory[];

	name = PersistentValue["CG:Organizer:Workspace", "Local"];
	workspaces = Confirm @ Workspaces[];

	Assert[ListQ[workspaces]];

	If[workspaces === {},
		Confirm @ FailureMessage[
			Organizer::error,
			"The Organizer root directory (``) does not contain any subdirectories. "
			<> "Please create a subdirectory and try again.",
			{InputForm[organizerRoot]}
		];
	];

	(* Handle some recoverable error conditions by prompting the user to pick a new
	   Workspace. *)
	Which[
		(* Handle the case that the saved Workspace name does not exist on disk. This
		   condition can happen when the user renames a Workspace. *)
		StringQ[name] && !MemberQ[workspaces, name], (
			name = ChoiceDialog[
				ToString @ StringForm[
					"Saved Workspace name '``' does not exist in the Organizer root directory: ``. "
					<> "Please choose another Workspace directory.",
					name, InputForm[organizerRoot]
				],
				Map[(# -> #)&, workspaces]
			];
			If[name === $Canceled,
				Confirm @ FailureMessage[
					Organizer::canceled,
					"Canceling operation. No Workspace directory is available."
				];
			];
			PersistentValue["CG:Organizer:Workspace", "Local"] = name;
		),
		(* Handle the case that the Workspace name is not a string. This condition can
		   occur when the user is setting up for the first time and has does not yet have
		   a saved Workspace name. *)
		!StringQ[name], (
			name = ChoiceDialog[
				"No Workspace is selected. Please choose one.",
				Map[(# -> #)&, workspaces]
			];
			If[name === $Canceled,
				Confirm @ FailureMessage[
					Organizer::canceled,
					"Canceling operation. No Workspace directory is available."
				];
			];
			PersistentValue["CG:Organizer:Workspace", "Local"] = name;
		)
	];

	dir = FileNameJoin[{organizerRoot, name}];
	If[!DirectoryQ[dir],
		Confirm @ FailureMessage[
			Organizer::error,
			"Workspace directory is unexpectedly invalid: ``",
			{InputForm[dir]}
		];
	];
	dir
];

CategoryDirectory[] := Try @ Module[{workspaceDir, name, dir, categories},
	workspaceDir = Confirm @ WorkspaceDirectory[];

	name = PersistentValue["CG:Organizer:Category", "Local"];
	categories = Confirm @ Categories[];

	Assert[ListQ[categories]];

	If[categories === {},
		Confirm @ FailureMessage[
			Organizer::error,
			"The current Workspace directory (``) does not contain any subdirectories. "
			<> "Please create a subdirectory and try again.",
			{InputForm[workspaceDir]}
		];
	];

	(* Handle some recoverable error conditions by prompting the user to pick a new
	   Category. *)
	Which[
		(* Handle the case that the saved Category name does not exist on disk. This
		   condition can happen when the user renames a Category. *)
		StringQ[name] && !MemberQ[categories, name], (
			name = ChoiceDialog[
				ToString @ StringForm[
					"Saved Category name '``' does not exist in the current Workspace directory: ``. "
					<> "Please choose another Category directory.",
					name, InputForm[workspaceDir]
				],
				Map[(# -> #)&, categories]
			];
			If[name === $Canceled,
				Confirm @ FailureMessage[
					Organizer::canceled,
					"Canceling operation. No Category directory is available."
				];
			];
			PersistentValue["CG:Organizer:Category", "Local"] = name;
		),
		(* Handle the case that the Category name is not a string. This condition can
		   occur when the user is setting up for the first time and has does not yet have
		   a saved Category name. *)
		!StringQ[name], (
			name = ChoiceDialog[
				"No Category is selected. Please choose one.",
				Map[(# -> #)&, categories]
			];
			If[name === $Canceled,
				Confirm @ FailureMessage[
					Organizer::canceled,
					"Canceling operation. No Category directory is available."
				];
			];
			PersistentValue["CG:Organizer:Category", "Local"] = name;
		)
	];

	dir = FileNameJoin[{workspaceDir, name}];
	If[!DirectoryQ[dir],
		Confirm @ FailureMessage[
			Organizer::error,
			"Category directory is unexpectedly invalid: ``",
			{InputForm[dir]}
		];
	];
	dir
]

Workspaces[] := Try @ subDirectoryNames[Confirm @ NotebooksDirectory[]]

(* Get a list of Categories which are a part of the current workspace. *)
Categories[] := Try @ subDirectoryNames[Confirm @ WorkspaceDirectory[]]

Projects[] := Try @ subDirectoryNames[Confirm @ CategoryDirectory[]]

subDirectoryNames[dir_?DirectoryQ] := Module[{names},
    names = FileNames[All, dir];
    names = Select[names, DirectoryQ];

    (* Get the last component of the file path; these are the Workspace/Category/Project names. *)
    names = Map[FileNameTake[#, -1]&, names];
    names = Select[names, !StringStartsQ[#, "."]&];

    names
]

(**************************************)
(* Interface Building Code            *)
(**************************************)

$PaletteWidth = 220;

OpenOrganizerPalette[] := Try @ Module[{
	organizerPacletLocation,
	paletteLocation,
	paletteNBObj
},
	(* Clear persistent TaggingRules (see PalettesMenuSettings). This seemed to be
	   necessary due to values which got cached, but after clearing them once, they
	   haven't come back -- keeping this around temporarily until I'm more confident
	   it won't be necessary. *)
	(* CurrentValue[$FrontEnd, System`PalettesMenuSettings] = DeleteCases[
		CurrentValue[$FrontEnd, System`PalettesMenuSettings],
		file_ /; StringContainsQ[file, "Organizer.nb"] -> _
	]; *)


	organizerPacletLocation = Confirm[PacletObject["ConnorGray/Organizer"]]["Location"];
	paletteLocation = FileNameJoin[{
		organizerPacletLocation,
		"FrontEnd", "Palettes", "Organizer.nb"
	}];

	If[!DirectoryQ[organizerPacletLocation] || !FileExistsQ[paletteLocation],
		Confirm @ FailureMessage[
			Organizer::error,
			"Unable to get location of Organizer paclet and/or palette: ``",
			{InputForm[organizerPacletLocation]}
		];
	];

	(* Check if the palette is already open by examining Notebooks[]. *)
	paletteNBObj = SelectFirst[
		Notebooks[],
		nb |-> SameQ[
			Quiet[NotebookFileName[nb], NotebookFileName::nosv],
			paletteLocation
		]
	];

	If[!MissingQ[paletteNBObj] && MatchQ[paletteNBObj, NotebookObject[__]],
		(* The palette is already open. Refresh its contents. *)
		Confirm @ RefreshOrganizerPalette[paletteNBObj];
		,
		(* Open the Organizer.nb. Whether it succeeds or fails, this operation returns Null. *)
		FrontEndTokenExecute["OpenFromPalettesMenu", "Organizer.nb"];
	];

	(* TODO: Return the NotebookObject[..] for the open palette? *)
]

InitializeOrganizerPalette[nb0_NotebookObject] := Try @ Module[{
	contents
},
	If[FailureQ[CategoryDirectory[]],
		(* If we can't get a CategoryDirectory[], make the contents of the palette be
		   a `Method -> "Queued" button for the user to click. This is a workaround for
		   the fact we cannot show dialog prompts to the user during a preemptive
		   evaluation (which notebook `Initialization :> (..)` is). *)
		Confirm @ SetOrganizerPaletteContent[nb0, Function[nb, (
			contents = Button[
				Style["Setup Organizer", 20],
				(
					(* CategoryDirectory will interactively prompt the user to pick new
					   values when the old values are bad in someway. Rely on this side-effect
					   to initialize the Organizer root directory state. *)
					HandleUIFailure[CategoryDirectory[]];

					(* Re-attempt initialization. *)
					HandleUIFailure[InitializeOrganizerPalette[nb]];
				),
				Method -> "Queued",
				Background -> Green,
				ImageSize -> Full
			];

			(* Delete everything in the notebook! *)
			SelectionMove[nb, All, Notebook];
			NotebookDelete[nb];

			(* Write the new content. *)
			NotebookWrite[
				nb,
				Cell[BoxData @ ToBoxes @ contents]
			];
		)]];
		,
		RefreshOrganizerPalette[nb0]
	];
]

RefreshOrganizerPalette[nb0_NotebookObject] := Try @ Module[{
	contents
},
	(* Echo["called RefreshOrganizerPalette:"]; *)

	SetOrganizerPaletteContent[
		nb0,
		Function[nb, (
			(* Compute the new contents for the palette. *)
			(* Do this *before* deleting the old contents to minimize the time that the palette
			is blank. *)
			contents = Confirm @ createOrganizerPalette[];

			(* Delete everything in the notebook! *)
			SelectionMove[nb, All, Notebook];
			NotebookDelete[nb];

			(* Write the new content. *)
			NotebookWrite[
				nb,
				Cell[BoxData @ ToBoxes @ contents]
			];
		)]
	]
]

SetOrganizerPaletteContent[nb_NotebookObject, callback_] := Try @ Module[{
	organizerPaletteQ, info,
	body, contents
},

	(*------------------------------------------------------------------------------*)
	(* Defensively check that this is the Organizer palette we're about to refresh. *)
	(*------------------------------------------------------------------------------*)

	(* If `nb` is somehow accidentally a user's notebook, we do NOT want to delete
	   everything in it and repopulate it with the Organizer palette content. Given the
	   sometimes murky behavior of EvaluationNotebook[] (which may have been called to get
	   `nb`) (especially in the presense of pre-emptive evaluations), it seems best to be
	   on guard against this potential bug. *)
	organizerPaletteQ = SameQ[
		CurrentValue[nb, {TaggingRules, "CG:Organizer", "DocumentType"}],
		"MainOrganizerPalette"
	];

	If[!organizerPaletteQ,
		info = Association[NotebookInformation[nb]];

		Confirm @ FailureMessage[
			Organizer::error,
			"CRITICAL ERROR: Unsafe attempt to refresh a notebook which was not the main "
			<> "Organizer palette. Doing so would delete all contents of the affected notebook. "
			<> "Notebook file path was: ``. Notebook title was: ``",
			{InputForm[info["FileName"]], InputForm[info["WindowTitle"]]}
		];
	];

	(*-------------------------------------------------------------------*)
	(* Delete the existing contents of the palette and (re-)populate it. *)
	(*-------------------------------------------------------------------*)

	(* Work around double Initialization bug. If we modify the contents of the notebook
	   during Initialization (happening right now), the Initialization code is run twice. *)
	SetOptions[nb, Initialization -> None];

	(* Make selection of the notebook contents possible. *)
	SetOptions[nb, Selectable -> True];


	Confirm @ callback[nb];


	(* Restore `Selectable -> False` *)
	SetOptions[nb, Selectable -> False];

	(* Force the notebook to resize to fit its content. *)
	SetOptions[nb, WindowSize -> All];

	CurrentValue[nb, {TaggingRules, "CG:Organizer", "DocumentType"}] = "MainOrganizerPalette";
]

createOrganizerPalette[] := Try @ With[{
    loadOrFail = $HeldLoadOrFail
}, Module[{
	categoryButton, categoryPicker, paletteContents
},
	categoryButton[category_?StringQ] := Button[
		category,
		(
			ReleaseHold[loadOrFail];
			PersistentValue["CG:Organizer:Category"] = category;

			(* OpenOrganizerPalette[] automatically refreshes the organizer palette. *)
			HandleUIFailure @ ConnorGray`Organizer`OpenOrganizerPalette[]
		),
		FrameMargins -> 2,
		Appearance -> "Palette",
		ImageSize -> Medium
	];

	categoryPicker = Pane[
		Grid[{Map[categoryButton, Confirm @ Categories[]]}, Spacings -> 0],
		$PaletteWidth,
		ImageSizeAction -> "Scrollable",
		Scrollbars -> {False, False}
	];

	paletteContents = Column[
		{
			Grid[
				{{
					Button[
						Style["New Project", 20],
						(
							ReleaseHold[loadOrFail];
							handleStartNewProject[];
						),
						Method -> "Queued",
						Background -> Green,
						ImageSize -> Full
					],
					AttachedPopupMenu[
						Style["\[CloverLeaf]", 25],
						Function[close,
							HandleUIFailure @ commandDropdownContents[close]
						]
					]
					(*
					Button[
						Style[Global`\[CloverLeaf], 25],
						(
							ReleaseHold[loadOrFail];
							openCommandDropdown[];
						),
						Method -> "Queued",
						Active -> False,
						Alignment -> Center,
						Background -> Lighter@Orange,
						ImageSize -> Full
					]
					*)
				}},
				ItemSize -> {{Scaled[0.75], Scaled[0.25]}},
				Spacings -> {0, 0}
			],
			Grid[Confirm @ buttonListToOpenActiveProjectLogs[], Spacings -> {0, 0}],
			categoryPicker
		}
		,
		Spacings -> 0.1
	];

	paletteContents = Pane[paletteContents, ImageSize -> $PaletteWidth];

	paletteContents
]]

commandDropdownContents[close_Function] := Try @ With[{
    loadOrFail = $HeldLoadOrFail
},
    Column[
        {
            Button[Style["Refresh", 20],
                (
                    ReleaseHold[loadOrFail];

					Assert[MemberQ[$Packages, "ConnorGray`Organizer`"]];

                    (* OpenOrganizerPalette[] automatically refreshes the palettte. *)
					HandleUIFailure @ ConnorGray`Organizer`OpenOrganizerPalette[]
                ),
                Method -> "Queued",
                Background -> LightBlue
            ],
            With[{
                choices = Map[
                    # :> (
                        ReleaseHold[loadOrFail];

                        PersistentValue["CG:Organizer:Workspace", "Local"] = #;
                        (* Set the category for the selected workspace. Because the
                           categories just happen to be sorted (by FileNames), "Active" is
                           typically the first category. There currently isn't a proper
                           concept of a "default" category for a workspace. *)
                        PersistentValue["CG:Organizer:Category"] = First[Categories[]];

						(* OpenOrganizerPalette[] automatically refreshes the palette. *)
						HandleUIFailure @ ConnorGray`Organizer`OpenOrganizerPalette[]
                    )&,
                    Confirm @ Workspaces[]
                ]
            },
                ActionMenu[
                    Style["Workspace ...", 18],
                    choices,
                    ImageSize -> Full
                ]
            ],
            Button[
                Style["Show Queues", 20],
                (
                    ReleaseHold[loadOrFail];

                    handleShowQueues[];
                ),
                Method -> "Queued",
                Background -> LightOrange
            ],
            Button[
                Style["Show Daily's", 20],
                (
                    ReleaseHold[loadOrFail];

                    HandleShowDailys[];
                ),
                Method -> "Queued",
                Background -> LightBlue
            ]
        }
    ]
]

buttonListToOpenActiveProjectLogs[] := Try @ Module[{projects, metaProjs},
    projects = Confirm @ Projects[];

    metaProjs = Sort[Select[projects, StringStartsQ["+"]]];
    projects = Sort[Complement[projects, metaProjs]];

    Map[
        Function[proj,
            With[{
                loadOrFail = $HeldLoadOrFail,
                path = FileNameJoin[{Confirm @ CategoryDirectory[], proj, "Log.nb"}]
            },
                If[FileExistsQ[path],
                    {
                        Pane[
							Button[
								Pane[
									Style[StringTrim[proj, "+"], 16],
									ImageSize -> Full,
									Alignment -> Center
								],
								(
									ReleaseHold[loadOrFail];
									Module[{nb},
										nb = NotebookOpen[path];
										If[MatchQ[nb, _NotebookObject],
											SetOptions[nb, Visible -> True];
										];
									]
								),
								Method -> "Queued",
								Background -> If[StringStartsQ[proj, "+"],
									Lighter@Lighter@Lighter@Blue,
									LightBlue
								]
							],
							ImageSize -> Full
						],
						Pane @ Button[Style["./", 16, Bold],
							(
								ReleaseHold[loadOrFail];
								RunProcess[{"open", FileNameDrop[path, -1]}];
							),
							Method -> "Queued",
							Background -> Lighter@Orange,
							ImageSize -> {30, 30}
						]
                    }
                    ,
                    {Panel[path, Background -> Darker@LightBlue]}
                ]
            ]
        ],
        Join[metaProjs, projects]
    ]
]

(******************************************************************************)
(* UI Event Handlers                                                          *)
(******************************************************************************)

handleStartNewProject[] := HandleUIFailure @ Try @ Module[{
    projName, dirPath, logNB
},
    projName = InputString[];
    If[!StringQ[projName],
        If[projName === $Canceled,
            Return[];
        ];
		Confirm @ FailureMessage[
			Organizer::error,
			"Invalid project name: ``",
			{InputForm[projName]}
		];
    ];

	dirPath = FileNameJoin[{Confirm @ CategoryDirectory[], projName}];
    If[FileExistsQ[dirPath],
		Confirm @ FailureMessage[
			Organizer::error,
			"File already exists at path ``.",
			{InputForm[dirPath]}
		];
    ];

    (* Make sure the icons are loaded *before* we modify the filesystem. *)
    LoadIcons[];

    CreateDirectory[dirPath];

	logNB = Confirm @ CreateLogNotebook[projName];

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];

    (* Refresh the organizer palette in-place. *)
    HandleUIFailure @ OpenOrganizerPalette[];
]

(**************************************)
(* Show Queue's Report                *)
(**************************************)

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
handleShowQueues[] := HandleUIFailure @ Try @ Module[{projects, settings, nb, path, cells, timestamp, workspaceName},

	(*--------------------------------------------------------------------*)
	(* Present the user with a DialogInput to select the projects to view *)
	(*--------------------------------------------------------------------*)

	projects = Projects[];

	Assert[MatchQ[projects, {___?StringQ}]];

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
			Confirm @ FailureMessage[
				Organizer::error,
				"Unexpected value returned from settings panel: ``",
				{InputForm[settings]}
			];
		)
	}];

	(*-----------------------*)
	(* Populate the notebook *)
	(*-----------------------*)

    nb = CreateNotebook[];

    workspaceName = FileNameTake[Confirm @ WorkspaceDirectory[], -1];

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

    Scan[
        Function[proj,
            path = FileNameJoin[{Confirm @ CategoryDirectory[], proj, "Log.nb"}];

            cells = Confirm @ cellsFromChapterInNB[path, "Queue"];

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

(**************************************)
(* Show Daily's Report                *)
(**************************************)

HandleShowDailys[] := HandleUIFailure @ Try @ Module[{
	settings,
	timePeriod,
	startDate,
	endDate,
	nb,
	workspaceName,
	timestamp,
	projects,
	cells
},
	(*---------------------------------------------------------*)
	(* Ask the user for the date range and projects to include *)
	(*---------------------------------------------------------*)

	projects = Projects[];

	Assert[MatchQ[projects, {___?StringQ}]];

	settings = DialogInput[{
		timePeriod,
		selectedProjects = projects
	},
		Column[{
			Panel[
				Column[{
					CheckboxBar[
						Dynamic[selectedProjects],
						projects,
						Appearance -> "Vertical"
					],
					PopupMenu[
						Dynamic[timePeriod],
						{"This Week", "Last Week", "Yesterday", "Past 7 Days", "Past 14 Days", "Past 30 Days"}
					]
				}],
				"Daily's Report Settings"
			],
			DefaultButton["Generate",
				DialogReturn[<|
					"TimePeriod" -> timePeriod,
					"Projects" -> selectedProjects
				|>]
			]
		}]
	];

	If[!AssociationQ[settings],
		(* PRE-COMMIT: Better error reporting. *)
		Return[$Failed]
	];

	timePeriod = Confirm @ Lookup[settings, "TimePeriod", $Failed];
	projects = Confirm @ Lookup[settings, "Projects", $Failed];

	Replace[timePeriod, {
		"Past 7 Days" :> (
			startDate = Today - Quantity[7, "Days"];
			endDate = Today;
		),
		"Past 14 Days" :> (
			startDate = Today - Quantity[14, "Days"];
			endDate = Today;
		),
		"Past 30 Days" :> (
			startDate = Today - Quantity[30, "Days"];
			endDate = Today;
		),
		"Last Week" :> Module[{daysOfLastWeek},
			daysOfLastWeek = Map[
				(* Subtract one day to get a Sunday-Sunday week instead of Monday-Monday. *)
				DatePlus[#, {-1, "Day"}] &,
				DayRange[
					DateValue[Today, "Week", DateObject] - Quantity[1, "Week"],
					DateValue[Today, "Week", DateObject]
				]
			];

			startDate = First[daysOfLastWeek];
			endDate = Last[daysOfLastWeek];

			Assert[startDate["DayName"] === Sunday];
			Assert[endDate["DayName"] === Sunday];
		],
		"This Week" :> Module[{daysOfThisWeek},
			(* Get the days of this week between the first Monday of the week and today. *)
			daysOfThisWeek = DayRange[
				DateValue[Today, "Week", DateObject],
				Today
			];

			startDate = First[daysOfThisWeek];
			endDate = Last[daysOfThisWeek];

			Assert[startDate["DayName"] === Monday];
		],
		"Yesterday" :> (
			startDate = Today - Quantity[1, "Day"];
			endDate = startDate;
		),
		_ :> Confirm[$Failed]
	}];

	Assert[DateObjectQ[startDate] && DateObjectQ[endDate]];
	Assert[startDate["Granularity"] === "Day" && endDate["Granularity"] === "Day"];

	(*---------------------------------*)
	(* Generate the All Daily's report *)
	(*---------------------------------*)

	nb = CreateNotebook[];

	workspaceName = FileNameTake[Confirm @ WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day", ", ", "Year",
		" at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"
	}];

	NotebookWrite[nb, Cell["All Daily's: " <> workspaceName, "Title"]];
	NotebookWrite[
		nb,
		Cell["Generated " <> timestamp, "Subtitle"]
	];
	NotebookWrite[
		nb,
		Cell[
			"Showing " <> TextString[startDate] <> " — " <> TextString[endDate] <> " (" <> timePeriod <> ")",
			"Subsubtitle"
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
					Style["All Daily's: " <> workspaceName <> ": " <> timestamp]
				}],
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> Lighter[Orange]
			]
		}
	];

	Scan[
		Function[proj,
			path = FileNameJoin[{Confirm @ CategoryDirectory[], proj, "Log.nb"}];

			cells = Confirm @ cellsFromChapterInNB[path, "Daily"];

			cells = Confirm @ Replace[
				filterDailyCellsByInterval[cells, DateInterval[{startDate, endDate}]],
				{
					$Failed :> Failure["ShowDailys", <|
						"MessageTemplate" -> "Unknown failure occurred while processing '``' project",
						"MessageParameters" -> {proj}
					|>],
					Failure[tag_, KeyValuePattern[{
						"MessageTemplate" -> template_?StringQ,
						"MessageParameters" -> params_?ListQ
					}]] :> Failure[tag, <|
						"MessageTemplate" -> "Error processing '``' project: " <> template,
						"MessageParameters" -> Join[{proj}, params]
					|>],
					err_?FailureQ :> err,
					result_ :> result
				}
			];

			cells = Replace[
				cells,
				{Cell["Daily", "Chapter", props___], rest___} :> {Cell["Daily — " <> proj, "Chapter", props], rest}
			];

			If[!MatchQ[cells, {___Cell}],
				Confirm[$Failed];
			];

            NotebookWrite[nb, cells];

			(* NotebookWrite[nb, cells]; *)
		],
		projects
	];

	(* Remove the warning docked cell -- the notebook is now complete. *)
	SetOptions[nb,
		(* Replace the temporary docked cell with a permanent one. *)
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Row[{
					Style["All Daily's: " <> workspaceName <> ": " <> timestamp],
					(* This notebook is not editable by default, to prevent confusion of
					   the generated content with the source content. Clicking this button
					   makes it possible to opt-in to editing, e.g. for the purpose of
					   editing or rearranging the Daily's report before sharing it. *)
					Button[
						"Make Editable",
						(
							SetOptions[EvaluationNotebook[], Editable -> True];
						)
					]
				}],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		}
	];

	SelectionMove[First[Cells[nb]], Before, Cell, AutoScroll -> True];
]

filterDailyCellsByInterval[
	cells0 : {___Cell},
	interval_DateInterval
] := Try @ Module[{
	cells = cells0,
	currentDate = None,
	updateCurrentDate
},
	(* Confirm that the granularity of the interval is Day. *)
	If[interval["Granularity"] =!= "Day",
		Confirm[Failure["DateFilter", <|
			"MessageTemplate" -> "Granularity of interval `` is not \"Day\"",
			"MessageParameters" -> {ToString[InputForm[interval]]}
		|>]];
	];

	(*----------------------------------------------*)
	(* updateSubsubsectionDate[] helper definitions *)
	(*----------------------------------------------*)

	updateCurrentDateFromSubsubsection[date0_?StringQ] := Module[{
		date = date0
	},
		If[!DateObjectQ[currentDate] || !MemberQ[{"Month", "Day"}, currentDate["Granularity"]],
			Confirm[Failure["DateFilter", <|
				"MessageTemplate" -> "Unknown current year when filtering Daily cells by date: ``",
				"MessageParameters" -> {InputForm[currentDate]}
			|>]];
		];

		(* TODO: Generalize these transformation rules as a user setting. *)
		date = FixedPoint[
			StringReplace[{
				StartOfString ~~ "[X] " ~~ rest___ ~~ EndOfString :> rest,
				StartOfString ~~ most___ ~~ "\[LongDash]" ~~ rest___ ~~ EndOfString :> most,
				StartOfString ~~ most___ ~~ Repeated[DigitCharacter, {4}] ~~ EndOfString :> most,
				WordBoundary ~~ day : Repeated[DigitCharacter, {1, 2}] ~~ ("st" | "nd" | "rd" | "th") ~~ WordBoundary :> day
			}],
			date
		];

		(* Ensure the date object is interpreted in the correct year. *)
		date = date <> " " <> ToString[currentDate["Year"]];

		currentDate = DateObject[
			{date, {"DayName", "MonthName", "Day", "Year"}},
			"Day"
		];
		If[!DateObjectQ[currentDate],
			Confirm[Failure["DateFilter", <|
				"MessageTemplate" -> "Cannot interpret Subsubsection as date: ``",
				"MessageParameters" -> {InputForm[date]}
			|>]];
		];
	];

	(*-------------------------*)
	(* Main Select filter loop *)
	(*-------------------------*)

	(* TODO:
		Optimize which cells are parsed using DateObject, by doing a crude
		replacement to limit by the "<Month> <Year>" Subsection cells. This is on a
		hunch that DateObject is probably fairly slow.
	*)

	(* Select all cells which describe a date within `interval`. *)

	(* NOTE:
		The code below handling inclusion of Section, Subsection, and Subsubsection cells
		uses DateOverlapsQ instead of IntervalMemberQ to work around the fact that
		IntervalMemberQ will return False if the granularity of the DateObject is larger
		than the granularity of the interval itself. E.g. consider:

			interval = DateInterval[{"January 1st, 2021", "January 7th, 2021"}];
			currentDate = DateObject["January 2021", "Month"];

			IntervalMemberQ[interval, currentDate]
				=> False

			DateOverlapsQ[interval, currentDate]
				=> True

		Using IntervalMemberQ would cause Section/Subsection cells with the year and
		month/year to be incorrectly stripped out by the Select.
	*)
	cells = Select[
		cells,
		Replace[#, {
			Cell["Daily", "Chapter", ___] -> True,
			Cell[year_?StringQ, "Section", ___] :> (
				DateOverlapsQ[interval, DateObject[{year, {"Year"}}, "Year"]]
			),
			Cell[monthYear_?StringQ, "Subsection", ___] :> (
				currentDate = DateObject[{monthYear, {"Month", "Year"}}, "Month"];
				If[!DateObjectQ[currentDate],
					Confirm[Failure["DateFilter", <|
						"MessageTemplate" -> "Cannot interpret Subsection as date: ``",
						"MessageParameters" -> {InputForm[monthYear]}
					|>]];
				];

				DateOverlapsQ[interval, currentDate]
			),
			Cell[dayNameMonth_?StringQ, "Subsubsection", ___] :> (
				updateCurrentDateFromSubsubsection[dayNameMonth];

				DateOverlapsQ[interval, currentDate]
			),
			Cell[dateTextData_TextData, "Subsubsection", ___] :> Module[{
				date
			},
				date = ReplaceRepeated[dateTextData, {
					TextData[{strings___?StringQ}] :> StringJoin[strings],
					StyleBox[string_?StringQ, ___] :> string
				}];

				If[!StringQ[date],
					Confirm[Failure["SubsubsectionDateFormat", <|
						"MessageTemplate" -> "Content of Subsubsection does not have the expected format:  ``",
						"MessageParameters" -> {ToString[InputForm[date]]}
					|>]];
				];

				updateCurrentDateFromSubsubsection[date];

				Assert[DateObjectQ[currentDate] && currentDate["Granularity"] === "Day"];

				DateOverlapsQ[interval, currentDate]
			],
			Cell[date_, "Subsubsection", ___] :> Confirm @ Failure["DateFilter", <|
				"MessageTemplate" -> "Cannot interpret Subsubsection with non-String contents as date: ``",
				"MessageParameters" -> {InputForm[date]}
			|>],
			(* Handle all the other cells types, e.g. TODO, Text, Input, Item, etc. Use
			   the `currentDate` value computed by previous handling of
			   Section/Subsection/Subsubsection headers which described the date. *)
			Cell[___] :> (
				If[!DateObjectQ[currentDate] || currentDate["Granularity"] =!= "Day",
					Confirm[Failure["DateFilter", <|
						"MessageTemplate" -> "Unknown current date when filtering Daily cells by date: ``: ``",
						"MessageParameters" -> {InputForm[currentDate], #}
					|>]];
				];
				IntervalMemberQ[interval, currentDate]
			),
			_ :> Confirm[$Failed]
		}] &
	];

	cells
]

(******************************************************************************)
(* Shared utility functions                                                   *)
(******************************************************************************)

cellsFromChapterInNB[path_?StringQ, chapter : "Queue" | "Daily"] := Try @ Module[{
	cells,
	chapterCell,
	isAlreadyOpen
},
	NotebookProcess[path, Function[nbObj, (
		chapterCell = Replace[chapter, {
			"Queue" :> FindQueueChapterCell[nbObj],
			"Daily" :> FindDailyChapterCell[nbObj],
			_ :> Confirm @ FailureMessage[
				Organizer::error,
				"Failed to read cells from: ``",
				{InputForm[path]}
			]
		}];

		GroupSelectionMove[chapterCell, All];
		cells = NotebookRead /@ SelectedCells[nbObj];

		(* Move the selection so that no cells are actually selected. This reduces the
		likelyhood that the user will accidentally erase selected cells if they switch
		focus back to the notebook and begin typing, expecting their cursor to be somewhere
		else. *)
		SelectionMove[nbObj, After, Cell];

		cells
	)]]
]

End[]

EndPackage[]