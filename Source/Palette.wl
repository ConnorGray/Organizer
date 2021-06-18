BeginPackage["Organizer`Palette`", {
    "Organizer`",
    "Organizer`Utils`",
    "Organizer`LogNotebookRuntime`"
}]

CategoryDirectory

Begin["`Private`"]

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


NotebooksDirectory[] := Module[{dir},
    dir = PersistentValue["CG:Organizer:RootDirectory", "Local"];
    If[!DirectoryQ[dir],
		If[$DynamicEvaluation,
			(* ChoiceDialog and SystemDialogInput don't work if this is a pre-emptive
			   evaluation, so skip trying to recover. This can happen when the hidden
			   button in the Log.nb docked title bar is clicked. *)
			Throw[StringForm[
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				dir
			]];
		];

		(* Ask the user if they would like to pick a new root directory. *)
		If[
			!ChoiceDialog[ToString @ StringForm[
				"Error: saved Organizer root directory does not exist: ``. Would you like to pick a new root directory?",
				InputForm[dir]
			]],
			Throw[$Canceled];
		];

		dir = SystemDialogInput["Directory", $HomeDirectory];

		If[!DirectoryQ[dir],
			If[dir === $Canceled,
				Throw[$Canceled];
			];

			Throw[StringForm[
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				dir
			]];
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
WorkspaceDirectory[] := Module[{name, dir, workspaces},
	name = PersistentValue["CG:Organizer:Workspace", "Local"];
	workspaces = Workspaces[];

	Assert[ListQ[workspaces]];

	If[workspaces === {},
		errorDialog[Failure["NoWorkspaceDirectories", <|
			"MessageTemplate" -> "The Organizer root directory (``) does not contain any subdirectories. Please create a subdirectory and try again.",
			"MessageParameters" -> {InputForm[NotebooksDirectory[]]}
		|>]];
		Throw[$Failed];
	];

	(* Handle some recoverable error conditions by prompting the user to pick a new
	   Workspace. *)
	Which[
		(* Handle the case that the saved Workspace name does not exist on disk. This
		   condition can happen when the user renames a Workspace. *)
		StringQ[name] && !MemberQ[workspaces, name], (
			name = ChoiceDialog[
				ToString @ StringForm[
					"Saved Workspace name '``' does not exist in the Organizer root directory: ``. Please choose another Workspace directory.",
					name, InputForm[NotebooksDirectory[]]
				],
				Map[(# -> #)&, workspaces]
			];
			If[name === $Canceled,
				Throw[$Failed];
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
				Throw[$Failed];
			];
			PersistentValue["CG:Organizer:Workspace", "Local"] = name;
		)
	];

	dir = FileNameJoin[{NotebooksDirectory[], name}];
	If[!DirectoryQ[dir],
		Throw[$Failed];
	];
	dir
];

CategoryDirectory[] := Module[{name, dir, categories},
	name = PersistentValue["CG:Organizer:Category", "Local"];
	categories = Categories[];

	Assert[ListQ[categories]];

	If[categories === {},
		errorDialog[Failure["NoCategoryDirectories", <|
			"MessageTemplate" -> "The current Workspace directory (``) does not contain any subdirectories. Please create a subdirectory and try again.",
			"MessageParameters" -> {InputForm[WorkspaceDirectory[]]}
		|>]];
		Throw[$Failed];
	];

	(* Handle some recoverable error conditions by prompting the user to pick a new
	   Category. *)
	Which[
		(* Handle the case that the saved Category name does not exist on disk. This
		   condition can happen when the user renames a Category. *)
		StringQ[name] && !MemberQ[categories, name], (
			name = ChoiceDialog[
				ToString @ StringForm[
					"Saved Category name '``' does not exist in the current Workspace directory: ``. Please choose another Category directory.",
					name, InputForm[WorkspaceDirectory[]]
				],
				Map[(# -> #)&, categories]
			];
			If[name === $Canceled,
				Throw[$Failed];
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
				Throw[$Failed];
			];
			PersistentValue["CG:Organizer:Category", "Local"] = name;
		)
	];

	dir = FileNameJoin[{WorkspaceDirectory[], name}];
	If[!DirectoryQ[dir],
		Throw[$Failed];
	];
	dir
]

Workspaces[] := subDirectoryNames[NotebooksDirectory[]]

(* Get a list of Categories which are a part of the current workspace. *)
Categories[] := subDirectoryNames[WorkspaceDirectory[]]

Projects[] := subDirectoryNames[CategoryDirectory[]]

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

(*
    Statefully create or refresh the global Organizer palette.
*)
CreateOrganizerPalette[] := With[{
    loadOrFail = $HeldLoadOrFail
},
    Module[{categoryPicker, categoryButton, paletteContents, existingNB, margins},
        categoryButton[category_?StringQ] := Button[
            category,
            (
                ReleaseHold[loadOrFail];
                PersistentValue["CG:Organizer:Category"] = category;

                (* CreateOrganizerPalette[] automatically overwrites the already-open
                organizer. This is effectively a Refresh. *)
                Organizer`CreateOrganizerPalette[]
            ),
            FrameMargins -> 2,
            Appearance -> "Palette",
            ImageSize -> Medium
        ];

        categoryPicker = Pane[
            Grid[{Map[categoryButton, Categories[]]}, Spacings -> 0],
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
                                commandDropdownContents[close]
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
                Grid[buttonListToOpenActiveProjectLogs[], Spacings -> {0, 0}],
                categoryPicker
            }
            ,
            Spacings -> 0.15
        ];

        paletteContents = Pane[paletteContents, ImageSize -> $PaletteWidth];

        existingNB = PersistentValue["CG:Organizer:PaletteObject", "FrontEndSession"];

        If[MatchQ[existingNB, NotebookObject[__] ],
            Module[{opts},
                opts = Options[existingNB];
                (* Check that existingNBObj is still actually open. If not, reset the global
                palette NB object. *)
                If[FailureQ @ opts,
                    PersistentValue["CG:Organizer:PaletteObject", "FrontEndSession"] = CreatePalette[paletteContents];
                    Return[];
                ];
                margins = Association[Options[existingNB] ][WindowMargins];
                CreatePalette[paletteContents, existingNB];
                SetOptions[existingNB, WindowMargins -> margins];
            ]
            ,
            PersistentValue["CG:Organizer:PaletteObject", "FrontEndSession"] = CreatePalette[paletteContents];
        ];
    ];
]

commandDropdownContents[close_Function] := With[{
    loadOrFail = $HeldLoadOrFail
},
    Column[
        {
            Button[Style["Refresh", 20],
                (
                    ReleaseHold[loadOrFail];

                    Assert[MemberQ[$Packages, "Organizer`"]];

                    (* CreateOrganizerPalette[] automatically overwrites the already-open
                    organizer. *)
                    Organizer`CreateOrganizerPalette[]
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

                        Organizer`CreateOrganizerPalette[]
                    )&,
                    Workspaces[]
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

buttonListToOpenActiveProjectLogs[] := Module[{projects, metaProjs},
    projects = Projects[];

    metaProjs = Sort[Select[projects, StringStartsQ["+"]]];
    projects = Sort[Complement[projects, metaProjs]];

    Map[
        Function[proj,
            With[{
                loadOrFail = $HeldLoadOrFail,
                path = FileNameJoin[{CategoryDirectory[], proj, "Log.nb"}]
            },
                If[FileExistsQ[path],
                    {
                        Button[Style[StringTrim[proj, "+"], 16],
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
                        Button[Style["./", 16, Bold],
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

handleStartNewProject[] := Module[{
    projName, dirPath, logNB
},
    projName = InputString[];
    If[!StringQ[projName],
        If[projName === $Canceled,
            Return[];
        ];
        Throw[StringForm["Invalid project name: ``", projName] ];
    ];

    dirPath = FileNameJoin[{CategoryDirectory[], projName}];
    If[FileExistsQ[dirPath],
        MessageDialog[StringForm["File exists at path ``", dirPath]];
        Return[$Failed];
    ];

    (* Make sure the icons are loaded *before* we modify the filesystem. *)
    LoadIcons[];

    CreateDirectory[dirPath];

    logNB = CreateNotebook[];

    NotebookWrite[logNB, Cell[projName, "Title"] ];
    NotebookWrite[
        logNB,
        Cell[
            "Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day"}],
            "Subtitle"
        ]
    ];

    NotebookWrite[logNB, Cell["Context", "Chapter"] ];

    NotebookWrite[logNB, Cell["Daily", "Chapter"] ];
    NotebookWrite[logNB, Cell[DateString[Now, {"MonthName", " ", "Year"}], "Subsection"] ];
    NotebookWrite[logNB, Cell[DateString[Now, {"DayName", ", ", "MonthName", " ", "Day"}], "Subsubsection"] ];

    NotebookWrite[logNB, Cell["Queue", "Chapter"] ];

    InstallLogNotebookStyles[logNB];
    InstallLogNotebookDockedCells[logNB, projName];

    (* TODO: Set DockedCells *)

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];

    (* Refresh the organizer palette in-place. *)
    CreateOrganizerPalette[];
]

(**************************************)
(* Show Queue's Report                *)
(**************************************)

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
handleShowQueues[] := Module[{projects, settings, nb, path, cells, timestamp, workspaceName},

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
			errorDialog[StringForm[
				"Unexpected value returned from settings panel: ``",
				InputForm[settings]
			]];
			Return[$Failed, Module];
		)
	}];

	(*-----------------------*)
	(* Populate the notebook *)
	(*-----------------------*)

    nb = CreateNotebook[];

    workspaceName = FileNameTake[WorkspaceDirectory[], -1];

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
            path = FileNameJoin[{CategoryDirectory[], proj, "Log.nb"}];

            cells = cellsFromChapterInNB[path, "Queue"];
            If[FailureQ[cells],
                MessageDialog[Row[{"Failed to read cells from: ", path}]];
                Return[];
            ];

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

HandleShowDailys[] := Module[{result},
	result = iHandleShowDailys[];

	If[MatchQ[result, _Failure],
		errorDialog[result];
	];
]

iHandleShowDailys[] := Enclose[Module[{
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
						{"This Week", "Last Week", "Yesterday", "Past 7 Days", "Past 30 Days"}
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

	workspaceName = FileNameTake[WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day",
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
			path = FileNameJoin[{CategoryDirectory[], proj, "Log.nb"}];

			cells = cellsFromChapterInNB[path, "Daily"];
			If[FailureQ[cells],
				MessageDialog[Row[{"Failed to read cells from: ", path}]];
				Return[];
			];

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
				BoxData @ ToBoxes @ Style["All Daily's: " <> workspaceName <> ": " <> timestamp],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		}
	];

	SelectionMove[First[Cells[nb]], Before, Cell, AutoScroll -> True];
],
#["Expression"]&
]

filterDailyCellsByInterval[
	cells0 : {___Cell},
	interval_DateInterval
] := Enclose[Module[{
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
],
#["Expression"] &
]

(******************************************************************************)
(* Shared utility functions                                                   *)
(******************************************************************************)

cellsFromChapterInNB[path_?StringQ, chapter : "Queue" | "Daily"] := Module[{
	cells,
	chapterCell,
	isAlreadyOpen
},
	NotebookProcess[path, Function[nbObj, (
		chapterCell = Replace[chapter, {
			"Queue" :> FindQueueChapterCell[nbObj],
			"Daily" :> FindDailyChapterCell[nbObj],
			_ :> Return[$Failed]
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