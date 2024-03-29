BeginPackage["ConnorGray`Organizer`Palette`"]

OrganizerDirectory = NotebooksDirectory
WorkspaceDirectory
CategoryDirectory
RefreshOrganizerPalette
InitializeOrganizerPalette

Workspaces
Categories
Projects

(*----------*)
(* Utilites *)
(*----------*)

CellsFromChapterInNB


Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`LogNotebookRuntime`"]
Needs["ConnorGray`Organizer`Reports`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]


NotebooksDirectory[] := Handle[_Failure] @ Module[{dir, error},
    dir = PersistentValue["CG:Organizer:RootDirectory", "Local"];

    If[!DirectoryQ[dir],
		If[$SynchronousEvaluation,
			(* ChoiceDialog and SystemDialogInput don't work if this is a pre-emptive
			   evaluation, so skip trying to recover. This can happen when the hidden
			   button in the Log.nb docked title bar is clicked. *)
			Raise[
				OrganizerError,
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				dir
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
			Raise[
				OrganizerError,
				"Canceling operation. No Organizer Root directory is available."
			];
		];

		dir = SystemDialogInput["Directory", $HomeDirectory];

		If[!DirectoryQ[dir],
			If[dir === $Canceled,
				(* The user decided not to pick a root directory. Return an error. *)
				Raise[
					OrganizerError,
					"Canceling operation. No Organizer Root directory is available."
				];
			];

			Raise[
				OrganizerError,
				"Error: Saved Organizer root directory is not DirectoryQ: ``",
				dir
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
WorkspaceDirectory[] := Handle[_Failure] @ Module[{
	organizerRoot, name, dir, workspaces
},
	organizerRoot = RaiseConfirm @ NotebooksDirectory[];

	name = PersistentValue["CG:Organizer:Workspace", "Local"];
	workspaces = RaiseConfirm @ Workspaces[];

	Assert[ListQ[workspaces]];

	If[workspaces === {},
		Raise[
			OrganizerError,
			"The Organizer root directory (``) does not contain any subdirectories. "
			<> "Please create a subdirectory and try again.",
			InputForm[organizerRoot]
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
				Raise[
					OrganizerError,
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
				Raise[
					OrganizerError,
					"Canceling operation. No Workspace directory is available."
				];
			];
			PersistentValue["CG:Organizer:Workspace", "Local"] = name;
		)
	];

	dir = FileNameJoin[{organizerRoot, name}];
	If[!DirectoryQ[dir],
		Raise[
			OrganizerError,
			"Workspace directory is unexpectedly invalid: ``",
			InputForm[dir]
		];
	];
	dir
];

CategoryDirectory[] := Handle[_Failure] @ Module[{workspaceDir, name, dir, categories},
	workspaceDir = RaiseConfirm @ WorkspaceDirectory[];

	name = PersistentValue["CG:Organizer:Category", "Local"];
	categories = RaiseConfirm @ Categories[];

	Assert[ListQ[categories]];

	If[categories === {},
		Raise[
			OrganizerError,
			"The current Workspace directory (``) does not contain any subdirectories. "
			<> "Please create a subdirectory and try again.",
			InputForm[workspaceDir]
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
				Raise[
					OrganizerError,
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
				Raise[
					OrganizerError,
					"Canceling operation. No Category directory is available."
				];
			];
			PersistentValue["CG:Organizer:Category", "Local"] = name;
		)
	];

	dir = FileNameJoin[{workspaceDir, name}];
	If[!DirectoryQ[dir],
		Raise[
			OrganizerError,
			"Category directory is unexpectedly invalid: ``",
			InputForm[dir]
		];
	];
	dir
]

Workspaces[] := Handle[_Failure] @ subDirectoryNames[RaiseConfirm @ NotebooksDirectory[]]

(* Get a list of Categories which are a part of the current workspace. *)
Categories[] := Handle[_Failure] @ subDirectoryNames[RaiseConfirm @ WorkspaceDirectory[]]

Projects[] := Handle[_Failure] @ subDirectoryNames[RaiseConfirm @ CategoryDirectory[]]

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

OpenOrganizerPalette[] := Handle[_Failure] @ Module[{
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


	organizerPacletLocation = RaiseConfirm[PacletObject["ConnorGray/Organizer"]]["Location"];
	paletteLocation = FileNameJoin[{
		organizerPacletLocation,
		"FrontEnd", "Palettes", "Organizer.nb"
	}];

	If[!DirectoryQ[organizerPacletLocation] || !FileExistsQ[paletteLocation],
		Raise[
			OrganizerError,
			"Unable to get location of Organizer paclet and/or palette: ``",
			InputForm[organizerPacletLocation]
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
		RaiseConfirm @ RefreshOrganizerPalette[paletteNBObj];
		,
		(* Open the Organizer.nb. Whether it succeeds or fails, this operation returns Null. *)
		FrontEndTokenExecute["OpenFromPalettesMenu", "Organizer.nb"];
	];

	(* TODO: Return the NotebookObject[..] for the open palette? *)
]

InitializeOrganizerPalette[nb0_NotebookObject] := Handle[_Failure] @ Module[{
	contents
},
	If[FailureQ[CategoryDirectory[]],
		(* If we can't get a CategoryDirectory[], make the contents of the palette be
		   a `Method -> "Queued" button for the user to click. This is a workaround for
		   the fact we cannot show dialog prompts to the user during a preemptive
		   evaluation (which notebook `Initialization :> (..)` is). *)
		RaiseConfirm @ SetOrganizerPaletteContent[nb0, Function[nb, (
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

RefreshOrganizerPalette[nb0_NotebookObject] := Handle[_Failure] @ Module[{
	contents
},
	(* Echo["called RefreshOrganizerPalette:"]; *)

	SetOrganizerPaletteContent[
		nb0,
		Function[nb, (
			(* Compute the new contents for the palette. *)
			(* Do this *before* deleting the old contents to minimize the time that the palette
			is blank. *)
			contents = createOrganizerPalette[];

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

SetOrganizerPaletteContent[nb_NotebookObject, callback_] := Handle[_Failure] @ Module[{
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

		Raise[
			OrganizerError,
			"CRITICAL ERROR: Unsafe attempt to refresh a notebook which was not the main "
			<> "Organizer palette. Doing so would delete all contents of the affected notebook. "
			<> "Notebook file path was: ``. Notebook title was: ``",
			InputForm[info["FileName"]],
			InputForm[info["WindowTitle"]]
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


	RaiseConfirm @ callback[nb];


	(* Restore `Selectable -> False` *)
	SetOptions[nb, Selectable -> False];

	(* Force the notebook to resize to fit its content. *)
	SetOptions[nb, WindowSize -> All];

	CurrentValue[nb, {TaggingRules, "CG:Organizer", "DocumentType"}] = "MainOrganizerPalette";
]

createOrganizerPalette[] := With[{
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
		Grid[
			{Map[categoryButton, RaiseConfirm @ Categories[]]},
			Spacings -> 0
		],
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
			Grid[buttonListToOpenActiveProjectLogs[], Spacings -> {0, 0}],
			categoryPicker
		}
		,
		Spacings -> 0.1
	];

	paletteContents = Pane[paletteContents, ImageSize -> $PaletteWidth];

	paletteContents
]]

commandDropdownContents[close_Function] := Handle[_Failure] @ With[{
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
                    workspace |-> Button[workspace, (
                        ReleaseHold[loadOrFail];

                        PersistentValue["CG:Organizer:Workspace", "Local"] = workspace;
                        (* Set the category for the selected workspace. Because the
                           categories just happen to be sorted (by FileNames), "Active" is
                           typically the first category. There currently isn't a proper
                           concept of a "default" category for a workspace. *)
                        PersistentValue["CG:Organizer:Category"] = First[Categories[]];

						(* OpenOrganizerPalette[] automatically refreshes the palette. *)
						HandleUIFailure @ ConnorGray`Organizer`OpenOrganizerPalette[]
					), Background -> LightGray],
                    RaiseConfirm @ Workspaces[]
                ]
            },
				Splice[choices]
            ],
            Button[
                Style["Show Queues", 20],
                (
                    ReleaseHold[loadOrFail];

                    HandleUIFailure @ CreateQueuesReport[];
                ),
                Method -> "Queued",
                Background -> LightOrange
            ],
            Button[
                Style["Show Daily's", 20],
                (
                    ReleaseHold[loadOrFail];

                    HandleUIFailure @ CreateDailysReport[];
                ),
                Method -> "Queued",
                Background -> LightBlue
            ]
        },
		Spacings -> 0
    ]
]

buttonListToOpenActiveProjectLogs[] := Module[{projects, metaProjs},
    projects = RaiseConfirm @ Projects[];

    metaProjs = Sort[Select[projects, StringStartsQ["+"]]];
    projects = Sort[Complement[projects, metaProjs]];

    Map[
        Function[proj,
            With[{
                loadOrFail = $HeldLoadOrFail,
                path = FileNameJoin[{
					RaiseConfirm @ CategoryDirectory[],
					proj,
					"Log.nb"
				}]
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

handleStartNewProject[] := HandleUIFailure @ Handle[_Failure] @ Module[{
    projName, dirPath, logNB
},
    projName = InputString[];
    If[!StringQ[projName],
        If[projName === $Canceled,
            Return[];
        ];
		Raise[
			OrganizerError,
			"Invalid project name: ``",
			InputForm[projName]
		];
    ];

	dirPath = FileNameJoin[{
		RaiseConfirm @ CategoryDirectory[],
		projName
	}];

    If[FileExistsQ[dirPath],
		Raise[
			OrganizerError,
			"File already exists at path ``.",
			InputForm[dirPath]
		];
    ];

    (* Make sure the icons are loaded *before* we modify the filesystem. *)
    LoadIcons[];

    CreateDirectory[dirPath];

	logNB = RaiseConfirm @ CreateLogNotebook[projName];

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];

    (* Refresh the organizer palette in-place. *)
    OpenOrganizerPalette[]
]

(******************************************************************************)
(* Shared utility functions                                                   *)
(******************************************************************************)

CellsFromChapterInNB[path_?StringQ, chapter : "Queue" | "Daily"] := Module[{
	nb,
	chapterCellGroup
},
	(* TODO(polish):
		If the notebook at `path` is opened and has unsaved changes, those changes
		won't be reflected in the generated report. *)
	nb = RaiseConfirmMatch[Get[path], _Notebook];

	(* TODO(robust):
		Errors for: Multiple matching chapters, no matching, and empty chapter
		cells that aren't in a group. *)
	chapterCellGroup = FirstCase[
		nb,
		Cell @ CellGroupData[
			{
				Cell[chapter, "Chapter", ___],
				rest___
			},
			_
		],
		{},
		Infinity
	];

	flattenCellGroups[chapterCellGroup]
]

(*------------------------------------*)

SetFallthroughError[flattenCellGroups]

(* Returns a flat list of one or more Cells without any cell groups. *)
flattenCellGroups[cell_Cell] := RaiseConfirmMatch[#, {___Cell}] & @ ConfirmReplace[cell, {
	Cell[
		CellGroupData[elems_List, RepeatedNull[_, 1]]
	] :> (
		flattenCellGroups[elems]
	),
	Cell[Except[_CellGroupData], ___] :> {cell},
	other_ :> Raise[
		OrganizerError,
		"Unexpected cell group structure: ``",
		other
	]
}]

flattenCellGroups[list_List] :=
	Flatten[Map[flattenCellGroups, list], 1]


End[]

EndPackage[]