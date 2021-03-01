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


NotebooksDirectory[] := Module[{dir},
    dir = PersistentValue["CG:Organizer:RootDirectory", "Local"];
    If[!DirectoryQ[dir],
        Throw[StringForm[
            "Error: saved organizer notebooks directory is not DirectoryQ: ``",
            dir
        ]];
    ];
    dir
]

(* Get the directory for the currently selected Workspace. *)
WorkspaceDirectory[] := Module[{name, dir},
    name = PersistentValue["CG:Organizer:Workspace", "Local"];
    If[!StringQ[name],
        name = ChoiceDialog[
            "No Workspace is selected. Please choose one.",
            Map[(# -> #)&, Workspaces[]]
        ];
        PersistentValue["CG:Organizer:Workspace", "Local"] = name;
        (* Throw[StringForm[
            "Error: no valid Workspace is selected: ``",
            name
        ]]; *)
    ];

    dir = FileNameJoin[{NotebooksDirectory[], name}];
    If[!DirectoryQ[dir],
        Throw[StringForm[
            "Error: saved Workspace name does not exist in the Notebooks directory: ``",
            dir
        ]];
    ];
    dir
];

CategoryDirectory[] := Module[{name},
    name = PersistentValue["CG:Organizer:Category", "Local"];
    If[!StringQ[name],
        name = ChoiceDialog[
            "No Category is selected. Please choose one.",
            Map[(# -> #)&, Categories[]]
        ];
        PersistentValue["CG:Organizer:Category", "Local"] = name;
    ];

    dir = FileNameJoin[{WorkspaceDirectory[], "Projects", name}];
    If[!DirectoryQ[dir],
        errorDialog[StringForm[
            "saved Category name '``' does not exist in the Workspace directory: ``",
            name,
            dir
        ]];
        Throw[$Failed];
    ];
    dir
]

Workspaces[] := subDirectoryNames[NotebooksDirectory[]]

(* Get a list of Categories which are a part of the current workspace. *)
Categories[] := subDirectoryNames[FileNameJoin[{WorkspaceDirectory[], "Projects"}]]

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
        Reverse @ {
            Button[
                Style["Show Queues", 20],
                (
                    ReleaseHold[loadOrFail];

                    handleShowQueues[];
                ),
                Method -> "Queued",
                Background -> LightOrange
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
                    Style["Workspace ...", 16],
                    choices,
                    ImageSize -> Full
                ]
            ],
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
            ]
        }
    ]
]

buttonListToOpenActiveProjectLogs[] := Module[{activeProjs},
    activeProjs = Projects[];

    (* Extremely janky way of making the Tasks and Bugs projects come first in the palette. *)

    activeProjs = Replace[activeProjs, {head__, "Ideas", tail__} :> {"Ideas", head, tail}];
    activeProjs = Replace[activeProjs, {head__, "Bugs", tail__} :> {"Bugs", head, tail}];
    activeProjs = Replace[activeProjs, {head__, "Tasks", tail__} :> {"Tasks", head, tail}];

    Map[
        Function[proj,
            With[{
                loadOrFail = $HeldLoadOrFail,
                path = FileNameJoin[{CategoryDirectory[], proj, "Log.nb"}]
            },
                If[FileExistsQ[path],
                    {
                        Button[Style[proj, 16],
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
                            Background -> Replace[proj, {
                                "Ideas" | "Bugs" | "Tasks" -> Lighter@Lighter@Lighter@Blue,
                                _ -> LightBlue
                            }]
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
        activeProjs
    ]
]

(**************************************)
(* UI Event Handlers                  *)
(**************************************)

handleStartNewProject[] := Module[{
    projNameSpaces, projName, dirPath, logNB
},
    projNameSpaces = InputString[];
    If[!StringQ[projNameSpaces],
        If[projNameSpaces === $Canceled,
            Return[];
        ];
        Throw[StringForm["Invalid project name: ``", projNameSpaces] ];
    ];
    projName = StringReplace[projNameSpaces, " " -> "-"];

    dirPath = FileNameJoin[{CategoryDirectory[], projName}];
    If[FileExistsQ[dirPath],
        MessageDialog[StringForm["File exists at path ``", dirPath]];
        Return[$Failed];
    ];

    (* Make sure the icons are loaded *before* we modify the filesystem. *)
    LoadIcons[];

    CreateDirectory[dirPath];

    logNB = CreateNotebook[];

    NotebookWrite[logNB, Cell[projNameSpaces, "Title"] ];
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

    installLogNotebookStyles[logNB];
    installLogNotebookDockedCells[logNB, projNameSpaces];

    (* TODO: Set DockedCells *)

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];

    (* Refresh the organizer palette in-place. *)
    CreateOrganizerPalette[];
]

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
handleShowQueues[] := Module[{nb, projects, path, cells, timestamp, workspaceName},
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
    installLogNotebookStyles[nb];

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

    projects = Projects[];

    Scan[
        Function[proj,
            path = FileNameJoin[{CategoryDirectory[], proj, "Log.nb"}];

            cells = queueCellsFromNB[path];
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

queueCellsFromNB[path_?StringQ] := Module[{nbObj, cells, queueChapterCell, isAlreadyOpen},
    isAlreadyOpen = notebookAtPathIsOpen[path];

    (* `Visible -> False` so we don't overload the user by opening a bunch of notebooks
       they didn't actually want to see. This also prevents a subtle annoyance: if you
       have multiple desktops (not multiple monitors, but multiple "virtual" desktop
       spaces), when `nbObj` is already open on a different desktop, the screen will
       quickly swipe over to it, and away from the All Queues notebook which is being
       generated.
    *)
    nbObj = NotebookOpen[path, Visible -> False];
    If[FailureQ[nbObj],
        Return[nbObj];
    ];

    (* If the user already had the notebook open, quickly make it visible again. This
       happens quickly enough that I haven't noticed any visual "flickering". *)
    If[isAlreadyOpen,
        SetOptions[nbObj, Visible -> True]
    ];

	queueChapterCell = FindQueueChapterCell[nbObj];

    GroupSelectionMove[queueChapterCell, All];
	cells = NotebookRead /@ SelectedCells[nbObj];

    (* Move the selection so that no cells are actually selected. This reduces the
       likelyhood that the user will accidentally erase selected cells if they switch
       focus back to the notebook and begin typing, expecting their cursor to be somewhere
       else. *)
    SelectionMove[nbObj, After, Cell];

    (* Only close the notebook if it was not already open before the user pressed the
       "Show Queues" button. *)
    If[!isAlreadyOpen,
        NotebookClose[nbObj, Interactive -> True];
    ];

    cells
]

notebookAtPathIsOpen[path_?StringQ] := AnyTrue[
    Notebooks[],
    Function[nb, Module[{name},
        name = Association[NotebookInformation[nb]]["FileName"];
        If[MissingQ[name],
            Return[False, Module];
        ];

        name = Replace[
            name,
            FrontEnd`FileName[{parts___}, name_, ___] :> FileNameJoin[{parts, name}]
        ];
        name == path
    ]]
]


End[]

EndPackage[]