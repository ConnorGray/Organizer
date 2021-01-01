BeginPackage["Organizer`Palette`", {
    "Organizer`",
    "Organizer`Utils`",
    "Organizer`LogNotebookRuntime`"
}]

Begin["`Private`"]

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

Workspaces[] := Module[{files},
    files = FileNames[All, NotebooksDirectory[]];
    files = Select[files, DirectoryQ];

    (* Get the last component of the file path -- this these are the Workspace names. *)
    files = Map[FileNameTake[#, -1]&, files];
    files = Select[files, !StringStartsQ[#, "."]&];

    files
]

(**************************************)
(* Interface Building Code            *)
(**************************************)

(*
    Statefully create or refresh the global Organizer palette.
*)
CreateOrganizerPalette[] := With[{
    loadOrFail = $HeldLoadOrFail
},
    Module[{paletteContents, existingNB, margins},
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
                Grid[buttonListToOpenActiveProjectLogs[], Spacings -> {0, 0}]
            }
            ,
            Spacings -> 0.15
        ];

        paletteContents = Pane[paletteContents, ImageSize -> 220];

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
                        Organizer`CreateOrganizerPalette[]
                    )&,
                    Workspaces[]
                ]
            },
                ActionMenu[
                    "Workspace ...",
                    choices
                    (* ImageSize -> Full *)
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

getListOfActiveProjects[] := Map[
    FileNameTake[#, -1] &,
    Select[
        FileNames[
            All,
            FileNameJoin[{WorkspaceDirectory[], "Projects", "Active"}]
        ],
        DirectoryQ
    ]
]

buttonListToOpenActiveProjectLogs[] := Module[{activeProjs},
    activeProjs = getListOfActiveProjects[];

    (* Extremely janky way of making the Tasks and Bugs projects come first in the palette. *)

    activeProjs = Replace[activeProjs, {head__, "Ideas", tail__} :> {"Ideas", head, tail}];
    activeProjs = Replace[activeProjs, {head__, "Bugs", tail__} :> {"Bugs", head, tail}];
    activeProjs = Replace[activeProjs, {head__, "Tasks", tail__} :> {"Tasks", head, tail}];

    Map[
        Function[proj,
            With[{
                loadOrFail = $HeldLoadOrFail,
                path = FileNameJoin[{WorkspaceDirectory[], "Projects", "Active", proj, "Log.nb"}]
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

    dirPath = FileNameJoin[{WorkspaceDirectory[], "Projects", "Active", projName}];
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

handleNewMeetingNotes[] := Module[{nameSpaces, nameHyphens},
    nameSpaces = InputString[];
    If[!StringQ[nameSpaces],
        Throw[StringForm["Invalid meeting notes name: ``", nameSpaces] ];
    ];
    nameHyphens = StringReplace[nameSpaces, " " -> "-"];
]

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
handleShowQueues[] := Module[{nb, projects, path, cells},
    nb = CreateNotebook[];

    NotebookWrite[nb, Cell["All Queues: " <> FileNameTake[WorkspaceDirectory[], -1], "Title"]];
    NotebookWrite[
        nb,
        Cell[
            "Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", " at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"}],
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
                BoxData @ ToBoxes @ Style["Building ...", Italic, GrayLevel[0.2]],
                "Subtitle",
                Background -> Lighter[Orange]
            ]
        }
    ];

    projects = getListOfActiveProjects[];

    Scan[
        Function[proj,
            path = FileNameJoin[{WorkspaceDirectory[], "Projects", "Active", proj, "Log.nb"}];

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
    SetOptions[nb, DockedCells -> {}];
]

queueCellsFromNB[path_?StringQ] := Module[{nbObj, cells, queueChapterCell, isAlreadyOpen},
    isAlreadyOpen = Echo[notebookAtPathIsOpen[path], "isAlreadyOpen"];

    (* `Visible -> False` so we don't overload the user by opening a bunch of notebooks
       they didn't actually want to see. *)
    nbObj = NotebookOpen[path, Visible -> isAlreadyOpen];
    If[FailureQ[nbObj],
        Return[nbObj];
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

notebookAtPathIsOpen[path_?StringQ] := Echo@AnyTrue[
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