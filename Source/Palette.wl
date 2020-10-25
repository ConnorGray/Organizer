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

Workspaces[] := Map[
    FileNameTake[#, -1]&,
    Select[FileNames[All, NotebooksDirectory[] ], DirectoryQ]
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
                (* mainBar[organizerPacletPath], *)
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
                        Button[
                            Style[Global`\[CloverLeaf], 25],
                            (
                                ReleaseHold[loadOrFail];
                                CreateWindow[PaletteNotebook@mainBar[]];
                            ),
                            Method -> "Queued",
                            Active -> False,
                            Alignment -> Center,
                            Background -> Lighter@Orange,
                            ImageSize -> Full
                        ]
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

mainBar[] := With[{
    loadOrFail = $HeldLoadOrFail
},
    Grid[
        {{
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
        }},
        (* ItemSize -> {{Scaled[0.6], Scaled[0.4]}}, *)
        Spacings -> 0
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
                                NotebookOpen[path];
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
        Throw[StringForm["File exists at path ``", dirPath] ];
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

    installLogNotebookDockedCells[logNB, projNameSpaces];

    (* TODO: Set DockedCells *)

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];
]

handleNewMeetingNotes[] := Module[{nameSpaces, nameHyphens},
    nameSpaces = InputString[];
    If[!StringQ[nameSpaces],
        Throw[StringForm["Invalid meeting notes name: ``", nameSpaces] ];
    ];
    nameHyphens = StringReplace[nameSpaces, " " -> "-"];
]


End[]

EndPackage[]