BeginPackage["Organizer`Palette`", {
    "Organizer`",
    "Organizer`LogNotebookRuntime`"
}]


Begin["`Private`"]


(*
    Statefully create or refresh the global Organizer palette.
*)
CreateOrganizerPalette[] := With[{
    (* This variable needs to be embeded using a With so that it's embeded in the held
       "Refresh" Button below. *)
    organizerPacletPath = FileNameDrop[PacletObject["Organizer"]["Location"], -1]
},
    If[!DirectoryQ[organizerPacletPath],
        MessageDialog[StringForm[
            "Saved path to the Organizer paclet is no longer a directory: ``",
            organizerPacletPath
        ]];
        Return[$Failed];
    ];

    Module[{paletteContents, existingNB, margins},
    paletteContents = Column[
        {
            Grid[
                {{
                    Button[
                        Style["New Project", 20],
                        handleStartNewProject[],
                        Method -> "Queued",
                        Background -> Green
                    ],
                    Button[Style["Refresh", 20],
                        (
                            If[!MemberQ[$Packages, "Organizer`"],
                                If[!DirectoryQ[organizerPacletPath],
                                    MessageDialog[StringForm[
                                        "Embedded path to the Organizer paclet is no longer a directory: ``",
                                        organizerPacletPath
                                    ]];
                                    Return[$Failed];
                                ];
                                PacletDirectoryLoad[organizerPacletPath];
                                Needs["Organizer`"];
                                If[!MemberQ[$Packages, "Organizer`"],
                                    MessageDialog[Row[{Style["Error:", Red], " Organizer` could not be loaded."}] ];
                                ];
                                Return[];
                            ];
                            Organizer`CreateOrganizerPalette[]
                        )&[],
                        Method -> "Queued",
                        Background -> LightBlue
                    ]
                }},
                ItemSize -> {{Scaled[0.6], Scaled[0.4]}},
                Spacings -> 0
            ],
            Grid[buttonListToOpenActiveProjectLogs[], Spacings -> {0, 0}]
        }
        ,
        Spacings -> 0.15
    ];

    paletteContents = Pane[paletteContents, ImageSize -> 220];

    existingNB = PersistentValue["CG:Organizer", "FrontEndSession"];

    If[MatchQ[existingNB, NotebookObject[__] ],
        Module[{opts},
            opts = Options[existingNB];
            (* Check that existingNBObj is still actually open. If not, reset the global
               palette NB object. *)
            If[FailureQ @ opts,
                PersistentValue["CG:Organizer", "FrontEndSession"] = CreatePalette[paletteContents];
                Return[];
            ];
            margins = Association[Options[existingNB] ][WindowMargins];
            CreatePalette[paletteContents, existingNB];
            SetOptions[existingNB, WindowMargins -> margins];
        ]
        ,
        PersistentValue["CG:Organizer", "FrontEndSession"] = CreatePalette[paletteContents];
    ];
];
]


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

    dirPath = FileNameJoin[{$MyNotebooksDirectory, "WRI", "Projects", "Active", projName}];
    If[FileExistsQ[dirPath],
        Throw[StringForm["File exists at path ``", dirPath] ];
    ];

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

    fInstallLogNotebookDockedCells[logNB, projNameSpaces];

    (* TODO: Set DockedCells *)

    NotebookSave[logNB, FileNameJoin[{dirPath, "Log.nb"}] ];
]

getListOfActiveProjects[] := Map[
    FileNameTake[#, -1] &,
    Select[
        FileNames[
            All,
            FileNameJoin[{$MyNotebooksDirectory, "WRI", "Projects", "Active"}]
        ],
        DirectoryQ
    ]
]

buttonListToOpenActiveProjectLogs[] := Module[{activeProjs},
    activeProjs = getListOfActiveProjects[];

      (* Extremely janky way of making the Tasks and Bugs projects come first in the palette. *)

    activeProjs = Replace[activeProjs, {head__, "Bugs", tail__} :> {"Bugs", head, tail}];
    activeProjs = Replace[activeProjs, {head__, "Tasks", tail__} :> {"Tasks", head, tail}];

    Map[
        Function[proj,
            With[{
                path = FileNameJoin[{$MyNotebooksDirectory, "WRI", "Projects", "Active", proj, "Log.nb"}]
            },
                If[FileExistsQ[path],
                    {
                        Button[Style[proj, 16], NotebookOpen[path],
                            Method -> "Queued",
                            Background -> Replace[proj, {
                                "Bugs" | "Tasks" -> Lighter@Lighter@Lighter@Blue,
                                _ -> LightBlue
                            }]
                        ],
                        Button[Style["./", 16, Bold], 
                            RunProcess[{"open", FileNameDrop[path, -1]}],
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

handleNewMeetingNotes[] := Module[{nameSpaces, nameHyphens},
    nameSpaces = InputString[];
    If[!StringQ[nameSpaces],
        Throw[StringForm["Invalid meeting notes name: ``", nameSpaces] ];
    ];
    nameHyphens = StringReplace[nameSpaces, " " -> "-"];
]


End[]

EndPackage[]