BeginPackage["Organizer`"]


(*TODO: Make $ScratchNBRootDirectory relative to TODOs.nb. The hyperlinks made by
        `New Scratch NB` should also be relative, and use $ScratchNBRootDirectory.*)

CreateOrganizerPalette
UpdateLogNotebooks

Begin["`Private`"]

(* PersistentValue["CG:Organizer:PaletteObject", "FrontEndSession"] = None; *)

If[!DirectoryQ[PersistentValue["CG:Organizer:RootDirectory", "Local"]],
	dir = SystemDialogInput[
		"Directory",
		FileNameDrop[PacletObject["Organizer"]["Location"], -1]
	];

    If[!DirectoryQ[dir],
        If[dir === $Canceled,
            Return[];
        ];
        Throw[StringForm["Invalid project name: ``", dir] ];
    ];

	PersistentValue["CG:Organizer:RootDirectory", "Local"] = dir;
];


Needs["Organizer`LogNotebookRuntime`"];
Needs["Organizer`Palette`"]

(* This function is meant to be called manually whenever there is a change to the standard
   set of docked cells or StyleDefinitions. *)
UpdateLogNotebooks[] := Module[{nbs, nbObj},
    nbs = FileNames[
        "Log.nb",
        FileNameJoin[{Organizer`Palette`Private`WorkspaceDirectory[], "Projects", "Active"}],
        Infinity
    ];

    Scan[
        Function[nbPath,
            nbObj = NotebookOpen[nbPath];

            installLogNotebookDockedCells[nbObj, FileNameSplit[nbPath][[-2]]];
            installLogNotebookStyles[nbObj];
        ],
        nbs
    ]
]


End[]

EndPackage[]
