BeginPackage["ConnorGray`Organizer`"]

OpenOrganizerPalette
UpdateLogNotebooks

Begin["`Private`"]

If[$VersionNumber >= 12.3,
	(* TODO: Once the "WolframVersion" of Organizer is increased to at least 12.3+, update
	         the code to use PersistentSymbol, and remove this Off call. *)
	Off[PersistentValue::obs]
];

Needs["ConnorGray`Organizer`LogNotebookRuntime`"];
Needs["ConnorGray`Organizer`Palette`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]
Needs["ConnorGray`Organizer`Notebook`Tasklist`"]
Needs["ConnorGray`Organizer`Toolbar`"]

(* This function is meant to be called manually whenever there is a change to the standard
   set of docked cells or StyleDefinitions. *)
UpdateLogNotebooks[] := Try @ Module[{nbs, nbObj},
    nbs = FileNames[
        "Log.nb",
        Confirm @ ConnorGray`Organizer`Palette`WorkspaceDirectory[],
        Infinity
    ];

	ConfirmMatchQ[nbs, {___?StringQ}];

    Scan[
		nbPath |-> NotebookProcess[
			nbPath,
			Function[nbObj, (
				InstallLogNotebookDockedCells[nbObj, FileNameSplit[nbPath][[-2]]];
				InstallLogNotebookStyles[nbObj];

				SetOptions[
					nbObj,
					TaggingRules -> {
						"CG:Organizer" -> {"DocumentType" -> "Log"}
					}
				];
			)],
			(* We're editing the notebooks, so save the notebook automatically, without
			   interactively prompting the user. *)
			"ForceSave" -> True
		],
        nbs
    ]
]


End[]

EndPackage[]
