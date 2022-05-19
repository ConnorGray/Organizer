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
UpdateLogNotebooks[] := Try @ Module[{
	nbs,
	nbObj,
	updateLogNotebook,
	processedFileCount = 0
},
	(* Load GeneralUtilities without putting it on the context path. We only
	   want to explicilty use symbols. *)
	Block[{$ContextPath = $ContextPath},
		Needs["GeneralUtilities`"]
	];

    nbs = FileNames[
        "Log.nb",
        Confirm @ ConnorGray`Organizer`Palette`OrganizerDirectory[],
        Infinity
    ];

	ConfirmMatchQ[nbs, {___?StringQ}];

	updateLogNotebook[nbPath_?StringQ] :=
		NotebookProcess[
			nbPath,
			Function[nbObj, AbortProtect @ (
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
		];

	(*-----------*)
	(* Main loop *)
	(*-----------*)

	GeneralUtilities`ComputeWithProgress[
		callback |-> Scan[
			path |-> (
				(* Update the progress monitoring. *)
				callback[<|
					"progress" -> processedFileCount / Length[nbs],
					"processed" -> processedFileCount,
					"totalNotebooks" -> Length[nbs],
					(* Get the Workspace/Category/Project/Filename.nb components for display. *)
					"filename" -> FileNameTake[path, -4]
				|>];

				updateLogNotebook[path];

				processedFileCount += 1;
			),
			nbs
		],
		<|
			"Dynamic" -> "Processing `filename`. `processed` of `totalNotebooks`."
		|>,
		"MinimumProgressTime" -> 0.01,
		"SettlingTime" -> 0.1
	];

	Success["UpdateLogNotebooks", <|
		"Completed" -> processedFileCount,
		"Total" -> Length[nbs]
	|>]
]


End[]

EndPackage[]
