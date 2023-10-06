BeginPackage["ConnorGray`Organizer`"]

OpenOrganizerPalette
UpdateLogNotebooks

CreateOrganizerNotebook::usage = "CreateOrganizerNotebook[type, title] creates a new Organizer notebook of the specified type."

CreateQueuesReport::usage = "CreateQueuesReport[] creates a new Queues report from the Log notebooks in the current Workspace > Category."
CreateDailysReport::usage = "CreateDailysReport[] creates a new Dailys report from the Log notebooks in the current Workspace > Category."

PacletInstall /@ PacletObject["ConnorGray/Organizer"]["Dependencies"]

Begin["`Private`"]

If[$VersionNumber >= 12.3,
	(* TODO: Once the "WolframVersion" of Organizer is increased to at least 12.3+, update
	         the code to use PersistentSymbol, and remove this Off call. *)
	Off[PersistentValue::obs]
];

Needs["ConnorGray`Organizer`Errors`"];
Needs["ConnorGray`Organizer`LogNotebookRuntime`"];
Needs["ConnorGray`Organizer`Palette`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]
Needs["ConnorGray`Organizer`Notebook`Tasklist`"]
Needs["ConnorGray`Organizer`Notebook`Design`"]
Needs["ConnorGray`Organizer`Notebook`BugReport`"]
Needs["ConnorGray`Organizer`Toolbar`"]

(*====================================*)

CreateOrganizerNotebook[
	type_?StringQ,
	title0 : _?StringQ : Automatic
] := Handle[_Failure] @ Module[{
	title
},
	title = ConfirmReplace[title0, {
		s_?StringQ :> s,
		Automatic :> Module[{input},
			input = InputString[
				TemplateApply[
					"Please enter a title for your new Organizer \"``\" notebook:",
					{type}
				]
			];

			If[!StringQ[input],
				Raise[
					OrganizerError,
					"Error prompting user for Organizer notebook title: ``",
					input
				];
			];

			input
		]
	}];

	Replace[type, {
		"Log" :> CreateLogNotebook[title],
		"Tasklist" :> CreateTasklistNotebook[title],
		"BugReport" :> CreateBugReportNotebook[title],
		"Design" :> CreateDesignNotebook[title],
		_ :> CreateFailure[OrganizerError, "Unknown Organizer notebook type: ``", type]
	}]
]

(*====================================*)

(* This function is meant to be called manually whenever there is a change to the standard
   set of docked cells or StyleDefinitions. *)
UpdateLogNotebooks[] := Handle[_Failure] @ Module[{
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
        RaiseConfirm @ ConnorGray`Organizer`Palette`OrganizerDirectory[],
        Infinity
    ];

	RaiseConfirmMatch[nbs, {___?StringQ}];

	updateLogNotebook[nbPath_?StringQ] :=
		NotebookProcess[
			nbPath,
			Function[nbObj, AbortProtect @ (
				InstallLogNotebookDockedCells[nbObj, FileNameSplit[nbPath][[-2]]];
				InstallLogNotebookStyles[nbObj];

				RaiseConfirm @ SetNotebookTaggingRules[nbObj, "Log"];
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
