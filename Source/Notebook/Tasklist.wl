(* Definitions for Organizer 'Tasklist'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Tasklist`"]


CreateTasklistNotebook::usage = "CreateTasklistNotebook[name] creates a new 'Tasklist' style Organizer notebook. Tasklist notebooks contain sequences of related TODOs."

$TasklistNotebookBackground = Darker[Green];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateTasklistNotebook[listName_?StringQ] := Handle[_Failure] @ Module[{
	nbObj
},
	nbObj = CreateNotebook[];

	NotebookWrite[nbObj, Cell[listName, "Title"] ];
	NotebookWrite[
		nbObj,
		Cell[
			"Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}],
			"Subtitle"
		]
	];

	RaiseConfirm @ InstallNotebookStyles[nbObj];
	RaiseConfirm @ InstallTasklistDockedCells[nbObj, listName];

	RaiseConfirm @ SetNotebookTaggingRules[nbObj, "Tasklist"];

	nbObj
]

(*======================================*)

InstallTasklistDockedCells[
	nbObj_NotebookObject,
	listName_?StringQ
] := With[{},
	SetOptions[
		nbObj,
		DockedCells -> MakeOrganizerDockedCells[
			listName,
			"Tasklist",
			$TasklistNotebookBackground
		]
	]
]



End[]

EndPackage[]