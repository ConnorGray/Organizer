(* Definitions for Organizer 'Tasklist'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Tasklist`"]


CreateTasklistNotebook::usage = "CreateTasklistNotebook[name] creates a new 'Tasklist' style Organizer notebook. Tasklist notebooks contain sequences of related TODOs."

$TasklistNotebookBackground = Darker[Green];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateTasklistNotebook[listName_?StringQ] := Try @ Module[{
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

	Confirm @ InstallNotebookStyles[nbObj];
	Confirm @ InstallTasklistDockedCells[nbObj, listName];

	Confirm @ SetNotebookTaggingRules[nbObj, "Tasklist"];

	nbObj
]

(*======================================*)

InstallTasklistDockedCells[
	nbObj_NotebookObject,
	listName_?StringQ
] := Try @ With[{},
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