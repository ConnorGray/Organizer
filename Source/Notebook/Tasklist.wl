(* Definitions for Organizer 'Tasklist'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Tasklist`"]


CreateTasklistNotebook::usage = "CreateTasklistNotebook[name] creates a new 'Tasklist' style Organizer notebook. Tasklist notebooks contain sequences of related TODOs."

$TasklistNotebookBackground = Darker[Green];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateTasklistNotebook[title_?StringQ] := Handle[_Failure] @ Module[{
	nb
},
	nb = Notebook[{
		Cell[title, "Title"],
		Cell[
			"Created " <> DateString[
				Now,
				{"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}
			],
			"Subtitle"
		]
	}];

	CreateOrganizerNotebookFromSettings[
		nb,
		title,
		"Tasklist",
		"Tasklist",
		$TasklistNotebookBackground
	]
]


End[]

EndPackage[]