(* Definitions for Organizer 'Note'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Note`"]


CreateNoteNotebook::usage = "CreateNoteNotebook[name] creates a new 'Note' style Organizer notebook. Note notebooks contain collections, discussion, links, narrative writing, and more."

$NoteNotebookBackground = RGBColor["#219ebc"];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateNoteNotebook[title_?StringQ] := Handle[_Failure] @ Module[{
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
		"Note",
		"Note",
		$NoteNotebookBackground
	]
]


End[]

EndPackage[]
