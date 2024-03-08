(* Definitions for Organizer 'Design'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Design`"]


CreateDesignNotebook::usage = "CreateDesignNotebook[name] creates a new 'Design' style Organizer notebook. Design notebooks contain key points, details, and discussion of a design proposal."

(* $DesignNotebookBackground = RGBColor["#68c5db"]; *)
$DesignNotebookBackground = RGBColor["#0197f6"];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateDesignNotebook[title_?StringQ] := Handle[_Failure] @ Module[{
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
		"Design",
		"Design",
		$DesignNotebookBackground
	]
]


End[]

EndPackage[]