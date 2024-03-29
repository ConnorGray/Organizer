(* Definitions for Organizer 'BugReport'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`BugReport`"]

CreateBugReportNotebook::usage = "CreateBugReportNotebook[name] creates a new 'BugReport' style Organizer notebook. BugReport notebooks contain system information and steps to reproduce a bug."

$BugReportNotebookBackground = RGBColor["#f5d491"];
$BugReportNotebookBackground = RGBColor["#be6e46"];

Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]

CreateBugReportNotebook[title_?StringQ] := Handle[_Failure] @ Module[{
	nb
},
	nb = Notebook[
		{
			Cell[title, "Title"],
			Cell[
				"Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}],
				"Subtitle"
			],

			Cell["Setup", "Chapter"],
			Cell[BoxData["$Version"], "Input"],
			Cell[
				BoxData @ ToBoxes @ Unevaluated[
					Dataset @ ResourceFunction["ToAssociations"][SystemInformation["Small"]]
				],
				"Input"
			],

			Cell["One-time", "Subsection"],
			Cell["Some explanation ...", "Subsubtitle"],
			Cell["Initialization", "Subsection"],
			Cell[
				BoxData @ ToBoxes @ Unevaluated[
					PacletDirectoryLoad[Placeholder["paclet"]]
				],
				"Code"
			],

			Cell["Steps to Reproduce", "Chapter"]
		},
		WindowTitle -> title
	];

	CreateOrganizerNotebookFromSettings[
		nb,
		title,
		"BugReport",
		"Bug Report",
		$BugReportNotebookBackground
	]
]

(*======================================*)


End[]

EndPackage[]