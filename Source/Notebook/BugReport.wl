(* Definitions for Organizer 'BugReport'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`BugReport`"]

CreateBugReportNotebook::usage = "CreateBugReportNotebook[name] creates a new 'BugReport' style Organizer notebook. BugReport notebooks contain system information and steps to reproduce a bug."

$BugReportNotebookBackground = RGBColor["#f5d491"];
$BugReportNotebookBackground = RGBColor["#be6e46"];

Begin["`Private`"]

Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]

CreateBugReportNotebook[title_?StringQ] := Try @ Module[{
	nb,
	nbObj
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

	nbObj = NotebookPut[nb, Visible -> False];

	Confirm @ InstallNotebookStyles[nbObj];
	Confirm @ installBugReportDockedCells[nbObj, title];
	Confirm @ SetNotebookTaggingRules[nbObj, "BugReport"];

	SetOptions[nbObj, Visible -> True];

	nbObj
]

(*======================================*)

installBugReportDockedCells[
	nbObj_NotebookObject,
	title_?StringQ
] := Try @ With[{},
	SetOptions[
		nbObj,
		DockedCells -> MakeOrganizerDockedCells[
			title,
			"Bug Report",
			$BugReportNotebookBackground
		]
	]
]

(*======================================*)


End[]

EndPackage[]