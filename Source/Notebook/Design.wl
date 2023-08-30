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


CreateDesignNotebook[listName_?StringQ] := Handle[_Failure] @ Module[{
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
	RaiseConfirm @ InstallDesignDockedCells[nbObj, listName];

	RaiseConfirm @ SetNotebookTaggingRules[nbObj, "Design"];

	nbObj
]

(*======================================*)

InstallDesignDockedCells[
	nbObj_NotebookObject,
	listName_?StringQ
] := With[{},
	SetOptions[
		nbObj,
		DockedCells -> MakeOrganizerDockedCells[
			listName,
			"Design",
			$DesignNotebookBackground
		]
	]
]



End[]

EndPackage[]