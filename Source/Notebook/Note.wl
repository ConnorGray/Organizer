(* Definitions for Organizer 'Note'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Note`"]


CreateNoteNotebook::usage = "CreateNoteNotebook[name] creates a new 'Note' style Organizer notebook. Note notebooks contain collections, discussion, links, narrative writing, and more."

$NoteNotebookBackground = RGBColor["#219ebc"];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateNoteNotebook[listName_?StringQ] := Handle[_Failure] @ Module[{
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
	RaiseConfirm @ InstallNoteDockedCells[nbObj, listName];

	RaiseConfirm @ SetNotebookTaggingRules[nbObj, "Note"];

	nbObj
]

(*======================================*)

InstallNoteDockedCells[
	nbObj_NotebookObject,
	listName_?StringQ
] := With[{},
	SetOptions[
		nbObj,
		DockedCells -> MakeOrganizerDockedCells[
			listName,
			"Note",
			$NoteNotebookBackground
		]
	]
]



End[]

EndPackage[]
