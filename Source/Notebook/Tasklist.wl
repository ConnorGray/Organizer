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

	SetOptions[
		nbObj,
		TaggingRules -> {
			"CG:Organizer" -> {"DocumentType" -> "Tasklist"}
		}
	];

	nbObj
]

(*======================================*)

InstallTasklistDockedCells[
	nbObj_NotebookObject,
	listName_?StringQ
] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	row,

	cell
},
	row = Row[{
		(* Make the Log.nb title a hidden button which opens the organizer palette. This
		is a quick and convenient way to access the palette without needing to keep
		it open all of the time. *)
		Button[
			Style[Pane[listName, ImageMargins -> 10], "Subchapter", White],
			(
				ReleaseHold[loadOrFail];
				OpenOrganizerPalette[]
			),
			Appearance -> None
		],
		Row[{
			MakeNewTodoButton[$TasklistNotebookBackground]
		}],
		Confirm @ MakeLinkButtonRow[Background -> $TasklistNotebookBackground],
		Confirm @ MakeColorPickerButtonGrid[]
	}];

	cell = Cell[
		BoxData[ToBoxes[row]],
		Background -> $TasklistNotebookBackground
	];

	SetOptions[nbObj, DockedCells -> {cell}]
]]



End[]

EndPackage[]