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
	toolbarRow,
	titleCell,
	toolbarCell
},
	toolbarRow = GridBox[{{
		MakeNewTodoButton[],
		Splice @ Confirm @ MakeLinkButtonRow[],
		ToBoxes @ Confirm @ MakeColorPickerButtonGrid[]
	}},
		GridBoxDividers -> {
			"Rows" -> {{None}},
			"ColumnsIndexed" -> {
				2 -> GrayLevel[0.7],
				5 -> GrayLevel[0.7]
			}
		},
		GridBoxSpacings -> {
			"Columns" -> {{0.2}},
			"ColumnsIndexed" -> {
				2 -> 1,
				5 -> 1
			}
		}
	];

	titleCell = Cell[
		BoxData @ MakeTitleBarCellBoxes[
			listName,
			"Tasklist"
		],
		Background -> $TasklistNotebookBackground
	];

	toolbarCell = Cell[
		BoxData[toolbarRow],
		Background -> GrayLevel[0.9],
		CellFrameMargins -> {{Inherited, Inherited}, {-1, 1}}
	];

	SetOptions[nbObj, DockedCells -> {
		titleCell,
		toolbarCell	
	}]
]]



End[]

EndPackage[]