(* Definitions for Organizer 'Design'-type notebooks. *)

BeginPackage["ConnorGray`Organizer`Notebook`Design`"]


CreateDesignNotebook::usage = "CreateDesignNotebook[name] creates a new 'Design' style Organizer notebook. Design notebooks contain key points, details, and discussion of a design proposal."

(* $DesignNotebookBackground = RGBColor["#68c5db"]; *)
$DesignNotebookBackground = RGBColor["#0197f6"];


Begin["`Private`"]

Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]
Needs["ConnorGray`Organizer`Utils`"]


CreateDesignNotebook[listName_?StringQ] := Try @ Module[{
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
	Confirm @ InstallDesignDockedCells[nbObj, listName];

	Confirm @ SetNotebookTaggingRules[nbObj, "Design"];

	nbObj
]

(*======================================*)

InstallDesignDockedCells[
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
			"Design"
		],
		Background -> $DesignNotebookBackground
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