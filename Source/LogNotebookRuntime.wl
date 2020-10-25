(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["Organizer`LogNotebookRuntime`", {
	"Organizer`"
}]

(*********)
(* Icons *)
(*********)

LoadIcons[] := (
	$iconCalendarWithPlus;
	$iconUnfinishedTodoList;
	$iconPlus;
)

(* Delay loading the icon (SetDelayed) until it's really needed. Then cache the loaded
   value. *)
$iconCalendarWithPlus := $iconCalendarWithPlus = ResourceFunction["SVGImport"][
	FileNameJoin[{
		PacletObject["Organizer"]["AssetLocation", "Icons"],
		"CalendarWithPlus.svg"
	}]
]

(* Delay loading the icon (SetDelayed) until it's really needed. Then cache the loaded
   value. *)
$iconUnfinishedTodoList := $iconUnfinishedTodoList = ResourceFunction["SVGImport"][
	FileNameJoin[{
		PacletObject["Organizer"]["AssetLocation", "Icons"],
		"UnfinishedTodoList.svg"
	}]
]

$iconPlus := $iconPlus = ResourceFunction["SVGImport"][
	FileNameJoin[{
		PacletObject["Organizer"]["AssetLocation", "Icons"],
		"Plus.svg"
	}]
]

(* ::Text:: *)


(* ::Subsubsection:: *)
(*TODOs*)


fCreateTodoCell[] := Module[{input, row},
	input = InputField["", String,
		Appearance -> "Frameless",
		ImageSize -> {600, Automatic},
		FieldHint -> "Empty TODO",
		BaseStyle -> {
			"Text",
			FontWeight -> Plain
		}
	];
	row = Row[{(*Checkbox[False]*)Checkbox[1, {1,2,3}], input}];

	Cell[BoxData @ ToBoxes @ row, "Text", CellMargins -> {{66, 0}, {0, 1}}]
]


fInsertTodoAfterSelection[] := Module[{newCell, nb},
	newCell = fCreateTodoCell[];
	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]

insertTodoForToday[nb_NotebookObject] := Module[{
	dailyChapterCell,
	todaySectionCell,
	monthSectionCell,
	cellsAfterThisMonth
},
	dailyChapterCell = SelectFirst[
		Cells[nb, CellStyle -> "Chapter"],
		MatchQ[
			NotebookRead[#],
			Cell["Daily", "Chapter", ___]
		] &
	];

	If[!MatchQ[dailyChapterCell, CellObject[___]],
		MessageDialog[StringForm[
			"Organizer`.`: Unable to insert new TODO cell: no 'Daily' chapter exists in specified notebook: ``",
			Information[nb]["WindowTitle"]
		]];
		Return[$Failed];
	];

	(*
		Check if the current notebook contains a Subsection for the current month.
	*)

	monthSectionCell = SelectFirst[
		Cells[nb, CellStyle -> "Subsection"],
		MatchQ[
			NotebookRead[#],
			Cell[DateString[{"MonthName", " ", "Year"}], "Subsection", ___]
		] &
	];

	If[MissingQ[monthSectionCell],
		moveSelectionToEndOfSection[dailyChapterCell];

		(* 3rd arg `All` is used so that the written cell is selected. *)
		NotebookWrite[nb, Cell[DateString[{"MonthName", " ", "Year"}], "Subsection"], All];

		(* Get the cell we just wrote/selected. *)
		monthSectionCell = Replace[SelectedCells[], {
			{cell_CellObject} :> cell,
			_ :> Throw["Expected NotebookWrite[] / SelectedCells[] to result in the cell which was written"]
		}];
		SelectionMove[nb, After, Cell]

		Assert[MatchQ[NotebookRead[monthSectionCell], Cell[_, "Subsection"]]]
	];

	(* Get all the cells which come *after* `monthSectionCell` in `nb`. This is necessary
	   because otherwise our search for the subsubsection cell which corresponds to the
	   current day might accidentally grab a cell which is from a previous year. This is
	   fairly unlikely, but we might as well do this right. *)
	cellsAfterThisMonth = Module[{cells},
		cells = Cells[nb];
		(* Drop all the cells before `monthSectionCell`. *)
		Drop[cells, LengthWhile[cells, # =!= monthSectionCell &]]
	];

	Assert[ListQ[cellsAfterThisMonth]];

	(*
		Check if the current notebook contains a Subsubsection which matches the current date.
	*)

	todaySectionCell = SelectFirst[
		(* Only check the cells which come after the `monthSectionCell`. *)
		cellsAfterThisMonth,
		MatchQ[
			NotebookRead[#],
			Cell[DateString[{"DayName", ", ", "MonthName", " ", "Day"}], "Subsubsection", ___]
		] &
	];

	If[MissingQ[todaySectionCell],
		(* Insert a subsubsection for the current date, and insert a new TODO cell inside it. *)
		moveSelectionToEndOfSection[monthSectionCell];
		NotebookWrite[nb, Cell[DateString[{"DayName", ", ", "MonthName", " ", "Day"}], "Subsubsection"]];
		NotebookWrite[nb, fCreateTodoCell[]];

		Return[];
	];

	(* Insert a new TODO cell at the end of the existing subsubsection for the current day. *)
	moveSelectionToEndOfSection[todaySectionCell];
	NotebookWrite[nb, fCreateTodoCell[]];
]

(* The Queue is for Last-in first-out (LIFO) style tasks. Anything which is a bit
   longer-term than that should go in another section, e.g. Features. This implies that
   *most* of the time, we want to insert the new TODO at the *top* of the Queue, so we
   do just that. *)
insertTodoAtTopOfQueue[nb_NotebookObject] := Module[{
	queueChapterCell
},
	queueChapterCell = SelectFirst[
		Cells[nb, CellStyle -> "Chapter"],
		MatchQ[
			NotebookRead[#],
			Cell["Queue", "Chapter", ___]
		] &
	];

	If[!MatchQ[queueChapterCell, CellObject[___]],
		MessageDialog[StringForm[
			"Organizer`.`: Unable to insert new TODO cell: no 'Queue' chapter exists in specified notebook: ``",
			Information[nb]["WindowTitle"]
		]];
		Return[$Failed];
	];

	SelectionMove[queueChapterCell, After, Cell];
	NotebookWrite[nb, fCreateTodoCell[]];
]

(* Thanks Dad for contributing this. *)
moveSelectionToEndOfSection[heading_CellObject] := Module[{cells},
	SelectionMove[heading, All, CellGroup];
	cells = SelectedCells[];
	(* Hacky way of checking if `heading` is the head of a CellGroup. *)
	If[First[cells] === heading,
		(* Use SelectedNotebook[] because the current situation is that multiple cells are
		   selected, and `heading` is the first cell in that group. We want to move to the
		   end of that set of selected cells. If we used `heading` here, we would only be
		   moving to the cell immediately after `heading`, which won't be at the end of
		   the group. *)
		SelectionMove[SelectedNotebook[], After, Cell],
		SelectionMove[heading, After, Cell]
	];
]

(* ::Subsubsection:: *)
(*Links*)


fCreateHyperlinkCell[] := Module[{filepath, label},
	filepath = SystemDialogInput["FileOpen"];
	If[!StringQ[filepath],
		Throw[StringForm["Invalid file path: ``", filepath]];
	];

	label = FileNameTake[filepath, -1];
	MessageDialog[{filepath, label}];

	Cell[
		BoxData @ ToBoxes @ Framed[
			Hyperlink[Style[label, Bold], filepath],
			RoundingRadius -> 5,
			Background -> LightBlue,
			FrameStyle -> Directive[Thick, Darker@Green]
		],
		CellMargins -> {{66, 0}, {0, 1}}
	]
]

fInsertLinkAfterSelection[] := Module[{newCell, nb},
	newCell = fCreateHyperlinkCell[];
	If[FailureQ @ newCell,
		Return[$Failed];
	];

	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


fGetDraggedHyperlink[] := Module[{path, res, data, hyperlink},
	(* TODO: Make path cross-platform. *)
	If[$SystemID =!= "MacOSX-x86-64",
		Throw["Cannot get dragged link on non-MacOSX platforms."];
	];

	path = FileNameJoin[{$HomeDirectory, "Desktop/dragged_link.html"}];
	CreateFile[path];
	RunProcess[{"open", "-a", "TextEdit", path}];

	(* Block on a dialog window until the user clicks to proceed.
	prevents us from reading from `dragged_link.html` before the
	user has actually had time to drag/paste anything in. *)
	res = DialogInput[DialogNotebook[{
		Button[
			Style["Finished Dragged Link", 30],
			DialogReturn[1],
			Background -> LightGreen,
			ImageMargins -> {{20,20},{30,20}},
			FrameMargins -> 120
		]
	}]];

	data = Import[path, {{"Hyperlinks", "Plaintext"}}];

	DeleteFile[path];

	hyperlink = Replace[data, {
		{{link_}, label_} :> Hyperlink[Style[label, 12], URL[link]],
		_ :> Throw[StringForm[
			"Dragged link did not have the expected format after Import: ``",
			data
		]]
	}];

	Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"]
]


fInsertDraggedHyperlink[] := Module[{newCell, nb},
	newCell = fGetDraggedHyperlink[];

	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


(* ::Chapter:: *)
(*New NB setup*)

iconButtonContent[icon_, tooltip_?StringQ] := Tooltip[
	Show[
		icon,
		ImageSize -> 20
	],
	tooltip,
	TooltipDelay -> 0.333
]

fInstallLogNotebookDockedCells[nbObj_, projName_?StringQ] := Module[{
	buttonOptions, newTODObutton,
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	newFileLinkButton, newDraggedLinkButton,
	openFolderButton, row, cell
},
	(* Options shared by all buttons in the toolbar *)
	buttonOptions = Sequence[
		Background -> Blend[{Darker@Orange,Red}],
		ContentPadding -> None,
		ImageMargins -> {{10,10},{10,10}},
		FrameMargins -> 7
	];

	newTODObutton = Button[
		iconButtonContent[$iconPlus, "Insert new TODO after current selection"],
		fInsertTodoAfterSelection[],
		buttonOptions
	];

	newTodayTodoButton = Button[
		iconButtonContent[$iconCalendarWithPlus, "Insert new TODO item for today"],
		insertTodoForToday[SelectedNotebook[]],
		buttonOptions
	];

	newTodoAtTopOfQueueButton = Button[
		iconButtonContent[
			$iconUnfinishedTodoList,
			"Insert new TODO item at the top of the Queue"
		],
		insertTodoAtTopOfQueue[SelectedNotebook[]],
		buttonOptions
	];

	newFileLinkButton = Button[
		"File Link",
		fInsertLinkAfterSelection[],
		buttonOptions,
		Method -> "Queued"
	];

	newDraggedLinkButton = Button[
		"Dragged Link",
		fInsertDraggedHyperlink[],
		buttonOptions,
		Method -> "Queued"
	];

	openFolderButton = Button[
		"Open Folder",
		Function[
			Switch[$SystemID,
				"MacOSX-x86-64",
					RunProcess[{"open", NotebookDirectory[]}],
				_,
					Throw[StringForm["Unhandled $SystemID: ``", $SystemID]]
			]
		],
		buttonOptions
	];

	row = Row[{
		Style[Pane[projName, ImageMargins -> 10], "Subchapter", White],
		newTODObutton,
		newTodayTodoButton,
		newTodoAtTopOfQueueButton,
		newFileLinkButton,
		newDraggedLinkButton,
		openFolderButton
	}];

	cell = Cell[
		BoxData[ToBoxes@row],
		Background -> Blend[{Darker@Orange,Red}]
	];

	SetOptions[nbObj, DockedCells->{cell}]
]


EndPackage[]
