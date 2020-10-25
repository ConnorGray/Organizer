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


(* NOTE:
	Old createTodoCell[] implementation. I used this for about two years, but it had
	serious flaws, not least of which being that it would sporadically crash the front end
	when cutting / pasting the TODO cells.

createTodoCell[] := Module[{input, row},
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
*)

createTodoCell[] := Cell[
	BoxData[FormBox[
		RowBox[{ToBoxes@Checkbox[1, {1, 2, 3}], ToBoxes@Placeholder["Empty TODO"]}],
		"Text"
	]],
	"Text",
	CellMargins -> {{66, 0}, {0, 1}}
]

writeTodoAndSelect[nb_NotebookObject] := (
	(* NOTE:
		The `Placeholder` here does NOT refer to the \[SelectionPlaceholder] used
		immediately previous — it refered to the Placeholder["Empty TODO"] value from
		createTodoCell[]. The two different placeholders are entirely unrelated.
		\[SelectionPlaceholder is part of how NotebookApply works. Ideally, we'd be able
		to do just:

			NotebookWrite[nb, createTodoCell[], Placeholder]

		and not even involve NotebookApply. This seems like it should work per
		NotebookWrite's documentation, but it doesn't.
	*)
	NotebookWrite[nb, createTodoCell[], All];
	NotebookApply[nb, "\[SelectionPlaceholder]", Placeholder];
)

(**************************************)
(* TODO Insertion                     *)
(**************************************)

insertTodoAfterSelection[] := Module[{nb},
	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];

	writeTodoAndSelect[nb]
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
		writeTodoAndSelect[nb];

		Return[];
	];

	(* Insert a new TODO cell at the end of the existing subsubsection for the current day. *)
	moveSelectionToEndOfSection[todaySectionCell];
	writeTodoAndSelect[nb];
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
	writeTodoAndSelect[nb];
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


createHyperlinkCell[] := Module[{filepath, label},
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

insertLinkAfterSelection[] := Module[{newCell, nb},
	newCell = createHyperlinkCell[];
	If[FailureQ @ newCell,
		Return[$Failed];
	];

	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, newCell];
]


getDraggedHyperlink[] := Module[{path, res, data, hyperlink},
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


insertDraggedHyperlink[] := Module[{newCell, nb},
	newCell = getDraggedHyperlink[];

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

installLogNotebookDockedCells[nbObj_, projName_?StringQ] := Module[{
	buttonOptions, newTODObutton,
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	newFileLinkButton, newDraggedLinkButton,
	openFolderButton, row, cell
},
	buttonBarOptions = Sequence[
		Background -> Blend[{Darker@Orange,Red}],
		ContentPadding -> None,
		FrameMargins -> 7
	];

	(* Options shared by all buttons in the toolbar *)
	buttonOptions = Sequence[
		buttonBarOptions,
		ImageMargins -> {{10,10},{10,10}}
	];

	newTODObutton = Button[
		iconButtonContent[$iconPlus, "Insert new TODO after current selection"],
		insertTodoAfterSelection[],
		buttonBarOptions
	];

	newTodayTodoButton = Button[
		iconButtonContent[$iconCalendarWithPlus, "Insert new TODO item for today"],
		insertTodoForToday[SelectedNotebook[]],
		buttonBarOptions
	];

	newTodoAtTopOfQueueButton = Button[
		iconButtonContent[
			$iconUnfinishedTodoList,
			"Insert new TODO item at the top of the Queue"
		],
		insertTodoAtTopOfQueue[SelectedNotebook[]],
		buttonBarOptions
	];

	newFileLinkButton = Button[
		"File Link",
		insertLinkAfterSelection[],
		buttonOptions,
		Method -> "Queued"
	];

	newDraggedLinkButton = Button[
		"Dragged Link",
		insertDraggedHyperlink[],
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
		Row[{
			newTODObutton,
			newTodayTodoButton,
			newTodoAtTopOfQueueButton
		}, ImageMargins -> 10],
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
