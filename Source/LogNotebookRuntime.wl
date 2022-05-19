(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["ConnorGray`Organizer`LogNotebookRuntime`"]

FindQueueChapterCell
FindDailyChapterCell
GroupSelectionMove

$LogNotebookBackground = Blend[{Darker@Orange, Red}]

InstallLogNotebookDockedCells
InstallLogNotebookStyles

(* These functions are part of the DockedCells toolbar. Renaming them is a
   backwards-incompatible change. *)
InsertTodoAfterSelection = ConnorGray`Organizer`Notebook`InsertTodoAfterSelection;
InsertTodoForToday
InsertTodoAtTopOfQueue

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Toolbar`"]

If[MissingQ @ PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"],
	PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"] = {
		{LightBlue, LightCyan, LightGreen},
		{LightYellow, LightOrange, LightRed}
	};
]

(**************************************)
(* TODO Insertion                     *)
(**************************************)

InsertTodoForToday[nb_NotebookObject] := Try @ Module[{
	dailyChapterCell,
	todaySectionCell,
	monthSectionCell,
	cellsAfterThisMonth
},
	dailyChapterCell = Confirm @ FindDailyChapterCell[nb];

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
			_ :> Confirm @ FailureMessage[
				Organizer::error,
				"Expected NotebookWrite[] / SelectedCells[] to result in the cell which was written"
			]
		}];
		SelectionMove[nb, After, Cell];

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
		WriteTodoAndSelect[nb];

		Return[];
	];

	(* Insert a new TODO cell at the end of the existing subsubsection for the current day. *)
	moveSelectionToEndOfSection[todaySectionCell];
	WriteTodoAndSelect[nb];
]

(* The Queue is for Last-in first-out (LIFO) style tasks. Anything which is a bit
   longer-term than that should go in another section, e.g. Features. This implies that
   *most* of the time, we want to insert the new TODO at the *top* of the Queue, so we
   do just that. *)
InsertTodoAtTopOfQueue[nb_NotebookObject] := Try @ Module[{
	queueChapterCell
},
	queueChapterCell = Confirm @ FindQueueChapterCell[nb];

	SelectionMove[queueChapterCell, After, Cell];
	WriteTodoAndSelect[nb];
]

(* Thanks Dad for contributing this. *)
moveSelectionToEndOfSection[heading_CellObject] := Module[{cells},
	GroupSelectionMove[heading, After]
]

(*
	Move the selection relative to the cell group associated with `heading`.

	If `heading` is not the first cell in a CellGroup (because it has no child cells),
	the selected cells will only include `heading` itself. This essentially works around
	the undesirable behavior that `SelectionMove[heading, All, CellGroup]` has in the
	situation that `heading` is not the head of it's own cell group, which is to instead
	select the *parent* cell group which `heading` is a part of.
*)
GroupSelectionMove[heading_CellObject, dir_] := Module[{nb, cells},
	nb = ParentNotebook[heading];
	SelectionMove[heading, All, CellGroup];
	cells = SelectedCells[nb];

	(* Hacky way of checking if `heading` is the head of a CellGroup. *)
	If[First[cells] === heading,
		(* Use SelectedNotebook[] because the current situation is that multiple cells are
		   selected, and `heading` is the first cell in that group. We want to move to the
		   end of that set of selected cells. If we used `heading` here, we would only be
		   moving to the cell immediately after `heading`, which won't be at the end of
		   the group. *)
		SelectionMove[nb, dir, Cell],
		SelectionMove[heading, dir, Cell]
	];
]

FindQueueChapterCell[nb_NotebookObject] := findChapterCell[nb, "Queue"]
FindDailyChapterCell[nb_NotebookObject] := findChapterCell[nb, "Daily"]

findChapterCell[nb_NotebookObject, contents_?StringQ] := Try @ Module[{cell},
	cell = SelectFirst[
		Cells[nb, CellStyle -> "Chapter"],
		MatchQ[
			NotebookRead[#],
			Cell[contents, "Chapter", ___]
		] &
	];

	If[!MatchQ[cell, CellObject[___]],
		Confirm @ FailureMessage[
			Organizer::error,
			"No `` chapter exists in current notebook: ``",
			{InputForm[contents], Information[nb]["WindowTitle"]}
		];
	];

	cell
]

(**************************************)
(* ::Chapter:: *)
(* New Notebook Setup                 *)
(**************************************)

InstallLogNotebookDockedCells[nbObj_, projName_?StringQ] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	buttonOptions,
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	openFolderButton, row, cell
},
	(* Options shared by all buttons in the toolbar *)
	buttonOptions = Sequence[
		$ButtonBarOptions,
		Background -> $LogNotebookBackground,
		ImageMargins -> {{10,10},{10,10}}
	];

	newTodayTodoButton = Button[
		IconButtonContent[GetIcon["CalendarWithPlus"], "Insert new TODO item for today"],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoForToday[SelectedNotebook[]];
		),
		Background -> $LogNotebookBackground,
		$ButtonBarOptions
	];

	newTodoAtTopOfQueueButton = Button[
		IconButtonContent[
			GetIcon["UnfinishedTodoList"],
			"Insert new TODO item at the top of the Queue"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertTodoAtTopOfQueue[SelectedNotebook[]];
		),
		Background -> $LogNotebookBackground,
		$ButtonBarOptions
	];

	openFolderButton = Button[
		IconButtonContent[
			GetIcon["OpenFolder"],
			"Open the folder containing the current Log notebook"
		],
		Function[
			Switch[$SystemID,
				"MacOSX-x86-64",
					RunProcess[{"open", NotebookDirectory[]}],
				_,
					Confirm @ FailureMessage[
						Organizer::error,
						"Unhandled $SystemID for opening folder: ``",
						{InputForm[$SystemID]}
					];
			]
		],
		buttonOptions
	];

	row = Row[{
		(* Make the Log.nb title a hidden button which opens the organizer palette. This
		   is a quick and convenient way to access the palette without needing to keep
		   it open all of the time. *)
		Button[
			Style[Pane[projName, ImageMargins -> 10], "Subchapter", White],
			(
				ReleaseHold[loadOrFail];
				OpenOrganizerPalette[]
			),
			Appearance -> None
		],
		Row[{
			MakeNewTodoButton[$LogNotebookBackground],
			newTodayTodoButton,
			newTodoAtTopOfQueueButton
		}, ImageMargins -> 10],
		Confirm @ MakeLinkButtonRow[Background -> $LogNotebookBackground],
		openFolderButton,
		Confirm @ MakeColorPickerButtonGrid[]
	}];

	cell = Cell[
		BoxData[ToBoxes@row],
		Background -> $LogNotebookBackground
	];

	SetOptions[nbObj, DockedCells->{cell}]
]
]

(* This definition kept for backwards compatibility, and potential future customization. *)
InstallLogNotebookStyles[nb_NotebookObject] :=
	InstallNotebookStyles[nb]


End[]

EndPackage[]
