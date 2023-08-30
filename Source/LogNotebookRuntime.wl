(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["ConnorGray`Organizer`LogNotebookRuntime`"]

FindQueueChapterCell
FindDailyChapterCell
GroupSelectionMove

(* These functions are part of the DockedCells toolbar. Renaming them is a
   backwards-incompatible change. *)
InsertTodoAfterSelection = ConnorGray`Organizer`Notebook`InsertTodoAfterSelection;
InsertTodoForToday
InsertTodoAtTopOfQueue
HandleCreateNewFile

Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Utils`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]
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

InsertTodoForToday[nb_NotebookObject] := Handle[_Failure] @ Module[{
	dailyChapterCell,
	todaySectionCell,
	monthSectionCell,
	cellsAfterThisMonth
},
	dailyChapterCell = RaiseConfirm @ FindDailyChapterCell[nb];

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
			_ :> Raise[
				OrganizerError,
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
InsertTodoAtTopOfQueue[nb_NotebookObject] := Handle[_Failure] @ Module[{
	queueChapterCell
},
	queueChapterCell = RaiseConfirm @ FindQueueChapterCell[nb];

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

findChapterCell[nb_NotebookObject, contents_?StringQ] := Handle[_Failure] @ Module[{cell},
	cell = SelectFirst[
		Cells[nb, CellStyle -> "Chapter"],
		MatchQ[
			NotebookRead[#],
			Cell[contents, "Chapter", ___]
		] &
	];

	If[!MatchQ[cell, CellObject[___]],
		Raise[
			OrganizerError,
			"No `` chapter exists in current notebook: ``",
			InputForm[contents],
			Information[nb]["WindowTitle"]
		];
	];

	cell
]

(*====================================*)
(* Creating new files in this project *)
(*====================================*)

HandleCreateNewFile[
	logNBObj_NotebookObject,
	type_?StringQ
] := Handle[_Failure] @ Module[{
	title,
	projectDir,
	notebookFile
},
	title = InputString[];

	If[!StringQ[title],
		If[title === $Canceled,
			Return[Null, Module];
		];
		Raise[
			OrganizerError,
			"Invalid `` notebook title: ``",
			type,
			InputForm[title]
		];
	];

	nbObj = RaiseConfirm @ CreateOrganizerNotebook[type, title];

	projectDir = Quiet[
		NotebookDirectory[logNBObj],
		{NotebookDirectory::nosv}
	];

	(* If the project notebook isn't saved, don't save this new notebook to
	   a relative location. *)
	If[!StringQ[projectDir],
		Return[Null, Module];
	];

	(* Offer the user the choice to save to a custom location. *)
	notebookFile = Replace[type, {
		"Tasklist" :> FileNameJoin[{projectDir, "Tasklists", title <> ".nb"}],
		(* Unrecognized notebook type. Don't save it for the user automatically. *)
		_ :> Return[Null, Module]
	}];

	(* Prevent NotebookSave from overwriting an existing file. *)
	If[FileType[notebookFile] =!= None,
		Raise[
			OrganizerError,
			"Unable to save new `` notebook: file with that name already exists: ``",
			type,
			InputForm[notebookFile]
		];
	];

	If[!DirectoryQ[FileNameDrop[notebookFile]],
		RaiseConfirm @ CreateDirectory[FileNameDrop[notebookFile]];
	];

	NotebookSave[nbObj, notebookFile]
]

(**************************************)
(* ::Chapter:: *)
(* New Notebook Setup                 *)
(**************************************)


End[]

EndPackage[]
