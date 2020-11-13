(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["Organizer`LogNotebookRuntime`", {
	"Organizer`",
	"Organizer`Utils`"
}]

If[MissingQ @ PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"],
	PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"] = {
		{LightRed, LightGreen, LightBlue},
		{LightYellow, LightOrange, LightCyan}
	};
]

(*********)
(* Icons *)
(*********)

importSVG[path_?StringQ] := Module[{func},
	func = ResourceFunction["SVGImport"];
	If[FailureQ[func],
		Throw["ResourceFunction[\"SVGImport\"] could not be loaded."];
	];

	func[path]
]

importIcon[filename_?StringQ] := importSVG[
	FileNameJoin[{
		PacletObject["Organizer"]["AssetLocation", "Icons"],
		filename
	}]
]

LoadIcons[] := (
	$iconCalendarWithPlus;
	$iconUnfinishedTodoList;
	$iconPlus;
	$iconFileLink;
	$iconLinkArea;
	$iconOpenFolder;
)

(* Delay loading the icon (SetDelayed) until it's really needed. Then cache the loaded
   value. *)
$iconCalendarWithPlus   := $iconCalendarWithPlus   = importIcon["CalendarWithPlus.svg"]
$iconUnfinishedTodoList := $iconUnfinishedTodoList = importIcon["UnfinishedTodoList.svg"]
$iconPlus               := $iconPlus               = importIcon["Plus.svg"]
$iconFileLink           := $iconFileLink           = importIcon["FileLink.svg"]
$iconLinkArea           := $iconLinkArea           = importIcon["LinkArea.svg"]
$iconOpenFolder         := $iconOpenFolder         = importIcon["OpenFolder.svg"]


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

(*
createTodoCell[] := Cell[
	BoxData[FormBox[
		RowBox[{ToBoxes@Checkbox[1, {1, 2, 3}], ToBoxes@Placeholder["Empty TODO"]}],
		"Text"
	]],
	"Text",
	CellMargins -> {{66, 0}, {0, 1}}
]
*)

(*
	Replace the current EvaluationCell[] with an inert TODO cell, and move the cursor
	inside of the cell. This happens in response to click or key down event inside a
	non-inert TODO cell.

	This function must be kept in sync with createTodoCell[].
*)
replaceWithInertTodoCellAndSelect[content_?StringQ] := Module[{cell},
	cell = Cell[
		(* This LetterQ check prevents us from putting a non-printable character in the new
		cell, which can happen if the user types e.g. a down arrow immediately after
		creating the cell (CurrentValue["EventKey"] would be a non-printable character in
		that situtation). *)
		TextData[If[LetterQ[content], content, ""] ],
		(* This inherits styles from the StyleData["TODO", ..] in the Log.nb's
		StyleDefinitions property. *)
		"TODO"
	];

	NotebookWrite[EvaluationCell[], cell, All];
	SelectionMove[SelectedNotebook[], After, CellContents]
]

(* NOTE: Setting an explicit `Background -> White` here is required because CellFrameLabels
         inherit their styling from the parent cell, and we don't want the checkbox cell
		 to have a colored background. *)
CreateCheckboxCell[] := Cell[
	BoxData @ ToBoxes @ Checkbox[
		Dynamic[
			CurrentValue[
				ParentCell@EvaluationCell[],
				{TaggingRules, "TODOCompletedQ"}
			],
			Function[val,
				SetOptions[
					ParentCell@EvaluationCell[],
					TaggingRules -> {"TODOCompletedQ" -> val}
				]
			]
		]
	],
	Background -> White
]

(*
	This function must be kept in sync with replaceWithInertTodoCellAndSelect[]
*)
createTodoCell[] := Cell[
	BoxData @ ToBoxes @ Placeholder["Empty TODO"],
	"TODO",
	(* These cell event actions are triggered the first time a user interacts with the
	   cell. Their purpose is to:

		* Remove the Placeholder[..] BoxData content, and change the cell to be a
		  TextData[..]-type cell. This is necessary because the FE handles
		  Cell[BoxData[..]] and Cell[TextData[..]] very differently. Earlier versions of
		  the TODO cell attempted to combine box data and textual input, but they all had
		  weird minor issues, like spacing between apostrophes and other input characters
		  following the auto-whitespace behavior of "Input" cells.
		* Remove the event handlers themselves, so we're left behind with a very plain,
		  non-likely-to-crash cell which behaves like any other Text-style cell.
	*)
	CellEventActions -> {
		(* NOTE: "MouseDown" is used instead of "MouseClicked" because "MouseClicked"
		         is triggered by the click event from the "new todo" button being clicked,
				 causing the placeholder to be immediately replaced before the user has a
				 chance to see it. *)
		"MouseDown" :> replaceWithInertTodoCellAndSelect[""],
		"KeyDown" :> replaceWithInertTodoCellAndSelect[CurrentValue["EventKey"]]
	}
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

	nb = EvaluationNotebook[];
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

setSelectedCellsBackground[color_] := Module[{selectedCells},
	selectedCells = SelectedCells[];
	Assert[MatchQ[selectedCells, {___CellObject}]];
	Map[SetOptions[#, Background -> color] &, selectedCells]
]

colorPickerButtonGrid[] := With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{colors, grid},
	colors = PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"];

	If[MissingQ[colors],
		Throw["No \"CG:Organizer:BackgroundColorPalette\" PersistentValue is set."];
	];

	If[!MatchQ[colors, {{___RGBColor}..}],
		Throw["\"CG:Organizer:BackgroundColorPalette\" PersistentValue is not a valid array of colors."]
	];

	grid = Map[
		Button[
			"",
			(
				ReleaseHold[loadOrFail];
				setSelectedCellsBackground[#];
			),
			ImageSize -> {20, 20},
			Background -> #,
			ImageMargins -> 0,
			ContentPadding -> None
		] &,
		colors,
		{2}
	];

	Grid[grid, Spacings -> {0.0, .0}, ItemSize -> All, Frame -> True, FrameStyle -> Thickness[2.5]]
]
]

installLogNotebookDockedCells[nbObj_, projName_?StringQ] := With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
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
		(
			ReleaseHold[loadOrFail];
			insertTodoAfterSelection[];
		),
		buttonBarOptions
	];

	newTodayTodoButton = Button[
		iconButtonContent[$iconCalendarWithPlus, "Insert new TODO item for today"],
		(
			ReleaseHold[loadOrFail];
			insertTodoForToday[SelectedNotebook[]];
		),
		buttonBarOptions
	];

	newTodoAtTopOfQueueButton = Button[
		iconButtonContent[
			$iconUnfinishedTodoList,
			"Insert new TODO item at the top of the Queue"
		],
		(
			ReleaseHold[loadOrFail];
			insertTodoAtTopOfQueue[SelectedNotebook[]];
		),
		buttonBarOptions
	];

	newFileLinkButton = Button[
		iconButtonContent[
			$iconFileLink,
			"Insert a link to a file chosen from the file system"
		],
		(
			ReleaseHold[loadOrFail];
			insertLinkAfterSelection[];
		),
		buttonBarOptions,
		Method -> "Queued"
	];

	newDraggedLinkButton = Button[
		iconButtonContent[
			$iconLinkArea,
			"Insert a link dragged into a popup TextEdit window"
		],
		(
			ReleaseHold[loadOrFail];
			insertDraggedHyperlink[];
		),
		buttonBarOptions,
		Method -> "Queued"
	];

	openFolderButton = Button[
		iconButtonContent[
			$iconOpenFolder,
			"Open the folder containing the current Log notebook"
		],
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
		(* Make the Log.nb title a hidden button which opens the organizer palette. This
		   is a quick and convenient way to access the palette without needing to keep
		   it open all of the time. *)
		Button[
			Style[Pane[projName, ImageMargins -> 10], "Subchapter", White],
			(
				ReleaseHold[loadOrFail];
				CreateOrganizerPalette[]
			),
			Appearance -> None
		],
		Row[{
			newTODObutton,
			newTodayTodoButton,
			newTodoAtTopOfQueueButton
		}, ImageMargins -> 10],
		Row[{
			newFileLinkButton,
			newDraggedLinkButton
		}, ImageMargins -> 10],
		openFolderButton,
		colorPickerButtonGrid[]
	}];

	cell = Cell[
		BoxData[ToBoxes@row],
		Background -> Blend[{Darker@Orange,Red}]
	];

	SetOptions[nbObj, DockedCells->{cell}]
]
]

installLogNotebookStyles[nb_NotebookObject] := (
	SetOptions[nb,
		StyleDefinitions -> Notebook[{
			Cell[StyleData[StyleDefinitions -> "Default.nb"] ],
			Cell[StyleData["TODO", StyleDefinitions -> StyleData["Text"] ],
				TaggingRules -> {"TODOCompletedQ" -> False},
				LineSpacing -> {0.95, 0},
				CellMargins -> {{66, 0}, {2, 2}},
				CellFrame -> {{2, 0}, {0, 0}},
				CellFrameColor -> GrayLevel[0.7],
				CellFrameMargins -> 5,
				CellFrameLabelMargins -> 3,
				CellFrameLabels -> {
					{CreateCheckboxCell[], None},
					{None, None}
				}
			]
		}]
	];
)


EndPackage[]
