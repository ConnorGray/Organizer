(* ::Package:: *)

(* ::Text:: *)
(*This file is loaded the `Projects/<Project Name>/Log.nb` files. It contains their "runtime" functions, i.e. the functions which are called by the button in their DockedCells toolbar.*)


(* ::Chapter:: *)
(*Runtime*)


BeginPackage["Organizer`LogNotebookRuntime`"]

FindQueueChapterCell
FindDailyChapterCell
GroupSelectionMove

InstallLogNotebookDockedCells
InstallLogNotebookStyles

(* These functions are part of the DockedCells toolbar. Renaming them is a
   backwards-incompatible change. *)
InsertTodoAfterSelection
InsertTodoForToday
InsertTodoAtTopOfQueue

Begin["`Private`"]

Needs["Organizer`"]
Needs["Organizer`Utils`"]

If[MissingQ @ PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"],
	PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"] = {
		{LightBlue, LightCyan, LightGreen},
		{LightYellow, LightOrange, LightRed}
	};
]

(*********)
(* Icons *)
(*********)

(*
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
*)

importIcon[filename_?StringQ] := Get[
	FileNameJoin[{
		PacletObject["Organizer"]["AssetLocation", "Icons"],
		filename <> ".wl"
	}]
]

LoadIcons[] := (
	$iconCalendarWithPlus;
	$iconUnfinishedTodoList;
	$iconPlus;
	$iconFileLink;
	$iconLinkArea;
	$iconOpenFolder;
	$iconMessageLink;
	$iconBrowserLink;
)

(* Delay loading the icon (SetDelayed) until it's really needed. Then cache the loaded
   value. *)
$iconCalendarWithPlus   := $iconCalendarWithPlus   = importIcon["CalendarWithPlus.svg"]
$iconUnfinishedTodoList := $iconUnfinishedTodoList = importIcon["UnfinishedTodoList.svg"]
$iconPlus               := $iconPlus               = importIcon["Plus.svg"]
$iconFileLink           := $iconFileLink           = importIcon["FileLink.svg"]
$iconLinkArea           := $iconLinkArea           = importIcon["LinkArea.svg"]
$iconOpenFolder         := $iconOpenFolder         = importIcon["OpenFolder.svg"]
$iconMessageLink        := $iconMessageLink        = importIcon["MessageLink.svg"]
$iconBrowserLink        := $iconBrowserLink        = importIcon["BrowserLink.svg"]


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
			Or[
				(* Note: Check for the legacy, un-namespaced "TODOCompletedQ" tagging rule. *)
				(* TODO:
					These should only be present in my personal log notebooks; this
				    legacy tagging rule is not part of any shared release of Organizer.
					Once I've cleaned these out of my personal notebooks, this workaround
					should be removed. *)
				TrueQ @ CurrentValue[
					ParentCell@EvaluationCell[],
					{TaggingRules, "TODOCompletedQ"}
				],
				TrueQ @ CurrentValue[
					ParentCell@EvaluationCell[],
					{TaggingRules, "CG:Organizer", "TODOCompletedQ"}
				]
			],
			Function[val, Module[{
				cell
			},
				cell = ParentCell[EvaluationCell[]];

				(* Remove the legacy, un-namespaced "TODOCompletedQ" tagging rule to ensure
				   it isn't ambiguous when compared to the namespaced tagging rule. *)
				(* See TODO above. *)
				SetOptions[cell, TaggingRules -> Replace[
					CurrentValue[cell, TaggingRules],
					{most___, "TODOCompletedQ" -> _, rest___} :> {most, rest}
				]];

				CurrentValue[cell, {TaggingRules, "CG:Organizer", "TODOCompletedQ"}] = val;
			]]
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

InsertTodoAfterSelection[] := Module[{nb},
	nb = SelectedNotebook[];
	SelectionMove[nb, After, Cell];

	writeTodoAndSelect[nb]
]

InsertTodoForToday[nb_NotebookObject] := Module[{
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
InsertTodoAtTopOfQueue[nb_NotebookObject] := Module[{
	queueChapterCell
},
	queueChapterCell = FindQueueChapterCell[nb];

	If[FailureQ[queueChapterCell],
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

findChapterCell[nb_NotebookObject, contents_?StringQ] := Module[{cell},
	cell = SelectFirst[
		Cells[nb, CellStyle -> "Chapter"],
		MatchQ[
			NotebookRead[#],
			Cell[contents, "Chapter", ___]
		] &
	];

	If[!MatchQ[cell, CellObject[___]],
		Return[$Failed];
	];

	cell
]

(**************************************)
(* ::Subsubsection:: *)
(*Links*)
(**************************************)

shortenURLLabel[label_?StringQ] := StringReplace[
  label,
  (* Personal set of URL shortening rules. *)
  "Pull Request #" ~~ content___ ~~ "- Wolfram Stash" :> "PR #" <> content
]

createSystemOpenCell[] := With[{
	filepath = SystemDialogInput["FileOpen", NotebookDirectory[]]
},
	If[filepath === $Canceled,
		Return[$Canceled];
	];

	If[!StringQ[filepath],
		Throw[StringForm["Invalid file path: ``", filepath]];
	];

	Cell[
		BoxData @ ToBoxes @ Framed[
			Button[
				Style[(* label *)FileNameTake[filepath, -1], "Hyperlink", Bold],
				SystemOpen[File[filepath]],
				Appearance -> None
			],
			RoundingRadius -> 5,
			Background -> LightBlue,
			FrameStyle -> Directive[Thick, Darker@Green]
		],
		CellMargins -> {{66, 0}, {0, 1}}
	]
]

insertCellAfterSelection[cell_] := Module[{nb},
	If[FailureQ @ cell || cell === $Canceled,
		Return[$Failed];
	];

	If[!MatchQ[cell, Cell[__]],
		Return[$Failed];
	];

	nb = EvaluationNotebook[];
	(* nb = SelectedNotebook[]; *)
	SelectionMove[nb, After, Cell];
	NotebookWrite[nb, cell];
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

	If[res === $Canceled,
		Return[$Canceled];
	];

	data = Import[path, {{"Hyperlinks", "Plaintext"}}];

	DeleteFile[path];

	hyperlink = Replace[data, {
		{{link_}, label_} :> Hyperlink[Style[shortenURLLabel[label], 12], URL[link]],
		_ :> Throw[StringForm[
			"Dragged link did not have the expected format after Import: ``",
			data
		]]
	}];

	Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"]
]

(**************************************)
(* AppleScripts                       *)
(**************************************)

$getMailLinkScript = "
tell application \"Mail\"
	(* open (get first message of inbox) *)

	set _selectedMsgs to selected messages of message viewer 0

	if (_selectedMsgs is equal to missing value) then
		log \"Missing[\\\"NotAvailable\\\"]\"
		return
	end if

	log \"{\"
	repeat with _theMsg in _selectedMsgs
		set _id to (message id of _theMsg)
		set _subject to (subject of _theMsg)
		set _date to (date received of _theMsg)
		set _sender to (sender of _theMsg)

		log \"<|\"
		log \"\\\"ID\\\" -> \\\"\"           & _id      & \"\\\", \"
		log \"\\\"Subject\\\" -> \\\"\"      & _subject & \"\\\", \"
		log \"\\\"DateReceived\\\" -> \\\"\" & _date    & \"\\\", \"
		log \"\\\"Sender\\\" -> \\\"\"       & _sender  & \"\\\"\"
		log \"|>, \"
	end repeat
	log \"Sequence[]}\"

	(*if (_msgs is not equal to missing value) then
		set _msg to last item of _msgs
		set _msgID to (message id of _msg)

		return _msgID
	end if
	*)
end tell
";

$getSafariLinkScript = "
tell application \"Safari\"
	log \"{\"
	repeat with _theDoc in every document
		set _title to (name of _theDoc)
		set _url to (URL of _theDoc)

		log \"<|\"
		log \"\\\"Title\\\" -> \\\"\" & _title as «class utf8»      & \"\\\", \"
		log \"\\\"URL\\\" -> \\\"\"   & _url  & \"\\\"\"
		log \"|>, \"
	end repeat
	log \"Sequence[]}\"
end tell
"

$getChromeLinkScript = "
if application \"Google Chrome\" is running then
	tell application \"Google Chrome\"
		log \"{\"
		repeat with _window in every window
			set _title to the title of active tab of _window
			set _url to the URL of active tab of _window

			log \"<|\"
			log \"\\\"Title\\\" -> \\\"\" & _title as «class utf8»      & \"\\\", \"
			log \"\\\"URL\\\" -> \\\"\"   & _url  & \"\\\"\"
			log \"|>, \"
		end repeat
		log \"Sequence[]}\"
	end tell
else
	log \"{}\"
end if
"

getAppleMailHyperlink[] := Module[{data, message, url, hyperlink},
	data = RunProcess[{"osascript", "-e", $getMailLinkScript}, "StandardError"];
	If[FailureQ[data],
		Return[data];
	];
	Assert[StringQ[data]];

	data = ToExpression[data];

	If[MissingQ[data],
		Return[data];
	];

	If[!ListQ[data] || data === {},
		Return[$Failed];
	];

	If[Length[data] === 1,
		message = data[[1]];
		url = URL["message://%3C" <> URLEncode[message["ID"]] <> "%3E"];
		hyperlink = Hyperlink[Style[message["Subject"], 12], url];

		Return[ Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"] ]
	];

	(* TODO: Support showing a listing of possible emails to link to when Length[data] > 1 *)
	Return[$Failed];
]

errorDialog[message_?StringQ] := MessageDialog[Row[{
	Style["Error: ", 14, Darker[Red]],
	message
}]]

getOpenPages[script_?StringQ] := Module[{data},
	data = RunProcess[{"osascript", "-e", script}, "StandardError"];

	(* Echo[InputForm[data], "data A"] *)

	If[FailureQ[data],
		errorDialog[ToString@StringForm[
			"Data returned from 'osascript' was FailureQ: ``",
			data
		]];
		Return[data];
	];
	Assert[StringQ[data]];

	If[StringContainsQ[data, "missing value"],
		errorDialog[ToString@StringForm[
			"Data returned from 'osascript' contains missing value: ``",
			data
		]];
		Return[$Failed];
	];

	data = ToExpression[data];

	(* Echo[data, "data B"]; *)

	If[!MatchQ[data, {KeyValuePattern[{"Title" -> _?StringQ, "URL" -> _?StringQ}]...}],
		errorDialog[ToString@StringForm[
			"Data returned from 'osascript' does not have the expected Association form: ``",
			InputForm[data]
		]];
		Return[$Failed];
	];

	Return[data];
]


getBrowserHyperlink[] := Module[{safariData, chromeData, data, pair, hyperlink},
	safariData = getOpenPages[$getSafariLinkScript];
	If[FailureQ[safariData],
		Return[$Failed];
	];

	chromeData = getOpenPages[$getChromeLinkScript];
	If[FailureQ[chromeData],
		Return[$Failed];
	];

	data = Join[safariData, chromeData];

	pair = Which[
		Length[data] === 0,
			errorDialog[ToString@StringForm[
				"Data returned from 'osascript' was an empty list: ``. Perhaps you have no browser windows open?",
				InputForm[data]
			]];
			Return[$Failed]
		,
		Length[data] === 1,
			data[[1]]
		,
		True,
			DialogInput[DialogNotebook[{
				Column[
					Map[
						Function[{pair},
							Button[pair["Title"], DialogReturn[pair]]
						],
						data
					]
				]
			}]]
	];

	If[pair === $Canceled,
		Return[$Failed];
	];

	hyperlink = Hyperlink[
		Style[shortenURLLabel[pair["Title"]], 12],
		pair["URL"]
	];

	Return[ Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"] ]
]

(**************************************)
(* ::Chapter:: *)
(* New Notebook Setup                 *)
(**************************************)

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

InstallLogNotebookDockedCells[nbObj_, projName_?StringQ] := With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	buttonOptions, newTODObutton,
	newTodayTodoButton,
	newTodoAtTopOfQueueButton,
	newFileLinkButton,
	newDraggedLinkButton,
	newMessageLinkButton,
	newBrowserLinkButton,
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
			InsertTodoAfterSelection[];
		),
		buttonBarOptions
	];

	newTodayTodoButton = Button[
		iconButtonContent[$iconCalendarWithPlus, "Insert new TODO item for today"],
		(
			ReleaseHold[loadOrFail];
			InsertTodoForToday[SelectedNotebook[]];
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
			InsertTodoAtTopOfQueue[SelectedNotebook[]];
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
			insertCellAfterSelection[createSystemOpenCell[]];
		),
		buttonBarOptions,
		Method -> "Queued"
	];

	newMessageLinkButton = Button[
		iconButtonContent[
			$iconMessageLink,
			"Insert a link to the selected Apple Mail message"
		],
		(
			ReleaseHold[loadOrFail];
			insertCellAfterSelection[getAppleMailHyperlink[]];
		),
		buttonBarOptions,
		Method -> "Queued"
	];

	newBrowserLinkButton = Button[
		iconButtonContent[
			$iconBrowserLink,
			"Insert a link to a web page open in Safari or Google Chrome"
		],
		(
			ReleaseHold[loadOrFail];
			insertCellAfterSelection[getBrowserHyperlink[]];
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
			insertCellAfterSelection[getDraggedHyperlink[]];
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
			newMessageLinkButton,
			newBrowserLinkButton,
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

InstallLogNotebookStyles[nb_NotebookObject] := With[{
	todoDefinitions = Sequence[
		TaggingRules -> {"CG:Organizer" -> {"TODOCompletedQ" -> False}},
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
},
	SetOptions[nb,
		StyleDefinitions -> Notebook[{
			Cell[StyleData[StyleDefinitions -> "Default.nb"] ],
			Cell[StyleData["TODO", StyleDefinitions -> StyleData["Text"] ],
				todoDefinitions,
				"ReturnCreatesNewCell" -> True,
				(* If the user presses the Tab key or the '*', convert this cell to a
				   "TODO:Item" cell. *)
				StyleKeyMapping -> {"Tab" -> "TODO:Item", "*" -> "TODO:Item"}
			],
			Cell[StyleData["TODO:Item", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Item" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.8, 0.043, 0.008]
				],
				CellMargins -> {{81, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Tab" -> "TODO:Subitem", "*" -> "TODO:Subitem", "Backspace" -> "TODO"},

				todoDefinitions
			],
			Cell[StyleData["TODO:Subitem", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Subitem" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.8, 0.043, 0.008]
				],
				CellMargins -> {{105, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Tab" -> "TODO:Subsubitem", "*" -> "TODO:Subsubitem", "Backspace" -> "TODO:Item"},

				todoDefinitions
			],
			Cell[StyleData["TODO:Subsubitem", StyleDefinitions -> StyleData["Text"] ],
				(* Override the default TODO cell frame. The gray bar looks weird as part
				   of an item. *)
				CellFrame -> {{0, 0}, {0, 0}},

				(* The below rules are taken from the Default.nb stylesheet notebook
				   definition for an "Subsubitem" cell. *)
				CellDingbat -> StyleBox[
					"\[FilledSmallSquare]",
					Alignment -> Baseline,
					RGBColor[0.6, 0.6, 0.6]
				],
				CellMargins -> {{129, 0}, {2, 2}},
				"ReturnCreatesNewCell" -> True,

				StyleKeyMapping -> {"Backspace" -> "TODO:Subitem"},

				todoDefinitions
			]
		}]
	];
]


End[]

EndPackage[]
