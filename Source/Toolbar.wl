BeginPackage["ConnorGray`Organizer`Toolbar`"]

MakeLinkButtonRow
MakeColorPickerButtonGrid

MakeToolbarButtonBoxes
MakeToolbarDropdownBoxes
IconButtonContent

GetIcon
LoadIcons


Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Utils`"]


(*========================================================*)

(*------------------------------------*)

MakeLinkButtonRow[] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	linkButtonAccentColor = RGBColor["#3da5d9"],
	newFileLinkButton,
	newMessageLinkButton,
	newBrowserLinkButton,
	newDraggedLinkButton
},
	newFileLinkButton = MakeToolbarButtonBoxes[
		GetIcon["FileLink"],
		"File",
		"Insert a link to a file chosen from the file system",
		Function[
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ createSystemOpenCell[]];
		],
		"Queued",
		White,
		linkButtonAccentColor
	];

	newMessageLinkButton = MakeToolbarButtonBoxes[
		GetIcon["MessageLink"],
		"Email",
		"Insert a link to the selected Apple Mail message",
		Function[
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ getAppleMailHyperlink[]];
		],
		Automatic,
		White,
		linkButtonAccentColor
	];

	newBrowserLinkButton = MakeToolbarButtonBoxes[
		GetIcon["BrowserLink"],
		"Web Page",
		"Insert a link to a web page open in Safari or Google Chrome",
		Function @ (
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ getBrowserHyperlink[]];
		),
		"Queued",
		White,
		linkButtonAccentColor
	];

	(* FIXME: Support dragged links in some other way?
		E.g. have a button within the popup that shows the browser links. That
		button should appear whether or not the automatic link assembly suceeds.
	newDraggedLinkButton = Button[
		IconButtonContent[
			GetIcon["LinkArea"],
			"Insert a link dragged into a popup TextEdit window"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ getDraggedHyperlink[]];
		),
		$ButtonBarOptions,
		Background -> background,
		Method -> "Queued"
	];
	*)

	{
		newBrowserLinkButton,
		newFileLinkButton,
		newMessageLinkButton
	}
]]

(*====================================*)

MakeToolbarButtonBoxes[
	icon_Graphics,
	label_?StringQ,
	tooltip_?StringQ,
	buttonFunction_Function,
	buttonMethod0 : _?StringQ | Automatic : Automatic,
	buttonDefaultBackground : _ : White,
	buttonAccentColor : _ : RGBColor["#60993e"]
] := Module[{
	buttonMethod = Replace[buttonMethod0, Automatic -> "Preemptive"]
},
	TemplateBox[
		{
			ToBoxes @ Show[icon, ImageSize -> 15, BaselinePosition -> Center],
			label,
			tooltip,
			buttonFunction,
			buttonMethod,
			buttonDefaultBackground,
			buttonAccentColor
		},
		"Organizer:IconAndLabelButtonTemplate"
	]
]

(*====================================*)

MakeToolbarDropdownBoxes[
	icon_Graphics,
	label_?StringQ,
	tooltip_?StringQ,
	choiceActions_?AssociationQ,
	buttonMethod0 : _?StringQ | Automatic : Automatic,
	buttonDefaultBackground : _ : White,
	buttonAccentColor : _ : RGBColor["#60993e"]
] := Module[{
	buttonMethod = Replace[buttonMethod0, Automatic -> "Preemptive"]
},
	TemplateBox[
		{
			ToBoxes @ Show[icon, ImageSize -> 15, BaselinePosition -> Center],
			label,
			tooltip,
			Normal[choiceActions],
			buttonMethod,
			buttonDefaultBackground,
			buttonAccentColor
		},
		"Organizer:IconAndLabelDropdownTemplate"
	]
]

(*====================================*)
(* Link Button Handlers               *)
(*====================================*)

createSystemOpenCell[] := Try @ With[{
	filepath = SystemDialogInput[
		"FileOpen",
		(* This can fail if the notebook has not been saved yet. *)
		Confirm @ NotebookDirectory[]
	]
},
	If[filepath === $Canceled,
		Return[$Canceled];
	];

	If[!StringQ[filepath],
		Confirm @ FailureMessage[
			Organizer::error,
			"Invalid file path: ``",
			{filepath}
		];
	];

	With[{
		(* Note:
			Make links to .nb files open with NotebookOpen instead of
			SystemOpen. Use SystemOpen for all other types of files.

			Using NotebookOpen is significant because of a subtle behavior of
			SystemOpen: If two different versions of Mathematica are running
			(e.g. a released version and a nightly build), and
			SystemOpen[".../SomeNotebook.nb"] is executed in the older version,
			the notebook will be opened in the newer version of Mathematica,
			which is unexpected.
		*)
		openerFunction = Which[
			FileExtension[filepath] === "nb",
				NotebookOpen,
			True,
				SystemOpen
		]
	},
		Cell[
			BoxData @ ToBoxes @ Framed[
				Button[
					Style[(* label *)FileNameTake[filepath, -1], "Hyperlink", Bold],
					openerFunction[File[filepath]],
					Appearance -> None
				],
				RoundingRadius -> 5,
				Background -> LightBlue,
				FrameStyle -> Directive[Thick, Darker@Green]
			],
			CellMargins -> {{66, 0}, {0, 1}}
		]
	]
]

getDraggedHyperlink[] := Try @ Module[{path, res, data, hyperlink},
	(* TODO: Make path cross-platform. *)
	If[$OperatingSystem =!= "MacOSX",
		Confirm @ FailureMessage[
			Organizer::error,
			"Cannot get dragged link on non-MacOSX platforms."
		];
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
		_ :> Confirm @ FailureMessage[
			Organizer::error,
			"Dragged link did not have the expected format after Import: ``",
			{InputForm[data]}
		]
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
	repeat with _theDoc in every document
		set _title to (name of _theDoc)
		set _url to (URL of _theDoc)

		log \"Title: \" & _title as «class utf8»
		log \"URL: \" & _url
	end repeat
end tell
"

$getChromeLinkScript = "
if application \"Google Chrome\" is running then
	tell application \"Google Chrome\"
		repeat with _window in every window
			set _title to the title of active tab of _window
			set _url to the URL of active tab of _window

			log \"Title: \" & _title as «class utf8»
			log \"URL: \" & _url
		end repeat
	end tell
else
	log \"{}\"
end if
"

getAppleMailHyperlink[] := Module[{data, message, url, box},
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
		url = "message://%3C" <> URLEncode[message["ID"]] <> "%3E";

		box = TemplateBox[
			{
				ToBoxes @ message["Subject"],
				url
			},
			"Organizer:EmailLinkTemplate"
		];

		Return[ Cell[BoxData[box], "Subitem"] ]
	];

	(* TODO: Support showing a listing of possible emails to link to when Length[data] > 1 *)
	Return[$Failed];
]

getOpenPages[script_?StringQ] := Try @ Module[{data},
	data = RunProcess[{"osascript", "-e", script}, "StandardError"];

	If[FailureQ[data],
		Confirm @ FailureMessage[
			Organizer::error,
			"Invocation of 'osascript' failed: ``",
			{InputForm[data]}
		];
	];
	Assert[StringQ[data]];

	If[StringContainsQ[data, "missing value"],
		Confirm @ FailureMessage[
			Organizer::error,
			"Data returned from 'osascript' contains missing value: ``",
			{InputForm[data]}
		];
	];

	data = StringSplit[data, "\n"];

	data = Partition[data, 2];

	data = Map[
		Replace[{
			{
				title_?StringQ /; StringStartsQ[title, "Title: "],
				url_?StringQ /; StringStartsQ[url, "URL: "]
			} :> <|
				"Title" -> StringDrop[title, 7],
				"URL" -> StringDrop[url, 5]
			|>,
			other_ :> Confirm @ FailureMessage[
				Organizer::error,
				"Data returned from 'osascript' does not have the expected form:",
				{InputForm[data]}
			]
		}],
		data
	];

	(* Echo[data, "data B"]; *)

	If[!MatchQ[data, {KeyValuePattern[{"Title" -> _?StringQ, "URL" -> _?StringQ}]...}],
		Confirm @ FailureMessage[
			Organizer::error,
			"Data returned from 'osascript' does not have the expected Association form: ``",
			{InputForm[data]}
		];
	];

	Return[data];
]

getBrowserHyperlink[] := Try @ Module[{safariData, chromeData, data, pair, hyperlink},
	safariData = Confirm @ getOpenPages[$getSafariLinkScript];
	chromeData = Confirm @ getOpenPages[$getChromeLinkScript];

	data = Join[safariData, chromeData];

	pair = Which[
		Length[data] === 0,
			Confirm @ FailureMessage[
				Organizer::error,
				"Data returned from 'osascript' was an empty list: ``. Perhaps you have no browser windows open?",
				{InputForm[data]}
			];
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
		Return[$Canceled];
	];

	hyperlink = Hyperlink[
		Style[shortenURLLabel[pair["Title"]], 12],
		pair["URL"]
	];

	Return[ Cell[BoxData @ ToBoxes @ hyperlink, "Subitem"] ]
]

(*====================================*)
(* Link Button Utilities              *)
(*====================================*)

shortenURLLabel[label_?StringQ] := StringReplace[
  label,
  (* Personal set of URL shortening rules. *)
  "Pull Request #" ~~ content___ ~~ "- Wolfram Stash" :> "PR #" <> content
]

(*====================================*)
(* Cell Background Color Picker       *)
(*====================================*)

setSelectedCellsBackground[color_] := Module[{selectedCells},
	selectedCells = SelectedCells[];
	Assert[MatchQ[selectedCells, {___CellObject}]];
	Map[SetOptions[#, Background -> color] &, selectedCells]
]

MakeColorPickerButtonGrid[] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	colors,
	buttonGroups
},
	colors = PersistentValue["CG:Organizer:BackgroundColorPalette", "Local"];

	If[MissingQ[colors],
		Confirm @ FailureMessage[
			Organizer::error,
			"No \"CG:Organizer:BackgroundColorPalette\" PersistentValue is set."
		];
	];

	If[!MatchQ[colors, {{___RGBColor}..}],
		Confirm @ FailureMessage[
			Organizer::error,
			"\"CG:Organizer:BackgroundColorPalette\" PersistentValue is not a valid array of colors."
		];
	];

	buttonGroups = Map[
		Button[
			"",
			(
				ReleaseHold[loadOrFail];
				setSelectedCellsBackground[#];
			),
			ImageSize -> {20, 20},
			Background -> #,
			ImageMargins -> 0,
			ContentPadding -> None,
			Appearance -> None
		] &,
		colors,
		{2}
	];

	dividers = FoldList[Plus, 1, Length /@ buttonGroups];
	(* dividers = Part[dividers, 2 ;; -2]; *)

	Grid[
		{Flatten[buttonGroups]},
		Spacings -> {0.0, .0},
		ItemSize -> All,
		Frame -> True,
		FrameStyle -> Thickness[1],
		Dividers -> {
			{
				(* Use light colored dividers by default. *)
				Directive[GrayLevel[0.7]],
				(* Use dark dividers to separate sections. *)
				Map[
					index |-> (index -> Black),
					dividers
				]
			},
			{}
		}
	]
]
]


(*========================================================*)
(* Icons                                                  *)
(*========================================================*)

$loadedIcons = <||>

LoadIcons[] := (
	GetIcon["CalendarWithPlus"];
	GetIcon["UnfinishedTodoList"];
	GetIcon["Plus"];
	GetIcon["FileLink"];
	GetIcon["LinkArea"];
	GetIcon["OpenFolder"];
	GetIcon["MessageLink"];
	GetIcon["BrowserLink"]
)

GetIcon[name_?StringQ] := Try @ Module[{
	icon
},
	(* Check if we've already loaded the icon, and if so, return the cached value. *)
	If[KeyExistsQ[$loadedIcons, name],
		Return[$loadedIcons[name], Module];
	];

	icon = Confirm @ importIcon[name <> ".svg"];

	(* Cache the loaded icon data. *)
	$loadedIcons[name] = icon;

	icon
]

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

importIcon[filename_?StringQ] := Try @ Get[
	FileNameJoin[{
		Confirm[Confirm[PacletObject["ConnorGray/Organizer"]]["AssetLocation", "Icons"]],
		filename <> ".wl"
	}]
]

(*====================================*)
(* Utilities                          *)
(*====================================*)

IconButtonContent[icon_, tooltip_?StringQ] := Tooltip[
	Show[
		icon,
		ImageSize -> 20
	],
	tooltip,
	TooltipDelay -> 0.333
]



End[]

EndPackage[]