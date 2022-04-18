BeginPackage["ConnorGray`Organizer`Toolbar`"]

MakeLinkButtonRow

IconButtonContent

$ButtonBarOptions

GetIcon
LoadIcons


Begin["`Private`"]

Needs["ConnorGray`Organizer`"]
Needs["ConnorGray`Organizer`Utils`"]

(* TODO:
	Move this function out of LogNotebookRuntime.wl and into a more general cell
    manipulation utilities package. *)
InsertCellAfterSelection = ConnorGray`Organizer`LogNotebookRuntime`insertCellAfterSelection;


(*========================================================*)

$ButtonBarOptions = Sequence[
	Background -> Blend[{Darker@Orange,Red}],
	ContentPadding -> None,
	FrameMargins -> 7
];

(*------------------------------------*)

MakeLinkButtonRow[] := Try @ With[{
	loadOrFail = $HeldLoadOrFail
},
Module[{
	newFileLinkButton,
	newMessageLinkButton,
	newBrowserLinkButton,
	newDraggedLinkButton
},
	newFileLinkButton = Button[
		IconButtonContent[
			GetIcon["FileLink"],
			"Insert a link to a file chosen from the file system"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ createSystemOpenCell[]];
		),
		$ButtonBarOptions,
		Method -> "Queued"
	];

	newMessageLinkButton = Button[
		IconButtonContent[
			GetIcon["MessageLink"],
			"Insert a link to the selected Apple Mail message"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ getAppleMailHyperlink[]];
		),
		$ButtonBarOptions,
		Method -> "Queued"
	];

	newBrowserLinkButton = Button[
		IconButtonContent[
			GetIcon["BrowserLink"],
			"Insert a link to a web page open in Safari or Google Chrome"
		],
		(
			ReleaseHold[loadOrFail];
			HandleUIFailure @ InsertCellAfterSelection[HandleUIFailure @ getBrowserHyperlink[]];
		),
		$ButtonBarOptions,
		Method -> "Queued"
	];

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
		Method -> "Queued"
	];

	Row[
		{
			newFileLinkButton,
			newMessageLinkButton,
			newBrowserLinkButton,
			newDraggedLinkButton
		},
		ImageMargins -> 10
	]
]]


(*====================================*)
(* Link Button Handlers               *)
(*====================================*)

createSystemOpenCell[] := Try @ With[{
	filepath = SystemDialogInput["FileOpen", NotebookDirectory[]]
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

getDraggedHyperlink[] := Try @ Module[{path, res, data, hyperlink},
	(* TODO: Make path cross-platform. *)
	If[$SystemID =!= "MacOSX-x86-64",
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

getOpenPages[script_?StringQ] := Try @ Module[{data},
	data = RunProcess[{"osascript", "-e", script}, "StandardError"];

	(* Echo[InputForm[data], "data A"] *)

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

	data = ToExpression[data];

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