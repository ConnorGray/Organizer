BeginPackage["ConnorGray`Organizer`Reports`"]

Begin["`Private`"]

Needs["ConnorGray`Organizer`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Palette`"] (* For CreateQueuesReport *)
Needs["ConnorGray`Organizer`Errors`"]
Needs["ConnorGray`Organizer`Notebook`"]
Needs["ConnorGray`Organizer`Notebook`Log`"]

(*====================================*)
(* Create Queue's Report              *)
(*====================================*)

(* Create and open a new NB which contains the Queue's NB section for every active project
   in the current workspace. *)
CreateQueuesReport[] := Module[{
	projects,
	settings,
	cells = {},
	nb, path,
	timestamp,
	workspaceName
},
	(*--------------------------------------------------------------------*)
	(* Present the user with a DialogInput to select the projects to view *)
	(*--------------------------------------------------------------------*)

	projects = RaiseConfirm @ Projects[];

	RaiseConfirmMatch[projects, {___?StringQ}];

	settings = DialogInput[{
		selectedProjects = projects
	},
		Column[{
			Panel[
				Column[{
					CheckboxBar[
						Dynamic[selectedProjects],
						projects,
						Appearance -> "Vertical"
					]
				}],
				"Queue's Report Settings"
			],
			DefaultButton["Generate",
				DialogReturn[<|"Projects" -> selectedProjects|>]
			]
		}]
	];

	projects = Replace[settings, {
		$Canceled :> Return[Null, Module],
		_?AssociationQ :> settings["Projects"],
		_ :> (
			Raise[
				OrganizerError,
				"Unexpected value returned from settings panel: ``",
				InputForm[settings]
			];
		)
	}];

	(*-----------------------*)
	(* Populate the notebook *)
	(*-----------------------*)

	workspaceName = FileNameTake[RaiseConfirm @ WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day",
		" at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"
	}];

	AppendTo[cells, Cell["All Queues: " <> workspaceName, "Title"]];
	AppendTo[
		cells,
		Cell[
			"Created " <> timestamp,
			"Subtitle"
		]
	];

	cells = Join[cells, Map[
		Function[proj, Module[{
			queueChapterGroup
		},
			path = FileNameJoin[{
				RaiseConfirm @ CategoryDirectory[],
				proj,
				"Log.nb"
			}];

			queueChapterGroup = FirstCase[
				RaiseConfirmMatch[Get[path], _Notebook],
				Cell @ CellGroupData[
					{
						Cell["Queue", "Chapter", chapterOpts___],
						groupCellsRest___
					},
					_
				] :> (
					Cell @ CellGroupData[
						{
							Cell[
								(* Include the project name in the Queue chapter
								   name to distinguish it from the Queue chapter
								   of all the other included projects. *)
								"Queue — " <> proj,
								"Chapter",
								chapterOpts,
								WholeCellGroupOpener -> True
							],
							groupCellsRest
						},
						(* Close the cell group by default so the user can see
						   all the chapter in the report at once initially. *)
						Closed
					]
				),
				(* TODO: Issue a warning when a Log notebook doesn't contain
					a Queue chapter? *)
				Nothing,
				Infinity
			];

			queueChapterGroup
		]],
		projects
	]];

	nb = NotebookPut[
		Notebook[cells],
		(* Disable editing. If the user wants to edit these queues, they should do it in
		the source notebook. *)
		Editable -> False,
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Style["All Queues: " <> workspaceName <> ": " <> timestamp],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		},
		(* Add style definitions so that copied TODO cells render properly. *)
		StyleDefinitions -> $OrganizerStylesheet
	];

	RaiseConfirm @ SetNotebookTaggingRules[nb, "GeneratedQueueReport"];

	nb
]

(*====================================*)
(* Create Daily's Report              *)
(*====================================*)

CreateDailysReport[] := Handle[_Failure] @ Module[{
	settings,
	timePeriod,
	startDate,
	endDate,
	nb,
	workspaceName,
	timestamp,
	projects,
	cells
},
	(*---------------------------------------------------------*)
	(* Ask the user for the date range and projects to include *)
	(*---------------------------------------------------------*)

	projects = RaiseConfirm @ Projects[];

	RaiseConfirmMatch[projects, {___?StringQ}];

	settings = DialogInput[{
		timePeriod,
		selectedProjects = projects
	},
		Column[{
			Panel[
				Column[{
					CheckboxBar[
						Dynamic[selectedProjects],
						projects,
						Appearance -> "Vertical"
					],
					PopupMenu[
						Dynamic[timePeriod],
						{"This Week", "Last Week", "Yesterday", "Past 7 Days", "Past 14 Days", "Past 30 Days"}
					]
				}],
				"Daily's Report Settings"
			],
			DefaultButton["Generate",
				DialogReturn[<|
					"TimePeriod" -> timePeriod,
					"Projects" -> selectedProjects
				|>]
			]
		}]
	];

	RaiseAssert[AssociationQ[settings]];

	timePeriod = RaiseConfirm @ Lookup[settings, "TimePeriod", $Failed];
	projects = RaiseConfirm @ Lookup[settings, "Projects", $Failed];

	ConfirmReplace[timePeriod, {
		"Past 7 Days" :> (
			startDate = Today - Quantity[7, "Days"];
			endDate = Today;
		),
		"Past 14 Days" :> (
			startDate = Today - Quantity[14, "Days"];
			endDate = Today;
		),
		"Past 30 Days" :> (
			startDate = Today - Quantity[30, "Days"];
			endDate = Today;
		),
		"Last Week" :> Module[{daysOfLastWeek},
			daysOfLastWeek = Map[
				(* Subtract one day to get a Sunday-Sunday week instead of Monday-Monday. *)
				DatePlus[#, {-1, "Day"}] &,
				DayRange[
					DateValue[Today, "Week", DateObject] - Quantity[1, "Week"],
					DateValue[Today, "Week", DateObject]
				]
			];

			startDate = First[daysOfLastWeek];
			endDate = Last[daysOfLastWeek];

			Assert[startDate["DayName"] === Sunday];
			Assert[endDate["DayName"] === Sunday];
		],
		"This Week" :> Module[{daysOfThisWeek},
			(* Get the days of this week between the first Monday of the week and today. *)
			daysOfThisWeek = DayRange[
				DateValue[Today, "Week", DateObject],
				Today
			];

			startDate = First[daysOfThisWeek];
			endDate = Last[daysOfThisWeek];

			Assert[startDate["DayName"] === Monday];
		],
		"Yesterday" :> (
			startDate = Today - Quantity[1, "Day"];
			endDate = startDate;
		)
	}];

	Assert[DateObjectQ[startDate] && DateObjectQ[endDate]];
	Assert[startDate["Granularity"] === "Day" && endDate["Granularity"] === "Day"];

	(*---------------------------------*)
	(* Generate the All Daily's report *)
	(*---------------------------------*)

	nb = CreateNotebook[];

	workspaceName = FileNameTake[RaiseConfirm @ WorkspaceDirectory[], -1];

	timestamp = DateString[Now, {
		"DayName", " ", "MonthName", " ", "Day", ", ", "Year",
		" at ", "Hour12Short", ":", "Minute", "AMPMLowerCase"
	}];

	NotebookWrite[nb, Cell["All Daily's: " <> workspaceName, "Title"]];
	NotebookWrite[
		nb,
		Cell["Generated " <> timestamp, "Subtitle"]
	];
	NotebookWrite[
		nb,
		Cell[
			"Showing " <> TextString[startDate] <> " — " <> TextString[endDate] <> " (" <> timePeriod <> ")",
			"Subsubtitle"
		]
	];

	(* Add style definitions so that copied TODO cells render properly. *)
	InstallLogNotebookStyles[nb];

	SetOptions[nb,
		(* Disable editing. If the user wants to edit these queues, they should do it in
		the source notebook. *)
		Editable -> False,
		(* Add a temporary docked cell warning the user that the notebook is still having
		content copied into it. This is removed later. *)
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Row[{
					Style["Generating: ", Italic, GrayLevel[0.2]],
					Style["All Daily's: " <> workspaceName <> ": " <> timestamp]
				}],
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> Lighter[Orange]
			]
		}
	];

	RaiseConfirm @ SetNotebookTaggingRules[nb, "GeneratedDailysReport"];

	Scan[
		Function[proj,
			path = FileNameJoin[{
				RaiseConfirm @ CategoryDirectory[],
				proj,
				"Log.nb"
			}];

			cells = CellsFromChapterInNB[path, "Daily"];

			cells = WrapRaised[OrganizerError, "Error processing '``' project", proj][
				filterDailyCellsByInterval[
					cells,
					DateInterval[{startDate, endDate}]
				]
			];

			cells = Replace[
				cells,
				{Cell["Daily", "Chapter", props___], rest___} :> {Cell["Daily — " <> proj, "Chapter", props], rest}
			];

			RaiseConfirmMatch[cells, {___Cell}];

			NotebookWrite[nb, cells];

			(* NotebookWrite[nb, cells]; *)
		],
		projects
	];

	(* Remove the warning docked cell -- the notebook is now complete. *)
	SetOptions[nb,
		(* Replace the temporary docked cell with a permanent one. *)
		DockedCells -> {
			Cell[
				BoxData @ ToBoxes @ Row[{
					Style["All Daily's: " <> workspaceName <> ": " <> timestamp],
					(* This notebook is not editable by default, to prevent confusion of
					the generated content with the source content. Clicking this button
					makes it possible to opt-in to editing, e.g. for the purpose of
					editing or rearranging the Daily's report before sharing it. *)
					Button[
						"Make Editable",
						(
							SetOptions[EvaluationNotebook[], Editable -> True];
						)
					]
				}],
				"Text",
				FontSize -> 14,
				FontColor -> GrayLevel[0.2],
				Background -> LightBlue
			]
		}
	];

	SelectionMove[First[Cells[nb]], Before, Cell, AutoScroll -> True];
]

filterDailyCellsByInterval[
	cells0 : {___Cell},
	interval_DateInterval
] := Module[{
	cells = cells0,
	currentDate = None,
	updateCurrentDate
},
	(* Confirm that the granularity of the interval is Day. *)
	If[interval["Granularity"] =!= "Day",
		Raise[
			OrganizerError,
			"Granularity of interval `` is not \"Day\"",
			ToString[InputForm[interval]]
		];
	];

	(*----------------------------------------------*)
	(* updateSubsubsectionDate[] helper definitions *)
	(*----------------------------------------------*)

	updateCurrentDateFromSubsubsection[date0_?StringQ] := Module[{
		date = date0
	},
		If[!DateObjectQ[currentDate] || !MemberQ[{"Month", "Day"}, currentDate["Granularity"]],
			Raise[
				OrganizerError,
				"Unknown current year when filtering Daily cells by date: ``",
				InputForm[currentDate]
			];
		];

		(* TODO: Generalize these transformation rules as a user setting. *)
		date = FixedPoint[
			StringReplace[{
				StartOfString ~~ "[X] " ~~ rest___ ~~ EndOfString :> rest,
				StartOfString ~~ most___ ~~ "\[LongDash]" ~~ rest___ ~~ EndOfString :> most,
				StartOfString ~~ most___ ~~ Repeated[DigitCharacter, {4}] ~~ EndOfString :> most,
				WordBoundary ~~ day : Repeated[DigitCharacter, {1, 2}] ~~ ("st" | "nd" | "rd" | "th") ~~ WordBoundary :> day
			}],
			date
		];

		(* Ensure the date object is interpreted in the correct year. *)
		date = date <> " " <> ToString[currentDate["Year"]];

		currentDate = DateObject[
			{date, {"DayName", "MonthName", "Day", "Year"}},
			"Day"
		];
		If[!DateObjectQ[currentDate],
			Raise[
				OrganizerError,
				"Cannot interpret Subsubsection as date: ``",
				InputForm[date]
			];
		];
	];

	(*-------------------------*)
	(* Main Select filter loop *)
	(*-------------------------*)

	(* TODO:
		Optimize which cells are parsed using DateObject, by doing a crude
		replacement to limit by the "<Month> <Year>" Subsection cells. This is on a
		hunch that DateObject is probably fairly slow.
	*)

	(* Select all cells which describe a date within `interval`. *)

	(* NOTE:
		The code below handling inclusion of Section, Subsection, and Subsubsection cells
		uses DateOverlapsQ instead of IntervalMemberQ to work around the fact that
		IntervalMemberQ will return False if the granularity of the DateObject is larger
		than the granularity of the interval itself. E.g. consider:

			interval = DateInterval[{"January 1st, 2021", "January 7th, 2021"}];
			currentDate = DateObject["January 2021", "Month"];

			IntervalMemberQ[interval, currentDate]
				=> False

			DateOverlapsQ[interval, currentDate]
				=> True

		Using IntervalMemberQ would cause Section/Subsection cells with the year and
		month/year to be incorrectly stripped out by the Select.
	*)
	cells = Select[
		cells,
		ConfirmReplace[#, {
			Cell["Daily", "Chapter", ___] -> True,
			Cell[year_?StringQ, "Section", ___] :> (
				DateOverlapsQ[interval, DateObject[{year, {"Year"}}, "Year"]]
			),
			Cell[monthYear_?StringQ, "Subsection", ___] :> (
				currentDate = DateObject[{monthYear, {"Month", "Year"}}, "Month"];
				If[!DateObjectQ[currentDate],
					Raise[
						OrganizerError,
						"Cannot interpret Subsection as date: ``",
						InputForm[monthYear]
					];
				];

				DateOverlapsQ[interval, currentDate]
			),
			Cell[dayNameMonth_?StringQ, "Subsubsection", ___] :> (
				updateCurrentDateFromSubsubsection[dayNameMonth];

				DateOverlapsQ[interval, currentDate]
			),
			Cell[dateTextData_TextData, "Subsubsection", ___] :> Module[{
				date
			},
				date = ReplaceRepeated[dateTextData, {
					TextData[{strings___?StringQ}] :> StringJoin[strings],
					StyleBox[string_?StringQ, ___] :> string
				}];

				If[!StringQ[date],
					Raise[
						OrganizerError,
						"Content of Subsubsection does not have the expected format: ``",
						ToString[InputForm[date]]
					];
				];

				updateCurrentDateFromSubsubsection[date];

				Assert[DateObjectQ[currentDate] && currentDate["Granularity"] === "Day"];

				DateOverlapsQ[interval, currentDate]
			],
			Cell[date_, "Subsubsection", ___] :> Raise[
				OrganizerError,
				"Cannot interpret Subsubsection with non-String contents as date: ``",
				InputForm[date]
			],
			(* Handle all the other cells types, e.g. TODO, Text, Input, Item, etc. Use
			the `currentDate` value computed by previous handling of
			Section/Subsection/Subsubsection headers which described the date. *)
			Cell[___] :> (
				If[!DateObjectQ[currentDate] || currentDate["Granularity"] =!= "Day",
					Raise[
						OrganizerError,
						"Unknown current date when filtering Daily cells by date: ``: ``",
						InputForm[currentDate],
						#
					];
				];
				IntervalMemberQ[interval, currentDate]
			)
		}] &
	];

	cells
]



End[]

EndPackage[]