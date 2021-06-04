#!/usr/bin/env wolframscript

If[$VersionNumber < 12.2,
	Print["ERROR: Script requires WL 12.2+"];
	Exit[-1]
];

$pacletRoot = FileNameDrop[$InputFileName, -2];

Echo[$pacletRoot]

$SVGs = FileNames["*.svg" ~~ EndOfString, FileNameJoin[{$pacletRoot, "resources/icons/"}]];

result = Enclose[
	Map[
		file |-> Module[{outFile, graphics},
			outFile = FileNameJoin[{
				$pacletRoot,
				"Icons",
				FileNameTake[file, -1] <> ".wl"
			}];
			graphics = Confirm[
				Confirm[ResourceFunction @ "SVGImport"][file]
			];
			If[Head[graphics] =!= Graphics3D,
				Confirm[$Failed];
			];
			Put[graphics, outFile];
		],
		$SVGs
	]
];

If[!ListQ[result],
	Print[ToString @ StringForm["FAILURE: ``", InputForm[result]]];
	Exit[-1];
]
