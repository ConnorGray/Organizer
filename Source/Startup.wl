Begin["ConnorGray`Organizer`Startup`"]

(* Ensure that e.g. CellInsertionMenu is installed as early as possible, because
   a user may open a notebook with the Organizer stylesheet (which depends on
   CellInsertionMenu) before they themselves ever actually load the Organizer
   package code. *)
Once[
	PacletInstall /@ PacletObject["ConnorGray/Organizer"]["Dependencies"],
	"FrontEndSession"
]

(* NOTE: These menus can be removed by evaluating:
	FrontEndExecute @ (FrontEnd`RemoveMenuCommands @@ $OrganizerAddedMenus)
*)
$OrganizerAddedMenus = FrontEnd`AddMenuCommands["NewText", {
	Menu["Organizer Notebook", {
		(* TODO:
			Make this equivalent to the 'New Project' button, by automatically
		    saving the created Log notebook into the current selected
			workspace/category? Alternatively, make the dialog that asks for the
			title also ask for workspace/category location. *)
		(* MenuItem[
			"Log Notebook",
			FrontEnd`KernelExecute[(
				Needs["ConnorGray`Organizer`" -> None];
				ConnorGray`Organizer`CreateOrganizerNotebook["Log"]
			)],
			FrontEnd`MenuEvaluator -> Automatic,
			Method -> "Queued"
		], *)
		MenuItem[
			"Design Notebook",
			FrontEnd`KernelExecute[(
				Needs["ConnorGray`Organizer`" -> None];
				ConnorGray`Organizer`CreateOrganizerNotebook["Design"]
			)],
			FrontEnd`MenuEvaluator -> Automatic,
			Method -> "Queued"
		],
		MenuItem[
			"Tasklist Notebook",
			FrontEnd`KernelExecute[(
				Needs["ConnorGray`Organizer`" -> None];
				ConnorGray`Organizer`CreateOrganizerNotebook["Tasklist"]
			)],
			FrontEnd`MenuEvaluator -> Automatic,
			Method -> "Queued"
		]
	}]
}];

Once[
	FrontEndExecute[{
		$OrganizerAddedMenus
	}],
	"FrontEndSession"
]

End[]