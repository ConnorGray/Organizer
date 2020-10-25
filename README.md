# Personal Organizer System

This paclet provides a palette and notebook organizer system.

## First-time setup

Begin by cloning this paclet to your local computer. Next, use `CreatePacletArchive` and
`PacletInstall` to install it to your system.

```wolfram
CreatePacletArchive["/path/to/Organizer"];

PacletInstall["/path/to/Organizer-0.1.0.paclet"];
```

Next, evaluate `` Needs["Organizer`"] ``. If this is your first time setting up the system,
a file selection dialog will appear asking you to select a directory.

```wolfram
Needs["Organizer`];
```

> If you've previously set-up the organizer system, but would like to change the root
> directory the system uses, you do so by evaluating:
>
> ```wolfram
> PersistentValue["CG:Organizer:RootDirectory", "Local"] = SystemDialogInput["Directory"];
> ```

Once you've chosen a root directory, you may open the palette by evaluating:

```wolfram
CreateOrganizerPalette[]
```

This should open a new empty palette window. New projects may be added by clicking the
"New Project" button and entering a name in the dialog window.

### Credits

The icons [CalendarWithPlus.svg](./resources/icons/CalendarWithPlus.svg) and
[UnfinishedTodoList.svg](./resources/icons/UnfinishedTodoList.svg) are from
[www.onlinewebfonts.com/icon][http://www.onlinewebfonts.com/icon], licensed by
CC BY 3.0.