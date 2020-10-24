# Personal Organizer System

This paclet provides a palette and notebook organizer system.

## First-time setup

Begin by cloning this paclet to your local computer. Next, use `PacletInstall` to
install it to your system.

```wolfram
PacletInstall["/path/to/Organizer"];
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
> PersistentValue["CG:Organizer:Directory", "Local"] = SystemDialogInput["Directory"];
> ```