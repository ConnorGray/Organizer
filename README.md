# Personal Organizer System

This repository contains the source code for the
[`ConnorGray/Organizer`](https://resources.wolframcloud.com/PacletRepository/resources/ConnorGray/Organizer/)
paclet.

`ConnorGray/Organizer` provides a notebook interface application for organizing project
notes and to-dos.

## Installation

`ConnorGray/Organizer` is available for installation from the [Wolfram Paclet Repository][WPR]:

```wolfram
PacletInstall[ResourceObject["ConnorGray/Organizer"]]
```

*Note: Installing from the Wolfram Paclet Repository currently requires Wolfram Language
v13.0 or later.*

[WPR]: https://resources.wolframcloud.com/PacletRepository/

## First-time setup

After [installing Organizer](#installation), you can perform first-time setup by opening the main
palette window. Once installed, Organizer will automatically appear in the system
**Palettes > Organizer** menu item. Click it to open the Organizer palette.

Read the [Organizer Overview tutorial](https://resources.wolframcloud.com/PacletRepository/resources/ConnorGray/Organizer/Organizer/tutorial/Overview.html)
to learn more.

> If you've previously set-up the organizer system, but would like to change the root
> directory the system uses, you do so by evaluating:
>
> ```wolfram
> PersistentValue["CG:Organizer:RootDirectory", "Local"] = SystemDialogInput["Directory"];
> ```
>
> Support for multiple organizer roots is not currently implemented. Instead it's suggested
> that you use the "workspaces" feature to split a single directory into distinct areas.

### On-disk directory structure

```
{RootDirectory}/{Workspace..}/{Category..}/{Project..}/Log.nb
```

### Credits

The icons:

* [CalendarWithPlus.svg](./Icons/CalendarWithPlus.svg)
* [UnfinishedTodoList.svg](./Icons/UnfinishedTodoList.svg)
* [Plus.svg](./Icons/Plus.svg)
* [FileLink.svg](./Icons/FileLink.svg)
* [LinkArea.svg](./Icons/LinkArea.svg)
* [OpenFolder.svg](./Icons/OpenFolder.svg)
* [MessageLink.svg](./Icons/MessageLink.svg)
* [BrowserLink.svg](./Icons/BrowserLink.svg)

are from [www.onlinewebfonts.com/icon](http://www.onlinewebfonts.com/icon), licensed as
CC BY 3.0.
