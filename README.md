# Motonad
Motonad is a simple, fast CLI todo and deadline tracker.  

# Usage
Firstly, create a `motonadrc` file in your `$HOME/.config/motonad/` folder.

To simply acknowledge your deadlines,
```
$ motonad
You have no tasks to complete.
```
Or, when there are deadlines present,
```
$ motonad
DUE 2022-04-18 00:00:00 UTC:
	Buy groceries
DUE 2023-04-17 00:00:00 UTC:
	Complete motonad project
```

To add a deadline,
```
$ motonad +nX "..."
```
Where X can be `d`, `m`, `y` for day, month, and year respectively; and n is for how many `X`.
For example,
```
[2022-04-17] $ motonad +5d "Write essay"
[2022-04-17] $ motonad
DUE 2022-04-22 00:00:00 UTC:
	Write essay
```

# Configuration
The `motonadrc` file is written in a specific format so as to be terse, yet contain extra details about how your deadlines are sorted, and displayed.
An example `motonadrc` may look like this:
```
$ cat ~/.config/motonad/motonadrc
{: due [2022-04-22T00:00:00Z]
Write essay
{: due [2022-04-18T00:00:00Z]
Go for a run
{: due [2022-05-17T00:00:00Z]
Complete project
```
