Data and Workspace Browser for Terminals 
========================================
Terminal-based-tools for viewing data, summarizing contents of objects and environments, deciphering unknown objects, and aid debugging complex functions.
It was developed to facilitate working with R in combination with the brilliant text editor [vim](http://www.vim.org) or over ssh connections without RStudio server,
but it works fine within RStudio or other GUIs too.

Installation
------------
The latest official release of the package is available on CRAN.
```
install.packages("dataview")
```
You typically also want to add these lines to the end of your `~/.Rprofile` to auto-load the package and hide all objects created during the startup
```
require(dataview)
whos.exclude(ls())
```
and perhaps also
```
print.factor <- heat.view
```

Usage & Functionality
---------------------
`dataview` centers around the functions `whos` inspired by the same function in MATLAB,
`entry.view` inspired by data base systems,
and `heat.view` that can visualize long vectors in a very compact manner (especially factors).

![simple structures](https://raw.githubusercontent.com/backlin/dataview/images/images/whos_entry_heat.png)

More complex objects are however better studied with the `browse` function that combines the above functions in stepwise manner. Note that it also supports partial name matching and tab completion!

![complex structures](https://raw.githubusercontent.com/backlin/dataview/images/images/browse.png)

Colors
------
The color schemes used by `dataview` are defined in the `xtermStyle` package. To change the default palette (dark on light terminal background) you must attach it and do the following:
```
require(xtermStyle)
style.palette("light") # Designed for dark terminal background
```
`style.palette` also let's you define your own palette if you are not happy with the pre-defined ones.