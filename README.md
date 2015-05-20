The R inspector
===============
Tools for overviewing the contents of objects and environments in a clean, colorful and human readable way.
The package was developed to facilitate the deciphering of unknown objects and debugging complex functions.


It centers around the functions `whos` inspired by the same function in MATLAB,
`entry.view` inspired by data base systems,
and `heat.view` that can visualize long vectors in a very compact manner (especially factors).

Installation
------------
The latest official release of the package is available on CRAN.
```
install.packages("inspectr")
```
You typically also want to add these lines to the end of your `~/.Rprofile` to auto-load the package and hide all objects created during the startup
```
require(inspectr)
whos.exclude(ls())
```
and perhaps also
```
print.factor <- heat.view
```

Usage & Functionality
---------------------
The functions `whos`, `entry.view`, and `heat.view` can be used to summarize the contents of simple objects.

![simple structures](https://raw.githubusercontent.com/backlin/dataview/images/images/inspector.png)

More complex objects are better studied with the `browse` function.

![complex structures](https://raw.githubusercontent.com/backlin/dataview/images/images/browse.png)


Colors
------
The color schemes used by Inspector are defined in the `xtermStyle` package. To change the default palette (dark on light terminal background) you must attach it and do the following:
```
require(xtermStyle)
style.palette("light") # Designed for dark terminal background
```
`style.palette` also let's you define your own palette if you are not happy with the pre-defined ones.