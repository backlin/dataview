Synesthesia for R
========
A terminal tool for overviewing your workspace and objects in a clean, colorful and human readable way.

If you prefer to do your R work in the terminal (probably in combination with Vim or Emacs)
`synesthesia` is a great little tool for keeping track of cluttered workspaces and disecting obscure objects.
It centers around the functions `whos` inspired by the same function in MATLAB,
`entry.view` inspired by data base systems,
and `heat.view` that can visualize long vectors in a very compact manner (especially factors).

Installation & Usage
------------
As it is a complete reimplementation of the `dataview` package, the installation procedure is a bit dirty at the moment.
It will soon take over as the official release and be published on CRAN, and `dataview` will be discontinued. 
```
library(devtools)
install_github("backlin/dataview@develop")
```
Then you typically also want to add these lines to the end of your `~/.Rprofile`
```
require(synesthesia)
whos.set.mask()
```
and perhaps also
```
print.factor <- heat.view # Not yet implemented, but coming soon!
```
