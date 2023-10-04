---
editor_options: 
  markdown
---

## Function naming convention:

first letter NOT caps, first letter of subsequent words CAPS, acronyms in CAPS

Exceptions: 
1. Utility function: they get utils_xxx.R style naming 
2. Plotting functions: plotXxxxx.R 
3. Functions that GET data from somewhere: getXxxxx.R 
4. Functions that create a product, perform a task: no specific scheme, but follow regular naming convention.

## Other

-   1 file per function, 1 function per file!
-   Development files (or just files to keep in case they're useful) should go in the /Dev folder. /Dev is included in .buildignore.

## Migration steps left to do

4.  Everyone seeks out bugs!
5.  Run devtools::check() often!!!
6.  G will re-run vignettes once everything is together.

## Extra things to do at the same time

1.  Think about writing TESTS!!!!
2.  Minimize as many dependencies as possible; if it's used already, stick to it!
3.  If using external function for very specific purpose (narrow scope of the function's full use) just write the code directly in your fx

## Post migration:

1.  Update virtual machine
