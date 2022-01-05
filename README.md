# maRc - Read MarcXML in R

This package provides methods to work with Marc files for R.
Currently only read operations are supported for MarcXML files, though write support and the ability to deal with non-XML Marc files is planned.

## Installation

This package is still in early development.
You can use the devtools package to install it directly from Github:

```{R}
devtools::install_github("davidfuhry/maRc")
```

## Basic usage

This package uses R6 classes. Example to read a MarcXML file:

```{R}
library(maRc)
record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

# Get fields with optional filtering
record$get_fields(tag = c(548, 550), simplify = TRUE)
```

