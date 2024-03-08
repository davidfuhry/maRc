# maRc - Read MarcXML in R

This package provides methods to work with Marc files for R.
Currently only read operations are supported for MarcXML files, though write support and the ability to deal with non-XML Marc files might be added in the future.

## Installation

You can use the devtools package to install the package directly from Github:

```{R}
devtools::install_github("davidfuhry/maRc")
```

## Basic usage

This package uses R6 classes. An example to read a marcXML file and extract information from specific fields:

```{R}
library(maRc)
record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

# Get fields with optional filtering
record$get_fields(tag = c(548, 550), simplify = TRUE)
```

