% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marc_datafield.R
\name{MarcDatafield}
\alias{MarcDatafield}
\title{R6 Class Representing a Marc21 data field}
\description{
This is the base class used to store and interact with marc data fields
}
\details{
This class provides the base functionality for storing marc data, as well as providing methods
to retrieve that data and convert it into other formats like data.frames
}
\examples{

## ------------------------------------------------
## Method `MarcDatafield$to_data_frame`
## ------------------------------------------------

record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

data_field <- record$get_fields(tag = "550")[[1]]
data_field$to_data_frame()

## ------------------------------------------------
## Method `MarcDatafield$get_values`
## ------------------------------------------------

record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

data_field <- record$get_fields(tag = "550")[[1]]
data_field$get_values(c("i", "a"))

## ------------------------------------------------
## Method `MarcDatafield$new`
## ------------------------------------------------

field <- MarcDatafield$new(tag = 550,
   ind_1 = " ",
   ind_2 = " ",
   codes = c("0", "a"),
   values = c("A value", "Another value"))
field
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{tag}}{Marc tag of the data field}

\item{\code{ind_1}}{First indictator of the data field}

\item{\code{ind_2}}{Second indictator of the data field}

\item{\code{codes}}{Subfield codes in this data field}

\item{\code{values}}{Subfield values of this data field}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-to_data_frame}{\code{MarcDatafield$to_data_frame()}}
\item \href{#method-get_values}{\code{MarcDatafield$get_values()}}
\item \href{#method-print}{\code{MarcDatafield$print()}}
\item \href{#method-new}{\code{MarcDatafield$new()}}
\item \href{#method-clone}{\code{MarcDatafield$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-to_data_frame"></a>}}
\if{latex}{\out{\hypertarget{method-to_data_frame}{}}}
\subsection{Method \code{to_data_frame()}}{
Simplifies this data field by coercing it into a data frame.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MarcDatafield$to_data_frame()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame containing the contents of this data field
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

data_field <- record$get_fields(tag = "550")[[1]]
data_field$to_data_frame()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_values"></a>}}
\if{latex}{\out{\hypertarget{method-get_values}{}}}
\subsection{Method \code{get_values()}}{
Retrieves values for one or more codes from the data field
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MarcDatafield$get_values(codes)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{codes}}{The code or codes for which to retreive values. Must be a vector of size >= 1 and type character}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character vector containing the requested values if there are any, NA otherwise
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{record <- MarcRecord$new()
record$read_record("http://d-nb.info/gnd/11897792X/about/marcxml")

data_field <- record$get_fields(tag = "550")[[1]]
data_field$get_values(c("i", "a"))
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print method for a marc data field
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MarcDatafield$print(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Constructor method for a marc data field.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MarcDatafield$new(tag, ind_1, ind_2, codes, values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{tag}}{Optional. The tag of the data field.}

\item{\code{ind_1}}{Optional. First indicator of the data field.}

\item{\code{ind_2}}{Optional. Second indicator of the data field.}

\item{\code{codes}}{Optional. Data codes of the data field.}

\item{\code{values}}{Optional. Data values of the data field.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{field <- MarcDatafield$new(tag = 550,
   ind_1 = " ",
   ind_2 = " ",
   codes = c("0", "a"),
   values = c("A value", "Another value"))
field
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MarcDatafield$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
