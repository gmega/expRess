% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/express.R
\name{express}
\alias{express}
\title{Graphs from R Expressions}
\usage{
express(expr, merge = TRUE, label_formatter = default_label_formatter)
}
\arguments{
\item{expr}{an R expression. Typically a composite function (e.g. `f(g(x), h(x))`).}

\item{merge}{if set to `TRUE` (the default), merges input variables with the same name
into the same graph node.}

\item{label_formatter}{a function which takes a character vector and returns a character
vector.}
}
\value{
a DiagrammeR graph corresponding to the supplied expression.
}
\description{
Creates a plottable (DiagrammeR) expression graph from an unevaluated R expression.
By default, merges the innermost symbols (input variables) with the same label into
common nodes, similarly to the kind of expression graph one would see in a neural
network.
}
\examples{
library(magrittr)
g <- express(f_1(f_2(f_3(x_1, x_2), x_3), f_1(f_4(f_5(x_1), f_6(x_2), x_4))))
g \%>\% DiagrammeR::render_graph()
}
