% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/namespaced-exprs.R
\name{namespaced-exprs}
\alias{namespaced-exprs}
\alias{is_ns_sym}
\alias{is_ns_sym_export}
\alias{is_ns_sym_private}
\alias{is_ns_call}
\alias{is_ns_call_export}
\alias{is_ns_call_private}
\title{Test for namespaced symbols and calls}
\usage{
is_ns_sym(x, ..., ns = NULL, private = NULL, quote = TRUE)

is_ns_sym_export(x, ..., ns = NULL, quote = TRUE)

is_ns_sym_private(x, ..., ns = NULL, quote = TRUE)

is_ns_call(x, ..., ns = NULL, private = NULL, quote = TRUE)

is_ns_call_export(x, ..., ns = NULL, quote = TRUE)

is_ns_call_private(x, ..., ns = NULL, quote = TRUE)
}
\arguments{
\item{x}{an R object.}

\item{...}{must be empty.}

\item{ns}{a string.}

\item{private}{a logical or \code{NULL}(default). If \code{TRUE}, it returns \code{TRUE} only if the function is NOT an exported function of package \code{ns}. If \code{FALSE}, it returns \code{TRUE} only if the function is an exported function of package \code{ns}.}

\item{quote}{a logical. If \code{TRUE}, strings are first converted to expression wwith \code{str2lang}.}
}
\value{
a logical.
}
\description{
Test for namespaced symbols and calls
}
