% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{fn}
\alias{fn}
\alias{fn_type}
\alias{fn_is_closure}
\alias{fn_is_special}
\alias{fn_is_builtin}
\alias{fn_name}
\alias{fn_names}
\alias{fn_set_name}
\alias{fn_ns}
\alias{fn_set_ns}
\alias{fn_in_ns}
\alias{fn_is_private}
\alias{fn_is_export}
\title{Extend rlang's fn functions}
\usage{
fn_type(fn)

fn_is_closure(fn)

fn_is_special(fn)

fn_is_builtin(fn)

fn_name(fn, ..., with_ns = TRUE)

fn_names(..., with_ns = TRUE)

fn_set_name(fn)

fn_ns(fn)

fn_set_ns(fn, ns)

fn_in_ns(fn, ns)

fn_is_private(fn, ns = NULL)

fn_is_export(fn, ns = NULL)
}
\arguments{
\item{fn}{a function.}

\item{with_ns}{a logical. If \code{TRUE}, \verb{fn_name(s)(*)} return  the name(s) of the functions as input including their namespace.}

\item{ns}{a string. The name of a namespace.}
}
\description{
Extend rlang's fn functions
}