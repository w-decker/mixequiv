% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_equiv.R
\name{test_equiv}
\alias{test_equiv}
\title{Test equivalence of mixed model parameter}
\usage{
test_equiv(model, method, mmd, mmd_lower, mmd_upper, term, alpha = 0.05)
}
\arguments{
\item{model}{lmer object}

\item{method}{desired equivalence test approach ("CI" or "TOST")}

\item{mmd}{minimal meaningful difference}

\item{mmd_lower}{minimal meaningful difference lower boundary}

\item{mmd_upper}{minimal meaningful difference upper boundary}

\item{term}{model term}

\item{alpha}{desired alpha}
}
\value{
list of results
}
\description{
Execute an equivalence test (TOST or CI approach) on a mixed model parameter
with a specified minimal meaningful difference and desired alpha.
}
