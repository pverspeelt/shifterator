% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entropy.R
\name{get_jsd_word_scores}
\alias{get_jsd_word_scores}
\title{Get JS Divergence Scores}
\usage{
get_jsd_word_scores(
  type2score_1,
  type2score_2,
  weight_1,
  weight_2,
  base,
  alpha
)
}
\arguments{
\item{type2score_1}{}

\item{type2score_2}{}

\item{weight_1}{Relative weight of type2freq_1 when constructing the mixed distribution.
Together with weight_2 should sum to 1.}

\item{weight_2}{Relative weight of type2freq_2 when constructing the mixed distribution.
Together with weight_1 should sum to 1.}

\item{base}{The base for the logarithm when computing entropy scores.}

\item{alpha}{The parameter for the generalized Tsallis entropy. Setting 'alpha = 1'
recovers the Shannon entropy.}
}
\value{
A data.frame containing the js scores
}
\description{
Calculates the contribution of the types in two systems to the Jensen-Shannon
divergence (JSD) between those systems
}
\keyword{internal}
