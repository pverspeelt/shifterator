% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shifts.R
\name{entropy_shift}
\alias{entropy_shift}
\title{Entropy Shift}
\usage{
entropy_shift(
  type2freq_1,
  type2freq_2,
  base = 2L,
  alpha = 1,
  reference_value = 0,
  normalization = "variation"
)
}
\arguments{
\item{type2freq_1}{A data.frame containing words and their frequencies.}

\item{type2freq_2}{A data.frame containing words and their frequencies.}

\item{base}{The base for the logarithm when computing entropy scores.}

\item{alpha}{The parameter for the generalized Tsallis entropy. Setting 'alpha = 1'
recovers the Shannon entropy.}

\item{reference_value}{Optional. String or numeric. The reference score to use
to partition scores into two different regimes. If 'average', uses the average
score according to type2freq_1 and type2score_1. If a lexicon is used for type2score,
you need to use the middle point of that lexicon's scale. If no value is supplied,
zero will be used as the reference point. See details for more information.}

\item{normalization}{Optional. Default value: "variation". If 'variation', normalizes
shift scores so that the sum of their absolute values sums to 1. If 'trajectory',
normalizes them so that the sum of shift scores is 1 or -1. The trajectory
normalization cannot be applied if the total shift score is 0, so scores are
left unnormalized if the total is 0 and 'trajectory' is specified.}
}
\value{
Returns a list object of class shift.
}
\description{
Shift object for calculating the shift in entropy between two systems.
}
\section{Shannon Entropy Shifts}{
We can use the Shannon entropy to identify more "surprising" words and how
they vary between two texts. The Shannon entropy \emph{H} is calculated as:

\eqn{H(P) = \sum_i p_i \log \frac{1}{p_i}} Where the factor \eqn{-\log p_i}
is the surprisal value of a word. The less often a word appears in a text,
the more surprising it is. The Shannon entropy can be interpreted as the
average surprisal value of a text. We can compare two texts by taking the difference
between their entropies, \eqn{H(P^{(2)}) - H(P^{(1)})}. When we do this, we
can get the contribution \eqn{\delta H_i} of each word to that difference:

\eqn{\delta H_i = p_i^{(2)} \log \frac{1}{p_i^{(2)}} - p_i^{(1)} \log \frac{1}{p_i^{(1)}}}

We can rank these contributions and plot them as a Shannon entropy word shift.
If the contribution \eqn{\delta H_i} is positive, then word \emph{i} the has a
higher score in the second text. If the contribution is negative, then its
score is higher in the first text.

The contributions \eqn{\delta H_i} are available in the type2shift_score
column in the shift_scores data.frame in the shift object. The surprisals
are available in the type2score_1 and type2score_2 columns.
}

\section{Tsallis Entropy Shifts}{
The Tsallis entropy is a generalization of the Shannon entropy which allows
us to emphasize common or less common words by altering an order parameter
\eqn{\alpha} \> 0. When \eqn{\alpha} \< 1, uncommon words are weighted more
heavily, and when \eqn{\alpha} \> 1, common words are weighted more heavily.
In the case where \eqn{\alpha} = 1, the Tsallis entropy is equivalent to the
Shannon entropy, which equally weights common and uncommon words.

The contribution \eqn{\delta H_i^{\alpha}} of a word to the difference in
Tsallis entropy of two texts is given by

\eqn{\delta H_i^{\alpha} = \frac{-\bigl(p_i^{(2)}\bigr)^\alpha + \bigl(p_i^{(1)}\bigr)^\alpha}{\alpha - 1}}.

The Tsallis entropy can be calculated using \code{entropy_shift} by passing it the
parameter \code{alpha}.
}

\examples{
library(shifterator)
library(quanteda)
library(quanteda.textstats)
library(dplyr)

reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") \%>\% 
  tokens(remove_punct = TRUE) \%>\% 
dfm() \%>\% 
textstat_frequency() \%>\% 
as.data.frame() \%>\% # to move from classes frequency, textstat, and data.frame to data.frame
select(feature, frequency) 

bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") \%>\% 
tokens(remove_punct = TRUE) \%>\% 
dfm() \%>\% 
textstat_frequency() \%>\% 
as.data.frame() \%>\% 
select(feature, frequency)

shannon_entropy_shift <- entropy_shift(reagan, bush)

tsallis_entropy_shift <- entropy_shift(reagan, bush, alpha = 0.8)

}
\seealso{
Other shifts: 
\code{\link{jsdivergence_shift}()},
\code{\link{kldivergence_shift}()},
\code{\link{proportion_shift}()},
\code{\link{weighted_avg_shift}()}
}
\concept{shifts}
