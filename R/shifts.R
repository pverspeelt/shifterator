#' Weighted Average Shift
#' 
#' Shift object for calculating weighted scores of two systems of types, and the shift between them.
#' 
#' @details 
#' \strong{reference_value:}
#' When a lexicon is used for type2score, you have to supply the middle point of 
#' the lexicon's scale. If the scale is from 1 to 9, the middle point you should use is 5.
#' If no reference value is given, a value of 0 will be used. This might skew the results when 
#' calculating the shift scores.
#' 
#' \strong{stop_lens:}
#' Stop_lens can be used to remove words that fall within a range from the shift score calculations.
#' This should be used in combination supplying a lexicon. If the scale of the lexicon is from one to 
#' nine, you can, for example, remove the words that would have a score between 4 and 6 by supplying
#' a vector of \code{c(4, 6)}. 
#' 
#'
#' @inheritParams shift
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#' @examples 
#' #' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' was <- weighted_avg_shift(reagan, bush, handle_missing_scores = "exclude")
#' 

weighted_avg_shift <- function(type2freq_1,
                             type2freq_2,
                             type2score_1 = NULL,
                             type2score_2 = NULL,
                             reference_value = NULL,
                             handle_missing_scores = "error",
                             stop_lens = NULL,
                             stop_words = NULL,
                             normalization = "variation"){
  
  # check inputs
  type2freq_1 <- check_and_rename(type2freq_1, 
                                  name_x = "type2freq_1",
                                  column_names = c("word", "freq_1"))
  
  type2freq_2 <- check_and_rename(type2freq_2, 
                                  name_x = "type2freq_2",
                                  column_names = c("word", "freq_2"))
  
  if(!is.null(type2score_1)){
    type2score_1 <- check_and_rename(type2score_1, 
                                     name_x = "type2score_1",
                                     column_names = c("word", "score_1")) 
  }
  
  if(!is.null(type2score_2)){
    type2score_2 <- check_and_rename(type2score_2, 
                                     name_x = "type2score_2",
                                     column_names = c("word", "score_2")) 
  }
  
  
  if(length(handle_missing_scores) != 1){
    message(sprintf("multiple values in handle_missing_scores: %s", paste0(handle_missing_scores, collapse = ", ")),
            "\nUse one of the following options: 'error', 'exclude' or 'adopt'.")
    stop_quietly()
  }
  
  if(!handle_missing_scores %in% c("error", "exclude", "adopt")){
    message(sprintf("Incorrect value for handle_missing_scores. You used: %s", handle_missing_scores),
            "\nUse one of the following options: 'error', 'exclude' or 'adopt'.")
    stop_quietly()
  }
  
  if(!normalization %in% c("variation", "trajectory")){
    message(sprintf("Incorrect value for normalization. You used: %s", normalization),
            "\nUse one of the following options: 'variation' or 'trajectory'.")
    stop_quietly()
  }
  
  if(!is.null(reference_value)){
    if(is.character(reference_value) && reference_value != "average"){
      message(sprintf("Incorrect value for reference_value: %s", reference_value),
              "\nUse a numeric value or use the value: 'average'. Check the help or vignette for more information.")
      stop_quietly()
    } 
  }
  
  # check stop_lens
  if(!is.null(stop_lens)){
    if(length(stop_lens) != 2){
      message("stop_lens needs to be a vector of 2 values with the first value lower than the second value.")
      stop_quietly()
    } else if((stop_lens[1] < stop_lens[2]) == FALSE) {
      message("stop_lens needs to be a vector of 2 values with the first value lower than the second value.")
      stop_quietly()
    }
  }
  
  if(!is.null(stop_words) && !is.character(stop_words)){
    message("stop_words needs to be a character vector.")
    stop_quietly()
  }
  
  
  

  weighted_out <- shift(type2freq_1 = type2freq_1,
                        type2freq_2 = type2freq_2,
                        type2score_1 = type2score_1,
                        type2score_2 = type2score_2,
                        reference_value = reference_value,
                        handle_missing_scores = handle_missing_scores,
                        stop_lens = stop_lens,
                        stop_words = stop_words,
                        normalization = normalization) 
  
  # Needed for showing the score on the plotting title.
  weighted_out$avg_score <- avg_weighted_score(weighted_out$shift_scores)
  
  weighted_out
}


#' Proportion Shift
#' 
#' Shift object for calculating differences in proportions of types across two systems.
#' 
#' The easiest word shift graph that we can construct is a proportion
#' shift. If \eqn{p_i^{(1)}} is the relative frequency of word *i* in the first
#' text, and \eqn{p_i^{(2)}} is its relative frequency in the second text, then
#' the proportion shift calculates their difference: 
#' 
#' **\eqn{\delta p_i = p_i^{(2)} - p_i^{(1)}}** 
#' 
#' If the difference is positive (\eqn{\delta p_i > 0}), then the word is 
#' relatively more common in the second text. If it is negative (\eqn{\delta p_i < 0}), 
#' then it is relatively more common in the first text. We can rank words by 
#' this difference and plot them as a word shift graph.
#'
#' @inheritParams shift 
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples
#' #' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' prop <- proportion_shift(reagan, bush)

proportion_shift <- function(type2freq_1, 
                             type2freq_2){
  
  # check inputs
  type2freq_1 <- check_and_rename(type2freq_1, 
                                  name_x = "type2freq_1",
                                  column_names = c("word", "freq_1"))
  
  type2freq_2 <- check_and_rename(type2freq_2, 
                                  name_x = "type2freq_2",
                                  column_names = c("word", "freq_2"))
  
  
  # merge and split systems so that all words exist in both.
  # words that were not in both systems are set to 0
  merge_and_split <- function(x, y){
    merged_set <- merge(x, y, by = "word", all = T)
    split_1 <- merged_set[ , c("word", "freq_1")]
    split_1$freq_1[is.na(split_1$freq_1)] <- 0  
    split_2 <- merged_set[ , c("word", "freq_2")]
    split_2$freq_2[is.na(split_2$freq_2)] <- 0 
    
    out <- list(type2freq_1 = split_1, 
                type2freq_2 = split_2)
  }
  
  mas <- merge_and_split(type2freq_1, type2freq_2)
  
  # overwrite input systems with new values
  type2freq_1 <- mas$type2freq_1
  type2freq_2 <- mas$type2freq_2
  
  prop_out <- shift(type2freq_1 = type2freq_1,
                    type2freq_2 = type2freq_2,
                    type2score_1 = NULL,
                    type2score_2 = NULL,
                    reference_value = 0,
                    handle_missing_scores = "error",
                    stop_lens = NULL,
                    stop_words = NULL,
                    normalization = "variation")
  
  # Plotting for proportion shift does not need a total.
  prop_out$show_total <- FALSE
  
  prop_out
}


#' Entropy Shift
#' 
#' Shift object for calculating the shift in entropy between two systems.
#' 
#' # Shannon Entropy Shifts
#' 
#' We can use the Shannon entropy to identify more "surprising" words and how 
#' they vary between two texts. The Shannon entropy *H* is calculated as: 
#' 
#' \eqn{H(P) = \sum_i p_i \log \frac{1}{p_i}} Where the factor \eqn{-\log p_i} 
#' is the surprisal value of a word. The less often a word appears in a text, 
#' the more surprising it is. The Shannon entropy can be interpreted as the 
#' average surprisal value of a text. We can compare two texts by taking the difference 
#' between their entropies, \eqn{H(P^{(2)}) - H(P^{(1)})}. When we do this, we 
#' can get the contribution \eqn{\delta H_i} of each word to that difference:
#' 
#' \eqn{\delta H_i = p_i^{(2)} \log \frac{1}{p_i^{(2)}} - p_i^{(1)} \log \frac{1}{p_i^{(1)}}}
#' 
#' We can rank these contributions and plot them as a Shannon entropy word shift. 
#' If the contribution \eqn{\delta H_i} is positive, then word *i* the has a 
#' higher score in the second text. If the contribution is negative, then its 
#' score is higher in the first text.
#' 
#' The contributions \eqn{\delta H_i} are available in the type2shift_score 
#' column in the shift_scores data.frame in the shift object. The surprisals 
#' are available in the type2score_1 and type2score_2 columns.
#' 
#' # Tsallis Entropy Shifts
#' 
#' The Tsallis entropy is a generalization of the Shannon entropy which allows 
#' us to emphasize common or less common words by altering an order parameter 
#' \eqn{\alpha} \> 0. When \eqn{\alpha} \< 1, uncommon words are weighted more 
#' heavily, and when \eqn{\alpha} \> 1, common words are weighted more heavily. 
#' In the case where \eqn{\alpha} = 1, the Tsallis entropy is equivalent to the 
#' Shannon entropy, which equally weights common and uncommon words. 
#' 
#' The contribution \eqn{\delta H_i^{\alpha}} of a word to the difference in 
#' Tsallis entropy of two texts is given by 
#' 
#' \eqn{\delta H_i^{\alpha} = \frac{-\bigl(p_i^{(2)}\bigr)^\alpha + \bigl(p_i^{(1)}\bigr)^\alpha}{\alpha - 1}}.
#' 
#' The Tsallis entropy can be calculated using `entropy_shift` by passing it the 
#' parameter `alpha`.
#' 
#'
#' @inheritParams shift
#' @param base The base for the logarithm when computing entropy scores.
#' @param alpha The parameter for the generalized Tsallis entropy. Setting 'alpha = 1'
#' recovers the Shannon entropy.
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples
#' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' shannon_entropy_shift <- entropy_shift(reagan, bush)
#' 
#' tsallis_entropy_shift <- entropy_shift(reagan, bush, alpha = 0.8)
#' 
entropy_shift <- function(type2freq_1,
                          type2freq_2,
                          base = 2L,
                          alpha = 1,
                          reference_value = 0,
                          normalization = "variation"){
  
  ### add checks on base and alpha
  
  # check inputs
  type2freq_1 <- check_and_rename(type2freq_1, 
                                  name_x = "type2freq_1",
                                  column_names = c("word", "freq_1"))
  
  type2freq_2 <- check_and_rename(type2freq_2, 
                                  name_x = "type2freq_2",
                                  column_names = c("word", "freq_2"))
  
  if(!normalization %in% c("variation", "trajectory")){
    message(sprintf("Incorrect value for normalization. You used: %s", normalization),
            "\nUse one of the following options: 'variation' or 'trajectory'.")
    stop_quietly()
  }
  
  if(!is.numeric(base) || base < 0) {
    message(sprintf("incorrect value for base: %s", base),
            "\nUse a positive number.")
    stop_quietly()
  }
  
  if(!is.numeric(alpha)) {
    message(sprintf("incorrect value for alpha: %s", alpha),
            "\nUse a numeric value.")
    stop_quietly()
  }
  
  if(!is.null(reference_value)){
    if(is.character(reference_value) && reference_value != "average"){
      message(sprintf("Incorrect value for reference_value: %s", reference_value),
              "\nUse a numeric value or use the value: 'average'. Check the help or vignette for more information.")
      stop_quietly()
    } 
  }
  
  
  # get the relative frequencies
  type2score_1 <- get_relative_frequency(type2freq_1)
  names(type2score_1) <- c("word", "score_1")
  type2score_2 <- get_relative_frequency(type2freq_2)
  names(type2score_2) <- c("word", "score_2")
  
  # get the entropy scores
  entropy_scores <- get_entropy_scores(type2score_1, type2score_2, base, alpha)
  type2score_1 <- entropy_scores[, c("word", "score_1")]
  type2score_2 <- entropy_scores[, c("word", "score_2")]
  
  entropy_out <- shift(type2freq_1 = type2freq_1,
                       type2freq_2 = type2freq_2,
                       type2score_1 = type2score_1,
                       type2score_2 = type2score_2,
                       handle_missing_scores = "error",
                       stop_lens = NULL,
                       stop_words = NULL,
                       reference_value = reference_value,
                       normalization = normalization) 
  
  # Needed for showing the score on the plotting title.
  entropy_out$avg_score <-  avg_weighted_score(entropy_out$shift_scores)
  
  entropy_out
}


#' Kullback-Leibler Divergence Shift
#' 
#' Shift object for calculating the Kullback-Leibler divergence (KLD) between two systems. 
#' 
#' The Kullback-Leibler divergence (KLD) is a useful asymmetric measure of
#' how two texts differ. One text is the reference text and the other is
#' the comparison text. If we let type2freq_1 be the reference text and 
#' type2freq_2 be the comparison text, then we can calculate the KLD as 
#' 
#' \eqn{D^{(KL)}(P^{(2)} || P^{(1)}) = \sum_i p_i^{(2)} \log \frac{p_i^{(2)}}{p_i^{(1)}}}.
#' 
#' A word's contribution can be written as the difference in surprisals between 
#' the reference and comparison text, similar to the Shannon entropy except 
#' weighting each surprisal by the frequency of the word in the comparison. 
#' 
#' \eqn{\delta KLD_i = p_i^{(2)} \log \frac{1}{p_i^{(1)}} - p_i^{(2)} \log \frac{1}{p_i^{2}}}.
#' 
#' The contribution is positive if \eqn{p_i^{(2)} > p_i^{(1)}}. Similarly, it is 
#' negative if \eqn{p_i^{(2)} < p_i^{(1)}}.
#' 
#' The total Kullback-Leibler divergence be accessed through the `difference` 
#' column in the created shift object.
#' 
#' ## **WARNING**
#' The KLD is only well-defined if every single word in the comparison text
#' is also in the reference text. If this is not the case KLD diverges to infinity.
#' 
#' @inheritParams entropy_shift
#' 
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#' 
#' @examples
#' \dontrun{
#' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' # This will return the message that the KL divergence is not well-defined.
#' kld <- kldivergence_shift(reagan, bush)
#' }

kldivergence_shift <- function(type2freq_1,
                               type2freq_2,
                               base = 2L,
                               reference_value = 0,
                               normalization = "variation"){
  
  # check inputs
  type2freq_1 <- check_and_rename(type2freq_1, 
                                  name_x = "type2freq_1",
                                  column_names = c("word", "freq_1"))
  
  type2freq_2 <- check_and_rename(type2freq_2, 
                                  name_x = "type2freq_2",
                                  column_names = c("word", "freq_2"))
  
  if(!normalization %in% c("variation", "trajectory")){
    message(sprintf("Incorrect value for normalization. You used: %s", normalization),
            "\nUse one of the following options: 'variation' or 'trajectory'.")
    stop_quietly()
  }
  
  if(!is.numeric(base) || base < 0) {
    message(sprintf("incorrect value for base: %s", base),
            "\nUse a positive number.")
    stop_quietly()
  }
  
  if(!is.null(reference_value)){
    if(is.character(reference_value) && reference_value != "average"){
      message(sprintf("Incorrect value for reference_value: %s", reference_value),
              "\nUse a numeric value or use the value: 'average'. Check the help or vignette for more information.")
      stop_quietly()
    } 
  }
  

  if(sum(!type2freq_2$word %in% type2freq_1$word) > 0){
    message(cat("There are types that appear in type2freq_2 but not type2freq_1:",
                "\nthe KL divergence is not well-defined."))
    stop_quietly()
  }
  
  # Get relative frequencies
  type2score_1 <- get_relative_frequency(type2freq_1)
  names(type2score_1) <- c("word", "score_1")
  type2score_2 <- get_relative_frequency(type2freq_2)
  names(type2score_2) <- c("word", "score_2")
  
  # Get surprisal scores
  entropy_scores <- get_entropy_scores(type2score_1, type2score_2, base)
  type2score_1 <- entropy_scores[, c("word", "score_1")]
  type2score_2 <- entropy_scores[, c("word", "score_2")]
  
  kld_out <- shift(type2freq_1 = type2freq_1,
                   type2freq_2 = type2freq_2,
                   type2score_1 = type2score_1,
                   type2score_2 = type2score_2,
                   handle_missing_scores = "error",
                   stop_lens = NULL,
                   stop_words = NULL,
                   reference_value = reference_value,
                   normalization = normalization) 
  
  kld_out
}


#' Jensen-Shannon Divergence Shift
#' 
#' Shift object for calculating the Jensen-Shannon divergence (JSD) between two systems.
#' 
#' The Jensen-Shannon divergence (JSD) accounts for some of the pathologies of 
#' the KLD. It does so by first creating a mixture text \eqn{M}, 
#' 
#' \eqn{M = \pi_1 P^{(1)} + \pi_2 P^{(2)}},
#' 
#' where \eqn{\pi_1} and \eqn{\pi_2} are weights on the mixture between the 
#' two corpora. The JSD is then calculated as the average KLD of each text from 
#' the mixture text,
#' 
#' \eqn{D^{(JS)} \bigl(P^{(1)} || P^{(2)}\bigr) = \pi_1 D^{(KL)} \bigl(P^{(1)} || M \bigr) + \pi_2 D^{(KL)} \bigl(P^{(2)} || M \bigr)}
#' 
#' If the probability of a word in the mixture text is \eqn{m_i = \pi_1 p_i^{(1)} + \pi_2 p_i^{(2)}}, 
#' then an individual word's contribution to the JSD can be written as 
#' 
#' \eqn{\delta JSD_i = m_i \log \frac{1}{m_i} - \biggl( \pi_i p_i^{(1)} \log \frac{1}{p_i^{(1)}} + \pi_2 p_i^{(2)} \log \frac{1}{p_i^{(2)}} \bigg)}
#' 
#' ## **Note**
#' 
#' The JSD is well-defined for every word because the KLD is taken with respect 
#' to the mixture text M, which contains every word from both texts by design. 
#' Unlike the other measures, a word's JSD contribution is always positive, so we 
#' direct it in the word shift graph depending on the text in which it has the 
#' highest relative frequency. A word's contribution is zero if and only if \eqn{p_i^{(1)} = p_i^{(2)}}.
#' 
#' Like the Shannon entropy, the JSD can be generalized using the Tsallis entropy 
#' and the order can be set through the parameter `alpha`.
#' 
#' Quite often the JSD is effective at pulling out distinct words from each corpus 
#' (rather than "stop words"), but it is a more complex measure and so it is harder 
#' to properly interpret it as a whole. 
#' 
#' The total Jensen-Shannon divergence be accessed through the `difference` column 
#' in the shift object.
#'
#' @inheritParams entropy_shift 
#' @param weight_1 Relative weight of type2freq_1 when constructing the mixed distribution.
#' Together with weight_2 should sum to 1.
#' @param weight_2 Relative weight of type2freq_2 when constructing the mixed distribution. 
#' Together with weight_1 should sum to 1.
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples
#' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' jsd <- jsdivergence_shift(reagan, bush)
jsdivergence_shift <- function(type2freq_1,
                               type2freq_2,
                               weight_1 = 0.5,
                               weight_2 = 0.5,
                               base = 2L,
                               alpha = 1,
                               reference_value = 0,
                               normalization = "variation"){
  
  # check inputs
  type2freq_1 <- check_and_rename(type2freq_1, 
                                  name_x = "type2freq_1",
                                  column_names = c("word", "freq_1"))
  
  type2freq_2 <- check_and_rename(type2freq_2, 
                                  name_x = "type2freq_2",
                                  column_names = c("word", "freq_2"))
  
  if(!normalization %in% c("variation", "trajectory")){
    message(sprintf("Incorrect value for normalization. You used: %s", normalization),
            "\nUse one of the following options: 'variation' or 'trajectory'.")
    stop_quietly()
  }
  
  # check that weights sum to 1.  
  if((weight_1 + weight_2) != 1){
    message(sprintf("weight_1 and weight_2 do not sum to 1. They sum to %s.", (weight_1 + weight_2)))
    stop_quietly()
  }
  
  # check that neither of the weights are 0 or negative.  
  if(any(c(weight_1, weight_2) <= 0)){
    message(sprintf("weight_1 and weight_2 can't be 0 or have a negative value. You used the values %s.", 
                    paste(weight_1, weight_2, sep = " and "))
            )
    stop_quietly()
  }
  
  if(!is.numeric(base) || base < 0) {
    message(sprintf("incorrect value for base: %s", base),
            "\nUse a positive number.")
    stop_quietly()
  }
  
  if(!is.numeric(alpha)) {
    message(sprintf("incorrect value for alpha: %s", alpha),
            "\nUse a numeric value.")
    stop_quietly()
  }
  
  if(!is.null(reference_value)){
    if(is.character(reference_value) && reference_value != "average"){
      message(sprintf("Incorrect value for reference_value: %s", reference_value),
              "\nUse a numeric value or use the value: 'average'. Check the help or vignette for more information.")
      stop_quietly()
    } 
  }
  
  
  # Get relative frequencies
  type2score_1 <- get_relative_frequency(type2freq_1)
  names(type2score_1) <- c("word", "score_1")
  type2score_2 <- get_relative_frequency(type2freq_2)
  names(type2score_2) <- c("word", "score_2")
  
  # get jsd scores
  jsd_scores <- get_jsd_word_scores(type2score_1, type2score_2, 
                                    weight_1 = weight_1, 
                                    weight_2 = weight_2, 
                                    base = base, 
                                    alpha = alpha)
  
  type2score_1 <- jsd_scores[, c("word", "score_1")]
  type2score_2 <- jsd_scores[, c("word", "score_2")]
  
  jsd_out <- shift(type2freq_1 = type2freq_1,
                   type2freq_2 = type2freq_2,
                   type2score_1 = type2score_1,
                   type2score_2 = type2score_2,
                   handle_missing_scores = "error",
                   stop_lens = NULL,
                   stop_words = NULL,
                   reference_value = reference_value,
                   normalization = normalization) 
  
  # needed for plotting. If TRUE the shift_scores are all positive.
  # Plotting would be all positive values and checks on this value
  # will shift values to negative for plotting purposes.
  if(alpha == 1 & reference_value == 0){
    jsd_out$all_pos_contributions <- TRUE
  } else {
    jsd_out$all_pos_contributions <- FALSE 
  }
  
  jsd_out
}


