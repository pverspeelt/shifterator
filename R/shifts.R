#' Weighted Average Shift
#' 
#' Shift object for calculating weighted scores of two systems of types, and the shift between them.
#'
#' @inheritParams shift
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples 
#' Example to follow.
weighted_avg_shift <- function(type2freq_1,
                             type2freq_2,
                             type2score_1 = NULL,
                             type2score_2 = NULL,
                             reference_value = NULL,
                             handle_missing_scores = "error",
                             stop_lens = NULL,
                             stop_words = "",
                             normalization = "variation"){
  
  shift(type2freq_1 = type2freq_1,
        type2freq_2 = type2freq_2,
        type2score_1 = type2score_1,
        type2score_2 = type2score_2,
        reference_value = reference_value,
        handle_missing_scores = handle_missing_scores,
        stop_lens = stop_lens,
        stop_words = stop_words,
        normalization = normalization) 
}


#' Proportion Shift
#' 
#' Shift object for calculating differences in proportions of types across two systems.
#'
#' @inheritParams shift 
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples
#' Example to follow.
proportion_shift <- function(type2freq_1, 
                             type2freq_2){
  
  
  types <- union(type2freq_1$word, type2freq_2$word)
  
  ### need to vectorize this part of the code.
  for(word_to_process in types){
    if(sum(type2freq_1$word == word_to_process) == 0){
      type2freq_1 <- rbind(type2freq_1, list(word = word_to_process, freq_1 = 0))
    } else if(sum(type2freq_2$word == word_to_process) == 0){
      type2freq_2 <- rbind(type2freq_2, list(word = word_to_process, freq_2 = 0))
    }
  }

  shift(type2freq_1 = type2freq_1,
        type2freq_2 = type2freq_2,
        type2score_1 = NULL,
        type2score_2 = NULL,
        reference_value = 0,
        handle_missing_scores = "error",
        stop_lens = NULL,
        stop_words = NULL,
        normalization = "variation") 
  
}


#' Title
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
#' example to follow
entropy_shift <- function(type2freq_1,
                          type2freq_2,
                          base = 2L,
                          alpha = 1,
                          reference_value = 0,
                          normalization = "variation"){
  
  ### add checks on base and alpha
  
  ### add checks on type2freq_1 and type2freq_2 same checks in shift
  
  
  # get the relative frequencies
  type2score_1 <- get_relative_frequency(type2freq_1)
  names(type2score_1) <- c("word", "score_1")
  type2score_2 <- get_relative_frequency(type2freq_2)
  names(type2score_2) <- c("word", "score_2")
  
  # get the entropy scores
  entropy_scores <- get_entropy_scores(type2score_1, type2score_2, base, alpha)
  type2score_1 <- entropy_scores[, c("word", "score_1")]
  type2score_2 <- entropy_scores[, c("word", "score_2")]
  
  shift(type2freq_1 = type2freq_1,
        type2freq_2 = type2freq_2,
        type2score_1 = type2score_1,
        type2score_2 = type2score_2,
        handle_missing_scores = "error",
        stop_lens = NULL,
        stop_words = NULL,
        reference_value = reference_value,
        normalization = "variation") 

}
