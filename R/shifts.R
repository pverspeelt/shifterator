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


#' Entropy Shift
#' 
#' Shift object for calculating the shift in entropy between two systems.
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


#' Kullback-Leibler Divergence Shift
#' 
#' Shift object for calculating the Kullback-Leibler divergence (KLD) between two systems
#' 
#' 
#' @inheritParams entropy_shift
#' 
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#' 
#' @examples
#' "Example to follow"
kldivergence_shift <- function(type2freq_1,
                               type2freq_2,
                               base = 2L,
                               reference_value = 0,
                               normalization = "variation"){
  
  ### add checks on base
  
  ### add checks on type2freq_1 and type2freq_2 same checks in shift
  
  symmetric_difference <- function(x, y){
    unique(c(setdiff(x, y), setdiff(y, x)))
  }
  
  if(length(symmetric_difference(type2freq_1$word, type2freq_2$word)) > 0){
    message(cat("There are types that appear in either type2freq_1 or",
            "\ntype2freq_2 but not the other: the KL divergence is not",
            "well defined"))
    stop_quietly()
  }
  
  # Get relative frequencies
  type2score_1 <- get_relative_frequency(type2freq_1)
  names(type2score_1) <- c("word", "score_1")
  type2score_2 <- get_relative_frequency(type2freq_2)
  names(type2score_2) <- c("word", "score_2")
  
  # Get surprisal scores
  type2score_1$score_1 <- type2score_1$score_1 * -1 * log(type2score_1$score_1, base)
  type2score_2$score_2 <- type2score_1$score_1 * -1 * log(type2score_1$score_1, base)
  
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


#' Jensen-Shannon Divergence Shift
#' 
#' Shift object for calculating the Jensen-Shannon divergence (JSD) between two systems
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
#' "Example to follow"
jsdivergence_shift <- function(type2freq_1,
                               type2freq_2,
                               weight_1 = 0.5,
                               weight_2 = 0.5,
                               base = 2L,
                               alpha = 1,
                               reference_value = 0,
                               normalization = "variation"){
  
  # check that weigths sum to 1.  
  if((weight_1 + weight_2) != 1){
    stop(sprintf("weight_1 and weight_2 do not sum to 1. They sum to %s.", (weight_1 + weight_2)))
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


