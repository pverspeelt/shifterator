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
  
  for(word_to_process in types){
    if(sum(type2freq_1$word == word_to_process) == 0){
      type2freq_1 <- rbind(type2freq_1, list(word = word_to_process, freq = 0))
    } else if(sum(type2freq_2$word == word_to_process) == 0){
      type2freq_2 <- rbind(type2freq_2, list(word = word_to_process, freq = 0))
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


