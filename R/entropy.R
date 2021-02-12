#' Get Relative Frequency
#' 
#' Calculates the relative frequency (proportion) of each word in the data.frame.
#'
#' @param data A data.frame containing words and their counts.
#'
#' @return A data.frame containing words and their relative frequencies.
#' 
#' @keywords internal
#' 
get_relative_frequency <- function(data) {
  # if length(class(data)) > 1 do as.data.frame
  # data <- as.data.frame(data)
  data$score <- data[[2]] / sum(data[[2]])
  return(data[, c(1,3)])
}


#' Get Entropy Scores
#'
#' @param type2score_1 
#' @param type2score_2 
#' @param base The base for the logarithm when computing entropy scores.
#' @param alpha The parameter for the generalized Tsallis entropy. Setting 'alpha = 1'
#' recovers the Shannon entropy.
#'
#' @return A data.frame containing the entropy scores
#' 
#' @keywords internal
#' 
get_entropy_scores <- function(type2score_1,
                               type2score_2,
                               base = 2L,
                               alpha = 1){
  # need to merge the type2score data.frames to make sure all words are in both dictionaries.
  all_scores <- merge(type2score_1, type2score_2, by = "word", all = TRUE)
  all_scores$score_1[is.na(all_scores$score_1)] <- 0
  all_scores$score_2[is.na(all_scores$score_2)] <- 0
  all_scores$score_1 <- get_entropy_word_scores(all_scores$score_1, base, alpha)
  all_scores$score_2 <- get_entropy_word_scores(all_scores$score_2, base, alpha)
  
  all_scores
}



#' Get Entropy Word Scores
#' 
#' Calculates the generalized Tsallis entropy scores.
#'
#' @param score The relative frequency. 
#' @param base The base for the logarithm when computing entropy scores.
#' @param alpha The parameter for the generalized Tsallis entropy. Setting 'alpha = 1'
#' recovers the Shannon entropy.
#'
#' @return The entropy score.
#'  
#' @keywords internal
#' 
get_entropy_word_scores <- function(score, 
                               base = 2L, 
                               alpha = 1){
  
  if(alpha == 1){
    score_new <- ifelse(score > 0, -log(score, base), 0)
  } else if(alpha > 0){
    score_new <- ifelse(score > 0, score^(alpha - 1) / (alpha - 1), 0)
    } else {
      score_new <- 0
  }
  
  score_new
}


#' Get JS Divergence Scores
#' 
#' Calculates the contribution of the types in two systems to the Jensen-Shannon
#' divergence (JSD) between those systems
#'
#' @inheritParams get_entropy_scores
#' @inheritParams jsdivergence_shift
#'
#' @return A data.frame containing the js scores
#' 
#' @keywords internal
#' 
get_jsd_word_scores <- function(type2score_1,
                                type2score_2, 
                                weight_1, 
                                weight_2, 
                                base, 
                                alpha){
  
  all_scores <- merge(type2score_1, type2score_2, by = "word", all = TRUE)
  all_scores$score_1[is.na(all_scores$score_1)] <- 0
  all_scores$score_2[is.na(all_scores$score_2)] <- 0
  
  # calculate mixture
  all_scores$mixture <- weight_1 * all_scores$score_1 + weight_2 * all_scores$score_2
  
  if(alpha == 1){
    all_scores$score_1 <- ifelse(all_scores$score_1 > 0, 
                                 weight_1 * (log(all_scores$mixture, base) - log(all_scores$score_1, base)), 
                                 weight_1 * log(all_scores$mixture, base))
    all_scores$score_2 <- ifelse(all_scores$score_2 > 0, 
                                 weight_2 * (log(all_scores$score_2, base) - log(all_scores$mixture, base)), 
                                 weight_2 * log(all_scores$mixture, base))
  } else if(alpha > 0){
    all_scores$score_1 <- ifelse(all_scores$score_1 > 0, 
                                 weight_1 * (all_scores$mixture^(alpha - 1) - all_scores$score_1^(alpha - 1)) / (alpha - 1), 
                                 0)
    all_scores$score_2 <- ifelse(all_scores$score_2 > 0, 
                                 weight_1 * (all_scores$mixture^(alpha - 1) - all_scores$score_2^(alpha - 1)) / (alpha - 1), 
                                 0)
  } else {
    all_scores$score_1 <- 0
    all_scores$score_2 <- 0
  }
  
  all_scores
}
