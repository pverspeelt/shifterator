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
#' "Example to follow."
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
  
  if(!is.null(stop_words) && class(stop_words) != "character"){
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
#' @inheritParams shift 
#'
#' @return Returns a list object of class shift.
#' @family shifts
#' @export
#'
#' @examples
#' "Example to follow."
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
#' "example to follow"
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
  
  if(alpha == 1 & reference_value == 0){
    jsd_out$all_pos_contributions <- TRUE
  } else {
    jsd_out$all_pos_contributions <- FALSE 
  }
  
  jsd_out
}


