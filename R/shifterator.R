#' Shift
#' 
#' Shift object for calculating weighted scores of two systems of types, and the shift between them
#'
#' @param type2freq_1 A data.frame containing words and their frequencies.
#' @param type2freq_2 A data.frame containing words and their frequencies.
#' @param type2score_1 Optional. A lexicon containing 2 columns. The first column the words and the second column the word score.
#' @param type2score_2 Optional. A lexicon containing 2 columns. The first column the words and the second column the word score.
#' @param reference_value Optional. String or numeric. The reference score to use 
#' to partition scores into two different regimes. If 'average', uses the average 
#' score according to type2freq_1 and type2score_1. If None and a lexicon is 
#' selected for type2score, uses the respective middle point in that lexicon's scale. 
#' Otherwise if NULL, uses zero as the reference point.
#' @param handle_missing_scores Optional. Default value: "error". If "error", throws an error 
#' whenever a word has a score in one score dictionary but not the other. If "exclude", 
#' excludes any word that is missing a score in one score dictionary from all word shift
#' calculations, regardless if it may have a score in the other dictionary. If "adopt" 
#' and the score is missing in one dictionary, then uses the score from the other 
#' dictionary if it is available
#' @param stop_lens Optional. Iterable of 2-tuples. Denotes intervals of scores 
#' that should be excluded from word shifts calculations. Types with scores in 
#' this range will be excluded from word shift calculations. See details for 
#' more information.
#' @param stop_words Optional. A string that contains words that should be excluded 
#' from word shifts calculations.
#' @param normalization Optional. Default value: "variation". If 'variation', normalizes
#'  shift scores so that the sum of their absolute values sums to 1. If 'trajectory', 
#'  normalizes them so that the sum of shift scores is 1 or -1. The trajectory 
#'  normalization cannot be applied if the total shift score is 0, so scores are 
#'  left unnormalized if the total is 0 and 'trajectory' is specified.
#'
#' @return Returns a list object of class shift.
#' @keywords internal
#'
#' 
shift <- function(type2freq_1,
                  type2freq_2,
                  type2score_1 = NULL,
                  type2score_2 = NULL,
                  reference_value = NULL,
                  handle_missing_scores = "error",
                  stop_lens = NULL,
                  stop_words = NULL,
                  normalization = "variation"){ 

  ## Check names on type2freq and set them to word and freq
  ## check handle_missing_scores on valid entries
  
  
  ## Set type2score dictionaries and set the column names to word and score
  # get_score_dictionary not needed if a dictionary is supplied. 
  # dictionary should be gotten via textdata or a download.
  if(!is.null(type2score_1) & !is.null(type2score_2)) {
    type2score_1 <- stats::setNames(type2score_1, c("word", "score_1"))
    type2score_2 <- stats::setNames(type2score_2, c("word", "score_2"))
    if(!identical(type2score_1, type2score_2)) {
      show_score_diffs <- TRUE 
      } else {
        show_score_diffs <- FALSE 
      }
  } else if(!is.null(type2score_1) & is.null(type2score_2)) {
      names(type2score_1) <- c("word", "score_1")
      type2score_2 <- stats::setNames(type2score_1, c("word", "score_1"))
      show_score_diffs <- FALSE
  } else if(is.null(type2score_1) & !is.null(type2score_2)) {
      names(type2score_2) <- c("word", "score_2")
      type2score_1 <- stats::setNames(type2score_2, c("word", "score_1"))
      show_score_diffs <- FALSE
  } else {
      type2score_1 <- data.frame(word = type2freq_1$word, score_1 = 1)
      type2score_2 <- data.frame(word = type2freq_2$word, score_2 = 1)
      show_score_diffs = FALSE
  }
  
  ## Preprocess words according to score rules, stop words, and stop lens
  handle_missing_scores <- handle_missing_scores
  
  # test stop lens if it is not null. It should be a numeric vector of length 2 
  # with the first value lower thatn the second value
  # set stop_lens
  if(is.null(stop_lens)) {
     stop_lens <- NULL
  } else {
    stop_lens <- stop_lens
  }
  
  # set stopwords
  if(is.null(stop_words)) {
    stop_words = ""
  } else {
    stop_words <- stop_words
  }
    
  # preprocess word scores
  preprocessed <- preprocess_words_scores(type2freq_1 = type2freq_1, 
                                          type2score_1 = type2score_1, 
                                          type2freq_2 = type2freq_2, 
                                          type2score_2 = type2score_2,
                                          stop_lens = stop_lens, 
                                          stop_words = stop_words, 
                                          handle_missing_scores = handle_missing_scores)
  

  ## Set reference value
  # If user supplies reference_value with instructions on how to use the dictionary
  # the reference value should be correct for the corresponding dictionary.
  # otherwise use the mean value of the dictionary?
  if(!is.null(reference_value)) {
    if(is.numeric(reference_value)){
      reference_value = reference_value
    } else if(reference_value == "average") {
      reference_value <- get_weighted_score(type2freq_1, type2score_1)
    } 
  } else {
    reference_value <- 0
  }
  

  # Get shift scores
  normalization = normalization
  
  # get_shift_scores(details=False)
  shift_scores <- get_shift_scores(all_system_scores = preprocessed$all_system_scores,
                                   reference_value = reference_value,
                                   normalization = normalization)
  
  
  
  
  out <- structure(list(filtered_types = preprocessed$filtered_types,
                        no_score_types = preprocessed$no_score_types,
                        adopted_score_types = preprocessed$adopted_score_types,
                        words_to_exclude = preprocessed$words_to_exclude,
                        words_in_stop_lens = preprocessed$words_in_stop_lens,
                        normalization = normalization,
                        shift_scores = shift_scores),
                   class = "shift"
                   )
  out
  
}


# return the weighted scores for a system.
# needed to calculate the reference value if the value is set to "average".
get_weighted_score <- function(type2freq, type2score){
  
  # Set the names correctly
  names(type2freq) <- c("word", "freq")
  names(type2score) <- c("word", "score")
  
  types <- intersect(type2freq$word, type2score$word)
  
  # Check we have a vocabulary to work with otherwise return 0
  if(length(types) == 0) {
    return(0)
  }
  
  # Get weighted score and total frequency
  total_freq <- sum(type2freq$freq[type2freq$word %in% types])
  total_score = sum(type2score$score[type2score$word %in% types] * type2freq$freq[type2freq$word %in% types])
  
  weighted_score = total_score / total_freq
  weighted_score
}

# return the summed shift component of the systems.
# input shift_scores data.frame
get_shift_components <- function(x) {
  pos_s_pos_p <- sum(ifelse(x$type2s_ref_diff > 0 & x$type2p_diff > 0, x$type2p_diff * x$type2s_ref_diff, 0))
  pos_s_neg_p <- sum(ifelse(x$type2s_ref_diff > 0 & x$type2p_diff <= 0, x$type2p_diff * x$type2s_ref_diff, 0))
  neg_s_pos_p <- sum(ifelse(x$type2s_ref_diff <= 0 & x$type2p_diff > 0, x$type2p_diff * x$type2s_ref_diff, 0))
  neg_s_neg_p <- sum(ifelse(x$type2s_ref_diff <= 0 & x$type2p_diff <= 0, x$type2p_diff * x$type2s_ref_diff, 0))
  pos_s <- sum(ifelse(x$type2s_diff > 0, x$type2p_avg * x$type2s_diff, 0))
  neg_s <- sum(ifelse(x$type2s_diff <= 0, x$type2p_avg * x$type2s_diff, 0))
  total <- sum(pos_s_pos_p, pos_s_neg_p, neg_s_pos_p, neg_s_neg_p, pos_s, neg_s)
  
  out <- data.frame(pos_s_pos_p, pos_s_neg_p, neg_s_pos_p, neg_s_neg_p, pos_s, neg_s, total)
  out  
}


# Average weighted score of system.
# Needed for main graph titel
# input shift_scores data.frane
avg_weighted_score <- function(x){
  avg_system_1 <- sum(x$freq_1 * x$score_1, na.rm = T) / sum(x$freq_1, na.rm = T)
  avg_system_2 <- sum(x$freq_2 * x$score_2, na.rm = T) / sum(x$freq_2, na.rm = T)
  
  weighted_scores <- data.frame(avg_system_1, avg_system_2)
  weighted_scores
}

