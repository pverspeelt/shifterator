
preprocess_words_scores <- function(type2freq_1, 
                                    type2score_1, 
                                    type2freq_2 = NULL, 
                                    type2score_2 = NULL,
                                    stop_lens = NULL, 
                                    stop_words = NULL, 
                                    handle_missing_scores = "error") {
  

    # Filters stop words according to a list of words or stop lens on the scores
    # Parameters
    # ----------
    # type2freq_1, type2freq_2: dict
    #     Keys are types, values are frequencies of those types
    # type2score_1, type2freq_2: dict
    #     Keys are types, values are scores associated with those types
    # stop_lens: iteratble of 2-tuples
    #     Denotes intervals that should be excluded from word shifts
    # stop_words: iterable
    #     Denotes words that should be excluded from word shifts
    # handle_missing_scores_scores
    #     If 'error', throws an error whenever a word has a score in one score
    #     dictionary but not the other. If 'exclude', excludes any word that is
    #     missing a score in one score dictionary from all word shift
    #     calculations, regardless if it may have a score in the other dictionary.
    #     If 'adopt' and the score is missing in one dictionary, then uses the
    #     score from the other dictionary if it is available

  ts_1 <- union(type2freq_1$word, type2score_1$word)
  ts_2 <- union(type2freq_2$word, type2score_2$word)
  ts <- union(ts_1, ts_2)

  type2freq_1_new <- list() # data.frame(word = character(0), freq = numeric(0))
  type2score_1_new <- list() # data.frame(word = character(0), score = numeric(0))
  type2freq_2_new <- list() # data.frame(word = character(0), freq = numeric(0))
  type2score_2_new <- list()# data.frame(word = character(0), score = numeric(0))
  adopted_score_types <- character(0)
  no_score_types <- character(0)
  filtered_types <- character(0)
  
  # process the words
  for (word_to_process in ts) {
    # Exclude words specified by stop words
    if(sum(stop_words == word_to_process) == 1) {
      filtered_types <- c(filtered_types, word_to_process)
      next
    } 
    # Handle words with missing scores before excluding based on stop lens
    if(sum(type2score_1$word == word_to_process) == 1){
      score_1 <- type2score_1$score[type2score_1$word == word_to_process]
    } else {
      score_1 <- NA_real_
    }
    if(sum(type2score_2$word == word_to_process) == 1){
      score_2 <- type2score_2$score[type2score_2$word == word_to_process]
    } else {
      score_2 <- NA_real_
    }
    # Word does not have score in either dictionary
    if(sum(type2score_1$word == word_to_process) == 0 & sum(type2score_2$word == word_to_process) == 0){
      no_score_types <- c(no_score_types, word_to_process)
      next
    } else if(sum(type2score_1$word == word_to_process) == 0 & sum(type2score_2$word == word_to_process) == 1){
      # Word has score in dict2 but not dict1
        if(handle_missing_scores == "adopt"){
          score_1 <- type2score_2$score[type2score_2$word == word_to_process]
          score_2 <- type2score_2$score[type2score_2$word == word_to_process]
          adopted_score_types <- c(adopted_score_types, word_to_process)
        } else if(handle_missing_scores == "error"){
            stop(glue("Word has freq but no score in type2score_1: {word_to_process}"))
        } else if(handle_missing_scores == "exclude"){
          no_score_types <- c(no_score_types, word_to_process)
          next
        }
     
    } else if(sum(type2score_1$word == word_to_process) == 1 & sum(type2score_2$word == word_to_process) == 0){
    # Word has score in dict1 but not dict2
        if(handle_missing_scores == "adopt"){
          score_1 <- type2score_1$score[type2score_1$word == word_to_process]
          score_2 <- type2score_1$score[type2score_1$word == word_to_process]
          adopted_score_types <- c(adopted_score_types, word_to_process)
        } else if(handle_missing_scores == "error"){
          stop(glue("Word has freq but no score in type2score_2: {word_to_process}"))
        } else if(handle_missing_scores == "exclude"){
          no_score_types <- c(no_score_types, word_to_process)
          next
        }
    } else {
        score_1 <- type2score_1$score[type2score_1$word == word_to_process]
        score_2 <- type2score_2$score[type2score_2$word == word_to_process]
    }
    
    # Exclude words based on stop lens
    filter_word <- FALSE
    if(is.null(stop_lens)){
      filter_word <- FALSE
    } else if((stop_lens[1] <= score_1 & score_1 <= stop_lens[2]) && (stop_lens[1] <= score_2 & score_2 <= stop_lens[2])){
    # Word is in stop lens
      filter_word <- TRUE
      # One score is in stop lens but the other is not
    } else if((stop_lens[1] <= score_1 & score_1 <= stop_lens[2]) || (stop_lens[1] <= score_2 & score_2 <= stop_lens[2])){
        stop(glue("stop_lens {stoplens} cannot be applied consistently.", 
                  " One word score falls within the stop lens while the other does not."))
    }
    
    if(filter_word == TRUE){
      filtered_types <- c(filtered_types, word_to_process)
      next
    }
    
    # Set words and freqs for words that pass all checks
    type2score_1_new[word_to_process] <- score_1
    if(sum(type2freq_1$word == word_to_process) == 1){
      type2freq_1_new[word_to_process] <- type2freq_1$freq[type2freq_1$word == word_to_process]
    } else {
      type2freq_1_new[word_to_process] = 0
    }
    
    type2score_2_new[word_to_process] = score_2
    if(sum(type2freq_2$word == word_to_process) == 1){
      type2freq_2_new[word_to_process] <- type2freq_2$freq[type2freq_2$word == word_to_process]
    } else {
      type2freq_2_new[word_to_process] = 0
    }
    
  }
  
  # Update types to only be those that made it through all filters
  final_types <- setdiff(setdiff(ts, filtered_types), no_score_types)
  type2freq_1_new <- data.frame(word = names(type2freq_1_new), score = Reduce(c, type2freq_1_new))
  type2freq_2_new <- data.frame(word = names(type2freq_2_new), score = Reduce(c, type2freq_2_new))
  type2score_1_new <- data.frame(word = names(type2score_1_new), score = Reduce(c, type2score_1_new))
  type2score_2_new <- data.frame(word = names(type2score_2_new), score = Reduce(c, type2score_2_new))
  # return filtered word frequencies and scores. 
  out <- list(type2freq_1_new = type2freq_1_new,
              type2freq_2_new = type2freq_2_new,
              type2score_1_new = type2score_1_new,
              type2score_2_new = type2score_2_new,
              final_types = final_types,
              filtered_types = filtered_types,
              no_score_types = no_score_types,
              adopted_score_types = adopted_score_types)
  out
  
}
