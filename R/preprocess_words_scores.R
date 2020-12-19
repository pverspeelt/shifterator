
preprocess_words_scores <- function(type2freq_1, 
                                    type2score_1, 
                                    type2freq_2 = NULL, 
                                    type2score_2 = NULL,
                                    stop_lens = NULL, 
                                    stop_words = NULL, 
                                    handle_missing_scores = "error") {
  
  all_words_system <- union(type2freq_1$word, type2freq_2$word)
  all_words_dict <- union(type2score_1$word, type2score_2$word)
  all_words <- union(all_words_system, all_words_dict)
  
  filtered_types <- ""
  adopted_score_types <- ""
  words_to_exclude <- ""
  words_in_stop_lens <- ""
  
  # merge systems and dictionaries for preprocessing
  all_systems <- merge(type2freq_1, type2freq_2, by = "word", all = TRUE)
  all_dictionaries <- merge(type2score_1, type2score_2, by = "word", all = TRUE)
  
  
  # words that need to be filtered based on the stop word list
  if(!is.null(stop_words)){
    filtered_types <- all_words_system[all_words_system %in% stop_words]
    # remove stop words from the systems
    if(length(filtered_types) >= 1) {
      all_systems <- all_systems[!all_systems$word %in% filtered_types, ]
      message(sprintf("There are %s words that are present in the stop word list.", length(filtered_types)),
              "\nThese will be removed.")
    }
  }
  
  # word does not have score in either dictionary. Remove these before handling missing scores
  no_score_types <- all_words_system[!all_words_system %in% all_words_dict]
  if(length(no_score_types) >= 1){
    all_systems <- all_systems[!all_systems$word %in% no_score_types, ]
    message(sprintf("There are %s words that are not present in a dictionary.", length(no_score_types)),
            "\nThese will be removed.")
  }
  
  # word does not appear in type2freq_1 or type2freq_2 but exists in one of the dictionaries
  # remove word that is not needed in the dictionaries
  # also remove words that are in the stop word list
  remove_before_handling_missing_scores <- c(filtered_types, all_words_dict[!all_words_dict %in% all_words_system])
  
  if(length(remove_before_handling_missing_scores) >= 1){
    all_dictionaries <- all_dictionaries[!all_dictionaries$word %in% remove_before_handling_missing_scores, ]
  }
  
  
  # merge systems and dictionaries 
  all_system_scores <- merge(all_systems, all_dictionaries, by = "word", all = TRUE)
  # Word has score in dict2 but not dict1 then add score from dict2 to dict1
  # Or word has score in dict1 but not dict2 then add score from dict1 to dict2
  
  if(handle_missing_scores == "adopt"){
    adopted_score_types <- all_system_scores$word[(is.na(all_system_scores$score_1) & 
                                                     !is.na(all_system_scores$score_2)) |
                                                    (!is.na(all_system_scores$score_1) & 
                                                       is.na(all_system_scores$score_2))]
    all_system_scores$score_1 <- ifelse(is.na(all_system_scores$score_1) & !is.na(all_system_scores$score_2), 
                                        all_system_scores$score_2, 
                                        all_system_scores$score_1)
    all_system_scores$score_2 <- ifelse(!is.na(all_system_scores$score_1) & is.na(all_system_scores$score_2), 
                                        all_system_scores$score_1, 
                                        all_system_scores$score_2)
    
  } else if(handle_missing_scores == "error"){
    if(sum(is.na(all_system_scores$score_1) | is.na(all_system_scores$score_2)) != 0) {
      check <- sum((is.na(all_system_scores$score_1) | is.na(all_system_scores$score_2)))
      message(sprintf("There are %s words that have a frequency but no score in the dictionary.", check),
              "\nFunction stopped. You might want to to use 'adopt' or 'exclude' in handle_missing_scores option.")
      
      # stop processing
      stop_quietly()
    }
    
  } else if(handle_missing_scores == "exclude"){
    if(sum(is.na(all_system_scores$score_1) | is.na(all_system_scores$score_2)) != 0){
      words_to_exclude <- all_system_scores$word[is.na(all_system_scores$score_1) | is.na(all_system_scores$score_2)]
      if(length(words_to_exclude) >= 0){
        message(sprintf("There are %s words excluded from the calculations", length(words_to_exclude)))
      } else {
        message("No words are excluded from the calculations")
      }
      all_system_scores <- all_system_scores[!all_system_scores$word %in% words_to_exclude, ]
    }
  }
  
  # create error all_system_scores$score_2[1] <- 0.8
  
  # Exclude words based on stop lens
  if(!is.null(stop_lens)){
    if(sum(!(stop_lens[1] <= all_system_scores$score_1 & all_system_scores$score_1 <= stop_lens[2]) ==
           (stop_lens[1] <= all_system_scores$score_2 & all_system_scores$score_2 <= stop_lens[2]))){
      
      incorrect_stop_lens <- all_system_scores$word[
        ((stop_lens[1] <= all_system_scores$score_1 & all_system_scores$score_1 <= stop_lens[2]) ==
           (stop_lens[1] <= all_system_scores$score_2 & all_system_scores$score_2 <= stop_lens[2])) == FALSE]
      
      message(sprintf("Stop_lens %s - %s cannot be applied consistently.", stop_lens[1], stop_lens[2]),
              "\nOne or more word scores from one dictionary fall within the stop lens,", 
              "\nbut the same word from the other dictionary does not.",
              sprintf("\nAn example is this word: %s", incorrect_stop_lens[1]))
      
      # stop processing    
      stop_quietly()
    } else {
      words_in_stop_lens <- all_system_scores$word[
        (stop_lens[1] <= all_system_scores$score_1 & all_system_scores$score_1 <= stop_lens[2]) &
          (stop_lens[1] <= all_system_scores$score_2 & all_system_scores$score_2 <= stop_lens[2])]
      
      all_system_scores <- all_system_scores[!all_system_scores$word %in% words_in_stop_lens, ]
    }
  }
  
  out <- list(filtered_types = filtered_types,
              no_score_types = no_score_types,
              adopted_score_types = adopted_score_types,
              words_to_exclude = words_to_exclude,
              words_in_stop_lens = words_in_stop_lens,
              all_system_scores = all_system_scores)
  out
}

