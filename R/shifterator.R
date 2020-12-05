shift <- function(type2freq_1,
                  type2freq_2,
                  type2score_1 = NULL,
                  type2score_2 = NULL,
                  reference_value = NULL,
                  handle_missing_scores = "error",
                  stop_lens = NULL,
                  stop_words = NULL,
                  normalization = "variation"){ 
  
  # Set type2score dictionaries
  if(!is.null(type2score_1) & !is.null(type2score_2)) {
    print("type2score_1 and type2score_2 are not NULL")
    # self.type2score_1, lex_ref = helper.get_score_dictionary(type2score_1)
    # self.type2score_2, _ = helper.get_score_dictionary(type2score_2)
    # _ in python means don't care about the returned value. (Aka dump it)
    # lex_ref is only relevant for which dictionary is used. 
    if(type2score_1 != type2score_2) {
      show_score_diffs <- TRUE 
      } else {
      show_score_diffs <- FALSE 
      }
  } else if(!is.null(type2score_1) & is.null(type2score_2)) {
      # self.type2score_1, lex_ref = helper.get_score_dictionary(type2score_1)
      # self.type2score_2 = self.type2score_1
      show_score_diffs <- FALSE
  } else if(is.null(type2score_1) & !is.null(type2score_2)) {
      # self.type2score_2, lex_ref = helper.get_score_dictionary(type2score_2)
      # self.type2score_1 = self.type2score_2
      show_score_diffs <- FALSE
  } else {
      # self.type2score_1 = {t: 1 for t in type2freq_1}
      # self.type2score_2 = {t: 1 for t in type2freq_2}
      show_score_diffs = FALSE
  }
  
  # Preprocess words according to score rules, stop words, and stop lens
  
  
  
  
  
  # Set reference value
  # lex_ref is used in the python version, but is this needed? 
  # If user supplies reference_value with instructions on how to use the dictionary
  # the reference value should be correct for the corresponding dictionary.
  # otherwise use the mean value of the dictionary?
  if(!is.null(reference_value)) {
    if(is.numeric(reference_value)){
      reference_value = reference_value
    } else 
      if(reference_value == "average") {
        reference_value <- get_weighted_score(type2freq_1, type2score_1) 
      } else 
    reference_value <- 0
  }
  


  
  
  
  # Get shift scores
  normalization = normalization
  # create get_shift_scores function
  # get_shift_scores(details=False)
  
  
  out <- structure(list(reference_value = reference_value,
                        normalization = normalization),
                   class = "shift"
                   )
  out
  
}



get_weighted_score <- function(type2freq_1, type2score_1){
  
  types <- intersect(type2freq_1$word, type2score_1$word)
  
  # Check we have a vocabulary to work with otherwise return 0
  if(length(types) == 0) {
    return(0)
  }
  
  # Get weighted score and total frequency
  total_freq <- sum(type2freq_1$freq[type2freq_1$word %in% types])
  total_score = sum(type2score_1$value[type2score_1$word %in% types])
    
  weighted_score = total_score / total_freq
  
}
