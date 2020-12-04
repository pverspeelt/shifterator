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
  lex_ref <- NULL
  if(!is.null(reference_value)) {
    if(reference_value == "average") {
      # create get_weighted_score function
      reference_value <- "average"
      # reference_value <- get_weighted_score(self.type2freq_1, self.type2score_1) 
    } else 
      reference_value <- reference_value
  } else 
    if(!is.null(lex_ref)) {
      reference_value <- lex_ref
    } else 
      reference_value <- 0
    

  
  
  
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
