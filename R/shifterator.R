shift <- function(type2freq_1,
                  type2freq_2,
                  type2score_1 = NULL,
                  type2score_2 = NULL,
                  reference_value = NULL,
                  handle_missing_scores = "error",
                  stop_lens = NULL,
                  stop_words = NULL,
                  normalization = "variation"){ 

  ## Check names om type2freq and set them to word and freq
  ## check handle_missing_scores on valid entries
  
  
  ## Set type2score dictionaries and set the column names to word and score
  # get_score_dictionary not needed if a dictionary is supplied. 
  # dictionary should be gotten via textdata or a download.
  if(!is.null(type2score_1) & !is.null(type2score_2)) {
    type2score_1 <- setNames(type2score_1, c("word", "score"))
    type2score_2 <- setNames(type2score_2, c("word", "score"))
    if(!identical(type2score_1, type2score_2)) {
      show_score_diffs <- TRUE 
      } else {
        show_score_diffs <- FALSE 
      }
  } else if(!is.null(type2score_1) & is.null(type2score_2)) {
      type2score_1 <- setNames(type2score_1, c("word", "score"))
      type2score_2 <- type2score_1
      show_score_diffs <- FALSE
  } else if(is.null(type2score_1) & !is.null(type2score_2)) {
      type2score_2 <- setNames(type2score_2, c("word", "score"))
      type2score_1 <- type2score_2
      show_score_diffs <- FALSE
  } else {
      type2score_1 <- data.frame(type2freq_1$word, score = 1)
      type2score_2 <- data.frame(type2freq_2$word, score = 1)
      show_score_diffs = FALSE
  }
  
  ## Preprocess words according to score rules, stop words, and stop lens
  handle_missing_scores <- handle_missing_scores
  
  # set stop_lens
  if(is.null(stop_lens)) {
     stop_lens <- ""
  } else {
    stop_lens <- stop_lens
  }
  
  # set stopwords
  if(is.null(stop_words)) {
    stop_words = data.frame()
  } else {
    stop_words <- stop_words
  }
    
  # preprocess word scores
  
  
  
  ## Set reference value
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
  
  
  out <- structure(list(type2score_1 = type2score_1,
                        type2score_2 = type2score_2,
                        reference_value = reference_value,
                        show_score_diffs = show_score_diffs,
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