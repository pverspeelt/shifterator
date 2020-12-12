get_shift_scores <- function(type2freq_1,
                             type2freq_2,
                             type2score_1,
                             type2score_2,
                             types,
                             reference_value,
                             normalization, 
                             details = FALSE) {
  
        # Calculates the type shift scores between the two systems
        # Parameters
        # ----------
        # details: boolean
        #     If true, returns each of the major components of each type's shift
        #     score, along with the overall shift scores. Otherwise, only returns
        #     the overall shift scores
        # Returns
        # -------
        # type2p_diff: dict
        #     If details is True, returns dict where keys are types and values are
        #     the difference in relatively frequency, i.e. p_i,2 - p_i,1 for type i
        # type2s_diff: dict,
        #     If details is True, returns dict where keys are types and values are
        #     the relative differences in score, i.e. s_i,2 - s_i,1 for type i
        # type2p_avg: dict,
        #     If details is True, returns dict where keys are types and values are
        #     the average relative frequencies, i.e. 0.5*(p_i,1+p_i,2) for type i
        # type2s_ref_diff: dict
        #     If details is True, returns dict where keys are types and values are
        #     relative deviation from reference score, i.e. 0.5*(s_i,2+s_i,1)-s_ref
        #     for type i
        # type2shift_score: dict
        #     Keys are types and values are shift scores. The overall shift scores
        #     are normalized according to the `normalization` parameter of the
        #     Shift object
 
  # s_avg_ref  <- reference_value
  # Get total frequencies
  total_freq_1 <- sum(type2freq_1$freq[type2freq_1$word %in% types])
  total_freq_2 <- sum(type2freq_2$freq[type2freq_2$word %in% types])
  
  # Get relative frequency of types in both systems
  type2freq_1$type2p_1 <- ifelse(type2freq_1$word %in% types, type2freq_1$freq / total_freq_1, 0)
  type2freq_2$type2p_2 <- ifelse(type2freq_2$word %in% types, type2freq_2$freq / total_freq_2, 0)
  
  # Calculate shift components
  type2p_avg = list()
  type2p_diff = list()
  type2s_diff = list()
  type2s_ref_diff = list()
  type2shift_score = list()
  
  for(type_to_process in types){
    type2p_avg[type_to_process] <- 0.5 * (type2freq_1$type2p_1[type2freq_1$word == type_to_process] + 
                                            type2freq_2$type2p_2[type2freq_2$word == type_to_process])
    
    type2p_diff[type_to_process] <- type2freq_2$type2p_2[type2freq_2$word == type_to_process] - 
                                      type2freq_1$type2p_1[type2freq_1$word == type_to_process]
    
    type2s_diff[type_to_process] <- type2score_2$score[type2score_2$word == type_to_process] - 
                                      type2score_1$score[type2score_1$word == type_to_process]
    
    type2s_ref_diff[type_to_process] <- 0.5 * (type2score_2$score[type2score_2$word == type_to_process] + 
                                          type2score_1$score[type2score_1$word == type_to_process] - reference_value)
    
    type2shift_score[type_to_process] <- type2p_diff[[type_to_process]] * type2s_ref_diff[[type_to_process]] +
                                          type2s_diff[[type_to_process]] * type2p_avg[[type_to_process]]
  
  }

  # Normalize the total shift scores
  total_diff <- sum(unlist(type2shift_score))
  
  if(normalization == "variation"){
    norm_value <- sum(abs(unlist(type2shift_score)))
  } else if(normalization == "trajectory" && total_diff != 0){
    norm_value <- abs(total_diff)
  } else {
    norm_value = 1
  }
  
  type2shift_score <- lapply(type2shift_score, function(x) x / norm_value)
  
  type2_shift_scores <- data.frame(word = types, 
                                   type2p_diff = Reduce(c, type2p_diff),
                                   type2s_diff = Reduce(c, type2s_diff),
                                   type2p_avg = Reduce(c, type2p_avg),
                                   type2s_ref_diff = Reduce(c, type2s_ref_diff),
                                   type2shift_score = Reduce(c, type2shift_score),
                                   stringsAsFactors = FALSE)
  
  # type2p_diff = data.frame(word = names(type2p_diff), freq_diff = Reduce(c, type2p_diff))
  # type2s_diff = data.frame(word = names(type2s_diff), score_diff = Reduce(c, type2s_diff))
  # type2p_avg = data.frame(word = names(type2p_avg), freq_avg = Reduce(c, type2p_avg))
  # type2s_ref_diff = data.frame(word = names(type2s_ref_diff), score_deviation = Reduce(c, type2s_ref_diff))
  # type2shift_score = data.frame(word = names(type2shift_score), score_norm = Reduce(c, type2shift_score))
  # 
  
  ### Q. return as a list of data.frames, a list of lists, or just 1 data.frame?
  ### return only type2shift_score if details is FALSE. 
  ### Q. will this lead to double calculations down the line?
  # out <- list(type2p_diff = type2p_diff,
  #             type2s_diff = type2s_diff,
  #             type2p_avg = type2p_avg,
  #             type2s_ref_diff = type2s_ref_diff,
  #             type2shift_score = type2shift_score
  # )
  # 
  
  type2_shift_scores
  
}
