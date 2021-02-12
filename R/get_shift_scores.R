get_shift_scores <- function(all_system_scores,
                             reference_value,
                             normalization) {
  
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

  # Calculate shift components
  # merge all word and score data.frames
  type2_shift_scores <- all_system_scores
  
  # Get total frequencies
  total_freq_1 <- sum(type2_shift_scores$freq_1, na.rm = TRUE)
  total_freq_2 <- sum(type2_shift_scores$freq_2, na.rm = TRUE)
  
  # Get relative frequency of types in both systems
  type2_shift_scores$type2p_1 <- ifelse(is.na(type2_shift_scores$freq_1), 0, type2_shift_scores$freq_1 / total_freq_1)
  type2_shift_scores$type2p_2 <- ifelse(is.na(type2_shift_scores$freq_2), 0, type2_shift_scores$freq_2 / total_freq_2)
  type2_shift_scores$type2p_avg <- 0.5 * (type2_shift_scores$type2p_1 + type2_shift_scores$type2p_2)
  type2_shift_scores$type2p_diff <- type2_shift_scores$type2p_2 - type2_shift_scores$type2p_1
  type2_shift_scores$type2s_diff <- type2_shift_scores$score_2 - type2_shift_scores$score_1
  type2_shift_scores$type2s_ref_diff <- 0.5 * (type2_shift_scores$score_2 + type2_shift_scores$score_1 - reference_value)
  type2_shift_scores$type2shift_score <- (type2_shift_scores$type2p_diff * type2_shift_scores$type2s_ref_diff) +
                                         (type2_shift_scores$type2s_diff * type2_shift_scores$type2p_avg)
  
  
  # Normalize the total shift scores
  total_diff <- sum(type2_shift_scores$type2shift_score, na.rm = TRUE)
  
  if(normalization == "variation"){
    norm_value <- sum(abs(type2_shift_scores$type2shift_score), na.rm = TRUE)
  } else if(normalization == "trajectory" && total_diff != 0){
    norm_value <- abs(total_diff)
  } else {
    norm_value <- 1
  }
  
  # Normalize based on norm_value
  type2_shift_scores$type2shift_score <- type2_shift_scores$type2shift_score / norm_value
  
  shift_scores_out <- list(shift_scores = type2_shift_scores,
                           difference = total_diff,
                           norm_value = norm_value)
  
  shift_scores_out
}
