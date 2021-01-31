# function for creating the total contributions plot.
# This function will be called from the get_shift_graphs function.
create_total_contributions_plot <- function(top_shift_scores, 
                                            totals,
                                            text_names,
                                            norm_value,
                                            y_limits,
                                            show_score_diffs,
                                            all_pos_contributions,
                                            detailed = FALSE, 
                                            show_total){
  
  # Labels used in the total contributions plot
  shift_labels <- list(
    all_pos_neg = text_names[1], 
    all_pos_pos = text_names[2], 
    neg_s = "\u25BD",
    neg_s_neg_p = "-\u2193",
    neg_s_pos_p = "-\u2191",
    neg_total = text_names[1],
    pos_s = "\u25B3",
    pos_s_neg_p = "+\u2193",
    pos_s_pos_p = "+\u2191",
    pos_total = text_names[2],
    total = "\u03A3")
  
  
  bar_order <- get_bar_order(show_score_diffs, all_pos_contributions, 
                             detailed = detailed, show_total = show_total)
  
  # create a function of this and put it outside of the create? 
  # or use the create as 
  value <- numeric(0L)
  for(b in bar_order){
    if(b == "total"){
      value[b] <- totals[[b]]
      next
    } else if(b == "neg_total"){
      value[b] <- totals[["neg_s"]] + totals[["neg_s_pos_p"]] + totals[["pos_s_neg_p"]]
      next
    } else if(b == "pos_total"){
      value[b] <- totals[["pos_s"]] + totals[["neg_s_neg_p"]] + totals[["pos_s_pos_p"]]
      next
    } else if(b == "all_pos_pos"){
      value[b] <- totals[[b]]
    } else if(b == "all_pos_neg"){
      value[b] <- totals[[b]]
    } else {
      value[b] <- totals[[b]]
    }
    
  }
  
  rescale_factor <- rescale(top_shift_scores,
                           value,
                           norm_value,
                           all_pos_contributions)
  
  # Rescalue value with norm_value and max of total values 
  # so the scaling falls in the same range as the shift_scores.
  value <- value * rescale_factor
  
  # data frame for ggplot.
  plotting_data <- data.frame(labels = factor(unlist(shift_labels[bar_order]), 
                                              levels = (unlist(shift_labels[bar_order]))), 
                              value)
  
  # get the colours for the plot
  colour <- .score_colours[bar_order] 
  
  # build title
  title <- sprintf("Shift of %s vs %s", text_names[1], text_names[2])
  
  ggplot2::ggplot(plotting_data, 
                  ggplot2::aes(x = labels , y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = colour) +
    ggplot2::geom_text(ggplot2::aes(label = labels), size = 3, hjust = c("outward")) + 
    ggplot2::scale_y_continuous(label = scales::percent, limits = y_limits) +
    ggplot2::coord_flip() +
    main_theme() + 
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(color = NA),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1), 
                                                      hjust = 0.5)) +
    ggplot2::ggtitle(title)
  

}


# Gets which cumulative bars to show at the top of the graph given what level
# of detail is being specified
# returns a vector of strings indicating which cumulative bars to show.
get_bar_order <- function(show_score_diffs, 
                          all_pos_contributions,
                          detailed = FALSE, 
                          show_total = TRUE){
  
  if(detailed == TRUE){
    if(show_score_diffs == TRUE){
      bar_order <- c("neg_s",
                     "pos_s",
                     "neg_s_neg_p",
                     "neg_s_pos_p",
                     "pos_s_neg_p",
                     "pos_s_pos_p")
    } else {
      bar_order <- c("neg_s_neg_p", "neg_s_pos_p", "pos_s_neg_p", "pos_s_pos_p")
    }
  } else if(!all_pos_contributions == TRUE) {
    bar_order <- c("neg_total", "pos_total")
  } else {
    bar_order <- c("all_pos_pos", "all_pos_neg")
  } 
  
  if(show_total == TRUE){
    bar_order = c("total", bar_order)
  }
  
  bar_order  
}


# function for calculating the rescaling factor to get the total contributions 
# in line with the shift_scores.
rescale <- function(top_shift_scores,
                    value,
                    norm_value,
                    all_pos_contributions){
  
  c_p = top_shift_scores$type2p_diff * top_shift_scores$type2s_ref_diff / norm_value
  c_s = top_shift_scores$type2p_avg * top_shift_scores$type2s_diff / norm_value
  
  total_heights <- ifelse(!all_pos_contributions == TRUE | top_shift_scores$type2p_diff > 0, 
                          c_p + c_s, -1 * (c_p + c_s))
  label_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                          c_p + c_s, 
                          ifelse(abs(c_p) > abs(c_s), 
                                 c_p, 
                                 c_s))
  
  if(!all_pos_contributions == TRUE){
    max_bar_height <- max(abs(label_heights))
  } else {
    max_bar_height <- max(abs(total_heights))
  }
  
  rescale <- max_bar_height / max(abs(value)) 
  
  rescale
}
