# internal stop function to stop the process but not show the error:
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


# theme for the main plot
main_theme <- function(...){
  ggplot2::theme_grey() +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1, colour = "grey"), 
                   panel.grid = ggplot2::element_line(colour = "grey"),
                   panel.grid.minor.x = ggplot2::element_line(color = NA),
                   panel.grid.minor.y = ggplot2::element_line(color = NA),
                   panel.background = ggplot2::element_rect(fill = NA),
                   axis.title.y = ggplot2::element_blank())
}

# theme for the text size inset plot
text_size_theme <- function(...){
  ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.5), 
                                                      lineheight = .9,
                                                      face = "bold.italic", 
                                                      colour = "black"))
}

# use in cumulative contribution plot
scientific_to_10 <- function(x){
  parse(text=gsub("1e\\+*", " 10^", scales::scientific_format()(x))) 
}


#' Get bar colours for plotting
#' 
#' Returns the component colours of each type's contribution bars.
#'
#' @param top_shift_scores top n values of the shift_scores data.frame from a 
#' shift object
#' @param all_pos_contributions Defaults to FALSE when calling get_shift_graphs.
#'
#' @return Returns a list with 3 values. The values are lists of the colors 
#' to assign to the p_diff and s_diff components respectively. If just the 
#' overall contributions are being shown in a simple (not detailed) shift graph, 
#' then the "total" colours are used.
#' 
#' @keywords internal
#' 
get_bar_colours <- function(top_shift_scores, all_pos_contributions){
  c_total = top_shift_scores$type2p_diff * top_shift_scores$type2s_ref_diff + 
    top_shift_scores$type2p_avg * top_shift_scores$type2s_diff
  # Get total contribution colors
  if(!all_pos_contributions == TRUE){
    bar_colours_total <- ifelse(c_total > 0, 
                                .score_colours[["pos_total"]], 
                                .score_colours[["neg_total"]])
  } else {
    bar_colours_total <- ifelse(top_shift_scores$type2p_diff > 0, 
                                .score_colours[["all_pos_pos"]], 
                                .score_colours[["all_pos_neg"]])
  }
  # Get p_diff * s_ref_diff comp colors
  bar_colours_p <- ifelse(top_shift_scores$type2s_ref_diff > 0 & top_shift_scores$type2p_diff > 0, 
                          .score_colours[["pos_s_pos_p"]], 
                          "")
  bar_colours_p <- ifelse(top_shift_scores$type2s_ref_diff > 0 & top_shift_scores$type2p_diff <= 0, 
                          .score_colours[["pos_s_neg_p"]], 
                          bar_colours_p)
  bar_colours_p <- ifelse(top_shift_scores$type2s_ref_diff <= 0 & top_shift_scores$type2p_diff > 0, 
                          .score_colours[["neg_s_pos_p"]], 
                          bar_colours_p)
  bar_colours_p <- ifelse(top_shift_scores$type2s_ref_diff <= 0 & top_shift_scores$type2p_diff <= 0, 
                          .score_colours[["neg_s_neg_p"]], 
                          bar_colours_p)
  
  # Get s_diff comp colors
  bar_colours_s <- ifelse(top_shift_scores$type2s_diff > 0, 
                          .score_colours[["pos_s"]], 
                          .score_colours[["neg_s"]])
  
  bar_colours <- list(bar_colours_total = bar_colours_total,
                      bar_colours_p = bar_colours_p,
                      bar_colours_s = bar_colours_s)
  
  bar_colours
}

.score_colours <- list(
  all_pos_neg = "#9E75B7",
  all_pos_pos = "#FECC5D",
  neg_s = "#9E75B7",
  neg_s_neg_p = "#C4CAFC",
  neg_s_pos_p = "#2F7CCE",
  neg_total = "#9E75B7",
  pos_s = "#FECC5D",
  pos_s_neg_p = "#FDFFD2",
  pos_s_pos_p = "#FFFF80",
  pos_total = "#FECC5D",
  total = "#707070")
