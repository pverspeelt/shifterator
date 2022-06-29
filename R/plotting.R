#' Generate a collection of shift graphs
#' 
#' When called this will construct word shift graphs.
#'
#' @param x A shift object.
#' @param text_names The names of the text to compare. Defaults to "Text 1" and "Text 2".
#' @param top_n Integer value. Number of words to display in the main graph. Defaults to 50
#' @param detailed Logical. Defaults to "FALSE". By default, the shifts display 
#' the overall contributions, rather than the detailed component contributions. 
#' By setting this value to "TRUE" the full details of the shifts are visualized.
#'
#' @return Returns the plots of all the shift graphs.
#' @export
#'
#' @examples
#' \dontrun{
#' library(shifterator)
#' library(quanteda)
#' library(quanteda.textstats)
#' library(dplyr)
#' 
#' reagan <- corpus_subset(data_corpus_inaugural, President == "Reagan") %>% 
#'   tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% # to move from classes frequency, textstat, and data.frame to data.frame
#' select(feature, frequency) 
#' 
#' bush <- corpus_subset(data_corpus_inaugural, President == "Bush" & FirstName == "George W.") %>% 
#' tokens(remove_punct = TRUE) %>% 
#' dfm() %>% 
#' textstat_frequency() %>% 
#' as.data.frame() %>% 
#' select(feature, frequency)
#' 
#' prop <- proportion_shift(reagan, bush)
#' 
#' # plot the proportion shift graph
#' get_shift_graphs(prop_shift, text_names = c("reagan", "bush"))
#' }
get_shift_graphs <- function(x, 
                             text_names = c("Text 1", "Text 2"),
                             top_n = 50L,
                             detailed = FALSE){
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", 
         call. = FALSE)
  }
  
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("patchwork is needed for this function to work. Install it via install.packages(\"patchwork\")", 
         call. = FALSE)
  }
  
  if(length(class(x)) > 1){
    stop("Please supply a shift object.", 
         call. = FALSE)
  }
  if(class(x) != "shift") {
    stop("Please supply a shift object.", 
         call. = FALSE)
  }  
  
  if(!is.numeric(top_n) || top_n < 1){
    stop(sprintf("Please use a positive integer value for top_n. You supplied: %s",
                 top_n), 
                 call. = FALSE)
  }
  
  # set all_pos_contributions
  if(exists("all_pos_contributions", where = x)){
    all_pos_contributions <- x$all_pos_contributions
  } else {
    all_pos_contributions <- FALSE
  }
  
  if(exists("show_total", where = x)){
    show_total <- x$show_total
  } else {
    show_total <- TRUE
  }
  
  
  # get top_n shift scores
  top_shift_scores <- utils::head(x$shift_scores[order(abs(x$shift_scores$type2shift_score), 
                                                       decreasing = TRUE) , , drop = FALSE], 
                                  top_n)
  
  # set main colours to supply totals. later check on how to show details.
  bar_colours <- get_bar_colours(top_shift_scores, all_pos_contributions)
  
  # check which main plot needs to be plotted
  # adjust the height of the patchwork plot when detailed == TRUE
  if(detailed == TRUE){
    
    bar_dimensions <-  bar_dimension(top_shift_scores, x$norm_value, all_pos_contributions)

    main_plot <- create_detailed_main_plot(top_shift_scores = top_shift_scores, 
                                           top_n = top_n,
                                           all_pos_contributions = all_pos_contributions,
                                           bar_dimensions = bar_dimensions)
    
    heights <- c(2, 4, 4, 4)
    
  } else {

    main_plot <- create_main_plot(top_shift_scores = top_shift_scores, 
                                  top_n = top_n,
                                  bar_colours = bar_colours$bar_colours_total,
                                  all_pos_contributions = all_pos_contributions)
    
    heights <- c(1, 4, 4, 4)
  }
  
  # calculate the totals for the total contributions plot
  totals <- get_shift_components(x$shift_scores, all_pos_contributions)
  
  total_plot <- create_total_contributions_plot(top_shift_scores = top_shift_scores,
                                                totals = totals, 
                                                show_score_diffs = x$show_score_diffs,
                                                text_names = text_names,
                                                norm_value = x$norm_value,
                                                all_pos_contributions = all_pos_contributions,
                                                detailed = detailed,
                                                show_total = show_total)
  
  # inset plots
  cum_contribution_plot <- cumulative_contribution_plot(x, top_n)
  text_size_plot <- create_text_size_plot(x, text_names)
  
  # add correct title to total
  if(exists("avg_score", where = x)){
    total_plot_title <- bquote(atop(.(text_names[1])*":" ~ Phi[avg] ~" = " ~  .(round(x$avg_score[[1]], 2)),
                             .(text_names[2])*":" ~ Phi[avg] ~" = " ~  .(round(x$avg_score[[2]], 2))))
    
  } else {

    total_plot_title <- sprintf("Shift of %s vs %s", text_names[1], text_names[2])
  }
  
  total_plot <- total_plot + ggplot2::ggtitle(total_plot_title)
  
  
  # patchwork parts
  # syncs ylims over total and main plots
  # puts insets on combined plot
  layout <- c(patchwork::area(1, 1, 1, 3),
              patchwork::area(2, 1, 4, 3))
  
  combined_plot <- total_plot + main_plot + patchwork::plot_layout(design = layout, heights = heights)
  
  plot_ranges_y <- c(ggplot2::ggplot_build(combined_plot[[1]])$layout$panel_scales_y[[1]]$range$range,
                     ggplot2::ggplot_build(combined_plot[[2]])$layout$panel_scales_y[[1]]$range$range)
  
  combined_plot <- combined_plot & 
    ggplot2::scale_y_continuous(label = scales::percent, 
                                limits = c(min(plot_ranges_y), max(plot_ranges_y)))
  
  combined_plot + 
    inset_element(cum_contribution_plot, 
                  left = 0.01, 
                  bottom = 0.05, 
                  right = 0.3, 
                  top = 0.35 
    ) +
    inset_element(text_size_plot, 
                  left = 0.8, 
                  bottom = 0.05, 
                  right = 0.975,
                  top = 0.25
    ) 
  

}

# main plot if detailed = FALSE
create_main_plot <- function(top_shift_scores, top_n, bar_colours, all_pos_contributions){
  
  # set main plotting params
  if(all_pos_contributions == TRUE){
    top_shift_scores$type2shift_score <- ifelse(top_shift_scores$type2p_diff <= 0,
                                                -1 * top_shift_scores$type2shift_score,
                                                top_shift_scores$type2shift_score)
  }
  
  # set hjust paramaters ("outward" is to close to the edge of the bars)
  shift_hj <- ifelse(top_shift_scores$type2shift_score >= 0, -0.2, 1.2)
  

  # create x axis ordering, labels and breaks
  top_shift_scores$ordering <- 1:top_n
  x_label_breaks <- seq(0, top_n, 5)
  x_label_breaks[1] <- 1
    
  main_plot <- ggplot2::ggplot(top_shift_scores, 
                               ggplot2::aes(x = .data$ordering, 
                                            y = .data$type2shift_score))
                               
  main_plot <- main_plot + 
    ggplot2::geom_col(fill = bar_colours) + 
    ggplot2::geom_text(ggplot2::aes(label = .data$word), 
                       size = 3, 
                       hjust = shift_hj) + 
    ggplot2::scale_x_reverse(breaks = x_label_breaks) + 
    ggplot2::coord_flip() +
    ggplot2::ylab(expression("Score shift" ~ delta * Phi [tau] * "(%)")) +
    main_theme() 
  
  main_plot
  
}


# text size plot
create_text_size_plot <- function(x, text_names){
  
  n1 = sum(x$shift_scores$freq_1, na.rm = TRUE)
  n2 = sum(x$shift_scores$freq_2, na.rm = TRUE)
  n = max(c(n1, n2))
  n1 <- n1/n
  n2 <- n2/n
  
  text_size <- data.frame(text_names, 
                          total = c(n1, n2),
                          order = c(1, 2))
  
  text_size_plot <- ggplot2::ggplot(text_size, 
                                    ggplot2::aes(x = stats::reorder(.data$text_names, 
                                                                    sort(.data$order, 
                                                                         decreasing = TRUE)), 
                                                 y = .data$total)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() + 
    ggplot2::ggtitle("Relative Text Size:") +
    text_size_theme() 
  
  text_size_plot
}


# cumulative contribution plot
cumulative_contribution_plot <- function(x, top_n){
  
  
  if(x$normalization == "variation"){
    cum_scores <- cumsum(abs(x$shift_scores$type2shift_score[order(abs(x$shift_scores$type2shift_score), 
                                                                   decreasing = TRUE)]))
    y_label <- expression(sum(abs(delta * Phi [tau])))
  } else {
    cum_scores <- cumsum(x$shift_scores$type2shift_score[order(abs(x$shift_scores$type2shift_score), 
                                                               decreasing = TRUE)])  
    y_label = expression(sum(delta * Phi [tau]))
  }
  
  cum_contribution_plot <- ggplot2::ggplot() + 
    ggplot2::geom_line(ggplot2::aes(x = seq_along(cum_scores), 
                                    y = cum_scores)) +
    ggplot2::scale_x_log10(labels = scientific_to_10) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    # set intercept based on top_n supplied for cutoff in main graph
    ggplot2::geom_vline(xintercept = top_n) + 
    ggplot2::theme_bw() + 
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::ylab(y_label) + 
    ggplot2::ggtitle("Cumulative Contribution") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.9), 
                                             hjust = 0.5,
                                             lineheight = .9,
                                             face = "bold.italic", 
                                             colour = "black"))
  
  cum_contribution_plot
  
}
