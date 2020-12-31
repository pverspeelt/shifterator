#' Generate a collection of shift graphs
#' 
#' When called this will construct word shift graphs.
#'
#' @param x A shift object.
#' @param top_n Integer value. Number of words to display in the main graph. Defaults to 50
#'
#' @return Returns the plots of all the shift graphs.
#' @export
#'
#' @examples
#' "example to follow"
get_shift_graphs <- function(x, top_n = 50L){
  
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
  
  top_shift_scores <- utils::head(x$shift_scores[order(abs(x$shift_scores$type2shift_score), 
                                                       decreasing = TRUE) , , drop = FALSE], 
                                  top_n)
  
  # set main colours
  pos_colour <- "#FECC5D"
  neg_colour <- "#9E75B7"
  
  main_plot <- create_main_plot(top_shift_scores = top_shift_scores, 
                                top_n = top_n, 
                                pos_colour = pos_colour,
                                neg_colour = neg_colour)
  
  # for now print. When done, patchword will take over.
  print(main_plot)
}


create_main_plot <- function(top_shift_scores, top_n, pos_colour, neg_colour){
  
  # set main plotting params
  # set colours
  shift_cols <- ifelse(top_shift_scores$type2shift_score >= 0, pos_colour, neg_colour)
  # set hjust paramaters ("outward" is to close to the edge of the bars)
  shift_hj <- ifelse(top_shift_scores$type2shift_score >= 0, -0.2, 1.2)
  # set scale limits
  shift_ylims <- c(-max(abs(top_shift_scores$type2shift_score)), 
                   max(abs(top_shift_scores$type2shift_score)))

  # create x axis ordering, labels and breaks
  top_shift_scores$ordering <- 1:top_n
  x_label_breaks <- seq(0, top_n, 5)
  x_label_breaks[1] <- 1
    
  main_plot <- ggplot2::ggplot(top_shift_scores, 
                               ggplot2::aes(x = .data$ordering, 
                                            y = .data$type2shift_score, 
                                            fill = shift_cols))
  main_plot <- main_plot + 
    ggplot2::geom_bar(stat = "identity") + 
    ggplot2::geom_text(ggplot2::aes(label = .data$word), 
                       size = 3, 
                       hjust = shift_hj) + 
    ggplot2::scale_fill_manual(values = c(neg_colour, pos_colour), 
                               guide = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::percent, 
                                limits = round(shift_ylims, digits = 2)) + 
    ggplot2::scale_x_reverse(breaks = x_label_breaks) + 
    ggplot2::coord_flip() +
    main_theme()
  
  
  main_plot
  
}

