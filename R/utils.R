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
  theme_minimal() +
    theme(panel.grid = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          plot.title = element_text(size = rel(1.5), 
                                    lineheight = .9,
                                    face = "bold.italic", 
                                    colour = "black"))
}

# use in cumulative contribution plot
scientific_to_10 <- function(x){
  parse(text=gsub("1e\\+*", " 10^", scales::scientific_format()(x))) 
}
