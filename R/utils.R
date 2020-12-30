# internal stop function to stop the process but not show the error:
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


main_theme <- function(...){
  ggplot2::theme_grey() +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1, colour = "grey"), 
                   panel.grid = ggplot2::element_line(colour = "grey"),
                   panel.grid.minor.x = ggplot2::element_line(color = NA),
                   panel.grid.minor.y = ggplot2::element_line(color = NA),
                   panel.background = ggplot2::element_rect(fill = NA))
}
