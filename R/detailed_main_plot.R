create_detailed_main_plot <- function(top_shift_scores, 
                                      top_n, 
                                      y_limits, 
                                      all_pos_contributions,
                                      norm_value){
  
  bar_dimensions <-  bar_dimension(top_shift_scores, norm_value, all_pos_contributions)
  
  # get the bar colours and add them to the bar_dimensions
  colours <- get_bar_colours(top_shift_scores, all_pos_contributions)
  bar_dimensions$bar_colours_p_solid <- colours$bar_colours_p
  bar_dimensions$bar_colours_s_solid <- colours$bar_colours_s
  bar_dimensions$bar_colours_p_fade <- colours$bar_colours_p
  bar_dimensions$bar_colours_s_fade <- colours$bar_colours_s
  
  # create x axis ordering, labels and breaks
  bar_dimensions$ordering <- 1:top_n
  x_label_breaks <- seq(0, top_n, 5)
  x_label_breaks[1] <- 1
  
  # stack the dimension in a long data form for ggplot. 
  bars <- cbind(bar_dimensions[,c("word", "ordering"), drop = FALSE], 
                utils::stack(bar_dimensions, select = c("p_solid_heights", 
                                                        "p_fade_heights", 
                                                        "s_solid_heights", 
                                                        "s_fade_heights")
                             ),
                utils::stack(bar_dimensions, select = c("bar_colours_p_solid", 
                                                        "bar_colours_p_fade", 
                                                        "bar_colours_s_solid", 
                                                        "bar_colours_s_fade")
                             ))
  names(bars) <- c("word", "ordering","heights","column_spec","colour","bar")
  
  # create shader values for alpha.
  bars$alpha_fade <- ifelse(bars$bar %in% c("bar_colours_p_solid", "bar_colours_s_solid"), 
                            1, 
                            .35)
  
  ## create plot. 
  # hjust is outward instead of calculation. 
  # label_heights defines location where the labels are placed.
  detailed_main_plot <- ggplot2::ggplot(bars,
                                        ggplot2::aes(x = .data$ordering,
                                                     y = .data$heights)) +
    ggplot2::geom_col(fill = scales::alpha(bars$colour, bars$alpha_fade)) +
    ggplot2::scale_x_reverse(breaks = x_label_breaks) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = y_limits) +
    ggplot2::annotate(geom = "text", 
                      x = bar_dimensions$ordering, 
                      y = bar_dimensions$label_heights, 
                      label = bar_dimensions$word, 
                      size = 3,
                      hjust = "outward") +
    ggplot2::coord_flip() +
    ggplot2::ylab(expression("Score shift" ~ delta * Phi [tau] * "(%)")) +
    main_theme()
  
  detailed_main_plot
}


# Gets the height and location of every bar needed to plot each type's contribution.
# returns a data.frame with ten columns: 
# `p_solid_heights`, `s_solid_bases`, `s_solid_heights`, `p_fade_heights`, `p_fade_bases`, 
# `s_fade_bases`,`s_fade_heights`, `total_heights`, `label_heights`. 
# Values are the corresponding bar dimensions for each word.
# 'p' stands for the component with p_diff
# 's' stands for the component with s_diff.
# 'solid' indicates the part of the contribution that is not alpha faded
# 'base' stands for where the bottom of the bar is
# 'height' stands for the height relative to the base
# Note, `p_solid_base` would always be 0, which is why it is not included
# `total_heights` is the overall contribution for simple (not detailed) shift
# graphs (base is always 0).
# `label_heights` is the label position after making up for counteracting components

bar_dimension <- function(top_shift_scores, norm_value, all_pos_contributions){
  
  c_p = top_shift_scores$type2p_diff * top_shift_scores$type2s_ref_diff / norm_value
  c_s = top_shift_scores$type2p_avg * top_shift_scores$type2s_diff / norm_value
  
  total_heights <- ifelse(!all_pos_contributions == TRUE | top_shift_scores$type2p_diff > 0, 
                          c_p + c_s, -1 * (c_p + c_s))
  
  p_solid_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                            c_p, ifelse(abs(c_p) > abs(c_s), c_p + c_s, 0))
  s_solid_bases <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                          c_p, ifelse(abs(c_p) > abs(c_s), 0, 0))
  s_solid_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                            c_s, ifelse(abs(c_p) > abs(c_s), 0, c_s + c_p))
  
  label_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                          c_p + c_s, ifelse(abs(c_p) > abs(c_s), c_p, c_s))
  
  p_fade_bases <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                         0, ifelse(abs(c_p) > abs(c_s), c_p + c_s, 0))
  
  p_fade_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                           0, ifelse(abs(c_p) > abs(c_s), -1 * c_s, c_p))
  
  s_fade_bases <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                         0, ifelse(abs(c_p) > abs(c_s), 0, c_s + c_p))
  
  s_fade_heights <- ifelse(sign(top_shift_scores$type2s_ref_diff * top_shift_scores$type2p_diff) * sign(top_shift_scores$type2s_diff) == 1,
                           0, ifelse(abs(c_p) > abs(c_s), c_s, -1 * c_p))
  
  bar_dimensions_out <- data.frame(word = top_shift_scores$word,
                                   total_heights, 
                                   p_solid_heights, 
                                   s_solid_bases, 
                                   s_solid_heights,
                                   label_heights,
                                   p_fade_bases,
                                   p_fade_heights,
                                   s_fade_bases,
                                   s_fade_heights)
  
  bar_dimensions_out
}