plot_PET <- function(df,
                     x_var,
                     fill_var = species,
                     fill_scale = scale_fill_Drosophila_trio,
                     col_var= species,
                     colour_scale = scale_colour_Drosophila_trio,
                     order_x,
                     coord_min = -10,
                     coord_max = NULL,
                     title = NULL){
  
  grouping_vars <- unique(c(rlang::as_name(ensym(x_var)),
                            rlang::as_name(ensym(fill_var)),
                            rlang::as_name(ensym(col_var))))
  
  coord_max <- ifelse(is.null(coord_max),
                          max(df$consumption_time_seconds, na.rm = T) + 10,
                          coord_max)
  
  df_summaries <- df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n = n(),
      .groups = "drop") %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
  
  
  plot <- df %>% 
    ggplot(aes(x = {{x_var}}, y = consumption_time_seconds)) +
    geom_boxplot(aes(fill = {{fill_var}}),
                 linewidth = 0.2, alpha = 1,
                 outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = {{col_var}}),
                size = 1, alpha = 0.6,
                width = 0.2, stroke = 0, height = 0) +
    geom_text(inherit.aes = FALSE,
              data = df_summaries,
              aes(x = {{x_var}}, y = -5, label = n),
              size = 1.75) +
    theme_PI() +
    xlab(NULL) +
    ylab('PET (sec)') +
    scale_x_discrete(limits = order_x) +
    scale_y_continuous(expand = c(0, 0), limits = c(coord_min, coord_max)) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    ggtitle(title)
  
  return(plot)
}