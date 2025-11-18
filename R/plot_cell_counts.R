plot_cell_counts <- function(df,
                             x_var,
                             ymin = 0,
                             ymax = 50,
                             fill_var = species,
                             fill_scale = scale_fill_Drosophila_trio,
                             col_var= species,
                             colour_scale = scale_colour_Drosophila_trio,
                             order_x,
                             title = NULL){
  
  grouping_vars <- unique(c(rlang::as_name(ensym(x_var)),
                            rlang::as_name(ensym(fill_var)),
                            rlang::as_name(ensym(col_var))))
  
  df_summaries <- df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(mean = mean(number_cells, na.rm = T),
              n = n(),
              sd = sd(number_cells, na.rm = T),
              se = sd/sqrt(n),
              .groups = "drop") %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))

  
  plot <- df_summaries %>% 
    ggplot(aes(x = {{x_var}}, y = mean)) +
    geom_bar(stat = 'identity', aes(fill = {{fill_var}}), alpha = 1, show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = {{col_var}}), width = 0.2, linewidth = 0.2, alpha = 1,   show.legend = FALSE) +
    geom_jitter(inherit.aes = F, data = df, aes(x = {{x_var}}, y = number_cells, colour = {{col_var}}), size = 1, alpha = 0.6, width = 0.2, stroke = 0, height = 0, show.legend = FALSE) +
    geom_text(aes(y=0, label = n), size = 1.75, colour = 'white', vjust = 0) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_PI() +
    xlab(NULL) +
    ylab('number cells') +
    scale_x_discrete(limits = order_x) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    ggtitle(title)
    
  return(plot)
}