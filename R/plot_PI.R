plot_PI <- function(df,
         x_var,
         fill_var = species,
         fill_scale = scale_fill_Drosophila_trio,
         col_var = species,
         colour_scale = scale_colour_Drosophila_trio,
         order_x,
         title = NULL) {
  
  df_summaries <- df %>%
    group_by({{x_var}}) %>%
    summarise(n = n()) %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
  
  
  plot <- ggplot(df, aes(x = {{x_var}}, y = preference_index)) +
    geom_hline(yintercept = 0, colour = "black", linetype = "solid", linewidth = 0.2) + 
    geom_boxplot(aes(fill = {{fill_var}}), linewidth = 0.2, alpha = 1, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = {{col_var}}), size = 1, alpha = 0.6, width = 0.2, stroke = 0, height = 0) +
    geom_text(inherit.aes = F, data = df_summaries, aes(x = {{x_var}}, y = -1.15, label = n), size = 1.75) +
    theme_PI() +
    ylab("Preference Index") +
    scale_x_discrete(limits = order_x) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    ggtitle(title) +
    coord_cartesian(ylim = c(-1.2, 1.1)) +
    scale_y_continuous(breaks = c(-1, 0, 1), labels = c('-1', '0', '+1')) -> plot 
  
  return(plot)
}
