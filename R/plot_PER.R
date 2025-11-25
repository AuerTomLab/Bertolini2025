plot_PER <- function(df,
                     x_var,
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
    summarise(mean = mean(`%_PER`, na.rm = T),
              n = n(),
              sd = sd(`%_PER`, na.rm = T),
              se = sd/sqrt(n),
              .groups = "drop") %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
  
  
  plot <- df_summaries %>% 
    ggplot(aes(x = {{x_var}}, y = mean)) +
    geom_bar(stat = 'identity', aes(fill = {{fill_var}}), alpha = 1, show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, colour = {{col_var}}), width = 0.2, linewidth = 0.2, alpha = 1,   show.legend = FALSE) +
    geom_jitter(inherit.aes = F, data = df, aes(x = {{x_var}}, y = `%_PER`, colour = {{col_var}}), size = 1, alpha = 0.6, width = 0.2, stroke = 0, height = 0, show.legend = FALSE) +
    geom_text(aes(y=0, label = n), size = 1.75, colour = 'white', vjust = 0) +
    theme_PI() +
    xlab(NULL) +
    ylab('PER probability') +
    scale_x_discrete(limits = order_x) +
    scale_y_continuous(expand = c(0, 0), 
                       limits = c(0, 1.1), 
                       breaks = c(0,0.333,0.666,1),
                       labels = c("0/3", "1/3", "2/3", "3/3")
    ) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    ggtitle(title)
  
  return(plot)
}