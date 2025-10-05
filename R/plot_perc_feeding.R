plot_perc_feeding <- function(df,
                              x_var,
                              fill_var = species,
                              fill_scale = scale_fill_Drosophila_trio,
                              col_var = species,
                              colour_scale = scale_colour_Drosophila_trio,
                              order_x,
                              title = NULL){
  
  df_summaries <- df %>%
    group_by({{x_var}}) %>%
    summarise(mean = mean(`%_feeding`, na.rm = T),
              n = n(),
              sd = sd(`%_feeding`, na.rm = T),
              se = sd/sqrt(n)) %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
  
  plot <- df_summaries %>%
    ggplot(aes(x = {{x_var}}, y = mean)) +
    geom_bar(aes(fill = {{fill_var}}), colour = 'black', stat = "identity", linewidth = 0.2, alpha = 1,  show.legend = FALSE) +
    geom_errorbar(aes(ymin = mean, ymax = mean + se), colour = 'black', width = 0.2, linewidth = 0.2, alpha = 1,) +
    geom_jitter(inherit.aes = F, data = df, aes(x = {{x_var}}, y = `%_feeding`, colour = {{col_var}}), size = 1, stroke = 0, alpha = 0.6, width = 0.2, height = 0) +
    geom_text(aes(x = {{x_var}}, y=0.1, label = n), size = 1.75) +
    theme_PI() +
    xlab(NULL) +
    ylab("% feeding (\u00B1 s.e.m.)") +
    scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1), labels = c('0', '50', '100')) +
    scale_x_discrete(limits = order_x) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) 
  
  return(plot)
  
}