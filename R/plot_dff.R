plot_dff <- function(df,
                     x_var,
                     order_x,
                     facet_var,
                     order_facets,
                     fill_var = species,
                     fill_scale = scale_fill_Drosophila_trio,
                     col_var = species,
                     colour_scale = scale_colour_Drosophila_trio,
                     x_pos_N_label = 1,
                     coord_min = NULL,
                     coord_max = NULL,
                     title = NULL){
  
  df %>%
    filter({{x_var}} %in% order_x) %>% 
    filter({{facet_var}} %in% order_facets) %>% 
    select(everything()) -> df
  
  df_summaries <- df %>%
    group_by({{x_var}}, {{facet_var}}) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate({{facet_var}} := fct_relevel({{facet_var}}, order_facets)) %>% 
    mutate({{x_var}} :=fct_relevel({{x_var}}, order_x))
  
  df %>% 
    mutate({{x_var}} := fct_relevel({{x_var}}, order_x)) %>%  
    mutate({{facet_var}} := fct_relevel({{facet_var}}, order_facets)) %>%
    ggplot(aes(x = {{x_var}}, y = dff)) +
    geom_boxplot(aes(fill = {{fill_var}}), linewidth = 0.2, alpha = 1, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = {{col_var}}), size = 1, alpha = 0.6, width = 0.2, stroke = 0, height = 0) +
    geom_text(inherit.aes = F, data = df_summaries, aes(x={{x_var}}, y=x_pos_N_label, label = paste0("n = ", n)), size = 1.75, colour = "black", hjust = 1) +
    facet_wrap(vars({{facet_var}}), ncol = 1, strip.position = "top") +
    coord_flip(ylim = c(coord_min, coord_max)) + 
    ylab("Δ F/F (%)") +
    xlab(NULL) +
    theme_dff() +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    scale_y_continuous(labels = function(x) x * 100) +
    ggtitle(title) -> plot
  
  return(plot)
  
}