plot_NSips_flyPAD <- function(df,
                              x_var,
                              food_A,
                              food_B,
                              fill_var = species,
                              fill_scale = scale_fill_Drosophila_trio,
                              col_var = species,
                              colour_scale = scale_colour_Drosophila_trio,
                              order_x,
                              title = NULL) {

  df_A <- df[df$food_choice == food_A, ]
  df_B <- df[df$food_choice == food_B, ]
  
  y_max <- max(df$`NumberOfSips/1hour`, na.rm = TRUE) + 1
  
  make_plot <- function(sub_df, food_label) {
    
    df_summaries <- sub_df %>%
      group_by({{x_var}}) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
    
    ggplot(sub_df, aes(x = {{x_var}}, y = `NumberOfSips/1hour`)) +
      geom_boxplot(aes(fill = {{fill_var}}),
                   linewidth = 0.2, alpha = 1,
                   outlier.shape = NA, show.legend = FALSE) +
      geom_jitter(aes(colour = {{col_var}}),
                  size = 1, alpha = 0.6,
                  width = 0.2, stroke = 0, height = 0) +
      geom_text(inherit.aes = FALSE,
                data = df_summaries,
                aes(x = {{x_var}}, y = -1.15, label = n),
                size = 1.75) +
      theme_PI() +
      ylab("NumberOfSips/1hour") +
      scale_x_discrete(limits = order_x) +
      coord_cartesian(ylim = c(0, y_max)) +
      scale_fill_manual(values = fill_scale()) +
      scale_colour_manual(values = colour_scale()) +
      ggtitle(food_label)
  }
  
  return(list(
    plot_A = make_plot(df_A, food_A),
    plot_B = make_plot(df_B, food_B)
  ))
  
}




plot_PI_flyPAD <- function(df,
                           x_var,
                           food_A,
                           food_B,
                           fill_var = species,
                           fill_scale = scale_fill_Drosophila_trio,
                           col_var = species,
                           colour_scale = scale_colour_Drosophila_trio,
                           order_x,
                           title = NULL) {
  
  grouping_vars <- unique(c("fly_id",
                            rlang::as_name(ensym(x_var)),
                            rlang::as_name(ensym(fill_var)),
                            rlang::as_name(ensym(col_var))))
  
  df %>% 
    group_by(across(all_of(grouping_vars))) %>%
    summarize(
      preference_index = (`NumberOfSips/1hour`[food_choice == {{food_A}}] -
                            `NumberOfSips/1hour`[food_choice == {{food_B}}]) /
        (`NumberOfSips/1hour`[food_choice == {{food_A}}] +
           `NumberOfSips/1hour`[food_choice == {{food_B}}]),
      .groups = "drop"
    ) %>% 
    select_all() -> df_PI
  
  
  df_summaries <- df_PI %>%
    group_by({{x_var}}) %>%
    summarise(n = n()) %>%
    mutate(!!rlang::ensym(x_var) := fct_relevel(!!rlang::ensym(x_var), order_x))
  
  plot <- df_PI %>% 
    ggplot(aes(x = {{x_var}}, y = preference_index)) +
    geom_hline(yintercept = 0, colour = "black", linetype = "solid", linewidth = 0.2) + 
    geom_boxplot(aes(fill = {{fill_var}}), linewidth = 0.2, alpha = 1, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(colour = {{col_var}}), size = 1, alpha = 0.6, width = 0.2, stroke = 0, height = 0) +
    geom_text(inherit.aes = F, data = df_summaries, aes(x = {{x_var}}, y = -1.15, label = n), size = 1.75) +
    theme_PI() +
    ylab("Preference Index") +
    scale_x_discrete(limits = order_x) +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    coord_cartesian(ylim = c(-1.2, 1.1)) +
    scale_y_continuous(breaks = c(-1, 0, 1), labels = c('-1', '0', '+1')) +
    ggtitle(title)
  
  return(plot)
}


plot_flyPAD <- function(df,
                        x_var,
                        food_A,
                        food_B,
                        fill_var = species,
                        fill_scale = scale_fill_Drosophila_trio,
                        col_var = species,
                        colour_scale = scale_colour_Drosophila_trio,
                        order_x,
                        title = NULL,
                        panel_height = unit(20, "mm"),
                        panel_width = unit(15, "mm")
                        ){
  
  
  plots_Nsips <- df %>% 
    plot_NSips_flyPAD(x_var = {{x_var}},   
                      food_A = food_A,
                      food_B = food_B,
                      order_x = order_x
    )
  
  p1 <- plots_Nsips[[1]] + 
    force_panelsizes(panel_height,
                     panel_width)
  
  p2 <- plots_Nsips[[2]] + 
    force_panelsizes(panel_height,
                     panel_width)
  
  p3 <- df %>%
    plot_PI_flyPAD(
      x_var = {{x_var}},
      food_A = food_A,
      food_B = food_B,
      order_x = order_x, 
      title = title
    ) +
    force_panelsizes(panel_height,
                     panel_width)
  
  
  
  p <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, align = "h")
  
  return(p)
}




plot_PCA_flyPAD <- function(df,
                            PC1 = PC1,
                            PC2 = PC2,
                            group_var,
                            label_var = strain,
                            fill_var = species,
                            fill_scale = scale_fill_Drosophila_trio,
                            col_var = species,
                            colour_scale = scale_colour_Drosophila_trio,
                            title = NULL) {
  
  plot <- df %>% 
    ggplot(aes(x = {{PC2}}, y = {{PC1}})) +
    geom_hline(yintercept = 0, linewidth = 0.2, colour = "black") +
    geom_vline(xintercept = 0,  linewidth = 0.2, colour = "black") +
    geom_point(aes(color = {{col_var}}), size = 1, alpha = 0.8, stroke = 0) +
    geom_polygon(aes(group = {{group_var}}, fill = {{fill_var}}), colour = "black", linewidth = 0.1) +
    geom_point(size = 1, alpha = 1, stroke = 0) +
    ggrepel::geom_text_repel(aes(label = {{label_var}}), size = 1.2, direction = "both", fontface = "italic") +
    theme_PCA() +
    ylab("PC1") +
    xlab("PC2") +
    scale_fill_manual(values = fill_scale()) +
    scale_colour_manual(values = colour_scale()) +
    scale_y_continuous(limits = c(-6,6), breaks = 0) +
    scale_x_continuous(limits = c(-6,6), breaks = 0) +
    ggtitle(title)
  
  
  return(plot)
}