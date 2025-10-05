theme_PI <- function(){
  theme(plot.background = element_blank(),
      panel.grid = element_blank(),
      axis.line.y = element_line(colour = "black", linewidth = 0.2),
      axis.line.x = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 7, colour = "black"),
      axis.title.y = element_text(size = 7, colour = "black"),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 6, colour = "black"),
      axis.text.x = element_text(size = 6, angle = 25, vjust = 1, hjust=1, face = "italic", colour = "black"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(linewidth = 0.2, colour = "black"),
      strip.text.x = element_text(size = 7, colour = "black", angle = 0),
      strip.background = element_blank(),
      plot.margin = margin(1, 1, 1, 1))
}
