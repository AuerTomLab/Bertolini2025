theme_PCA <- function(){
  theme(panel.grid = element_blank(),
        axis.line.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = "none",
        axis.title = element_text(size = 7, colour = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = margin(1, 1, 1, 1),
        plot.title = element_text(size = 7, colour = "black")
  )
}
