lexis <- function(data, fill_column, 
                  breaks = c(0, .05, .1, .15, .2, .25, .3, .4, .5, .75, .9, 5),
                  labels = c("0 to 5%", 
                              "5 to 10%",
                              "10 to 15%",
                              "15 to 20%",
                              "20 to 25%",
                              "25 to 30%",
                              "30 to 40%",
                              "40 to 50%",
                              "50 to 75%",
                              "75 to 90%",
                              ">90% Death Coverage"
                  )) {
# 
fill_column <- enquo(fill_column)
  
# discretize sex ratio
data <- data %>%
  mutate(coverage_categorical =
           cut(!! fill_column,
               breaks, labels,
               include.lowest = FALSE))

# plot mortality sex ratio Lexis surface
lexis_plot <- ggplot(data) +
  # heatmap
  geom_raster(aes(x = Year+1, y = Age+1,
                  fill = coverage_categorical)) +
  # Lexis grid
  geom_hline(yintercept = seq(0, 110, 10),
             alpha = 0.2, lty = "dotted") +
  geom_vline(xintercept = seq(1940, 2010, 10),
             alpha = 0.2, lty = "dotted") +
  geom_abline(intercept = seq(-100, 100, 10)-1910,
              alpha = 0.2, lty = "dotted") +
  # scales
  scale_fill_brewer(name = NULL, type = "div", palette = 5, drop = FALSE) +
  scale_x_continuous("Year", expand = c(0.02, 0),
                     breaks = seq(1940, 2010, 10)) +
  scale_y_continuous("Age", expand = c(0, 0),
                     breaks = seq(0, 110, 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  # coord
  coord_equal() +
  # theme
  theme_void() +
  theme(
    axis.text = element_text(colour = "black"),
    axis.text.y = element_text(size=15),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), 
    plot.title = element_text(size=20, vjust = 2),
    legend.text = element_text(size = 15)
    
  )

return(lexis_plot)

}
