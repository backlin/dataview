library(dplyr)
library(ggplot2)

g <- data.frame(x = 1:10, y = 1:10) %>%
  ggplot(aes(x = x, y = y)) +
    scale_y_continuous(limits = c(1, 10), expand = c(0, 0)) +
    geom_point()
g <- ggplot_gtable(ggplot_build(g))

