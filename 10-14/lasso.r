library(casl)
library(ggplot2)
library(dplyr)

x <- seq(-2, 2, 0.1)

tibble(x = x, y = casl_util_soft_thresh(x, 1)) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_line() +
  theme_minimal()
