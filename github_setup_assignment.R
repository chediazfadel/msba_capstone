library(tidyverse)

set.seed(123)
df <- tibble(x = rnorm(5000))

df %>%
  ggplot() +
  geom_histogram(aes(x),
                 binwidth = 0.2,
                 fill = "darkred", color = "white") +
  stat_bin(aes(x = x, y = after_stat(count), label = ifelse(after_stat(count) == 0, "", after_stat(count))),
             geom = "text", binwidth = 0.2, size = 5, fontface = "bold", vjust = 0) +
  theme(panel.background = element_blank())