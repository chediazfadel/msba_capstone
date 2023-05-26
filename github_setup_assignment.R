library(tidyverse)

set.seed(123)
df <- tibble(x = rnorm(5000))

df %>%
  ggplot() +
  geom_histogram(aes(x),
                 binwidth = 0.2,
                 fill = "darkred", color = "white") +
  labs(x = "rate of return")
