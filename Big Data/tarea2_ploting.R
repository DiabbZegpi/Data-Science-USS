library(tidyverse)
library(here)
library(ggtext)

theme_set(theme_light(base_family = "Amasis MT Std", base_size = 16))
theme_update(
  text = element_text(color = "#111111"),
  plot.title = element_markdown(hjust = .5),
  plot.subtitle = element_markdown(hjust = .5),
  strip.text = element_text(color = "#111111", face = "italic", size = "16")
)
