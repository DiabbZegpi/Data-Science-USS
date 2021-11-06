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

clean_data <- read_csv(file = here("Big Data", "Data", "clean_data.csv"))
skimr::skim(clean_data)

sorted_pct <- function(..., wt = NULL) {
  clean_data %>% 
    count(..., sort = TRUE) %>% 
    mutate(pct = n / sum(n))
}

sorted_pct(STATE)
sorted_pct(MAKE)
sorted_pct(MAKE, MODEL)
sorted_pct(COLOR)
sorted_pct(REPORTED_YEAR, wt = NUMBER_OF_TRIPS)
sorted_pct(REPORTED_YEAR, REPORTED_MONTH, wt = NUMBER_OF_TRIPS)


