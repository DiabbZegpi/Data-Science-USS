library(tidyverse)
library(here)
library(ggtext)

theme_set(theme_minimal(base_family = "Amasis MT Std", base_size = 16))
theme_update(
  text = element_text(color = "#111111"),
  plot.title = element_markdown(hjust = .5),
  plot.subtitle = element_markdown(hjust = .5),
  strip.text = element_text(color = "#111111", face = "italic", size = "16"),
  panel.grid.minor.x = element_blank()
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

clean_data %>% 
  group_by(DATE) %>% 
  summarize(TRIPS = sum(NUMBER_OF_TRIPS)) %>% 
  ggplot(aes(DATE, TRIPS)) +
  geom_line(color = "#7030A0", size = 1) +
  geom_smooth(method = "loess", formula = y ~ x, alpha = .5, color = "#b898d0", fill = "#b898d0") +
  scale_y_continuous(labels = number_format()) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m-%Y") +
  labs(title = "Número de viajes de TPNS en Chicago",
       x = NULL, y = "Número de viajes")

clean_data %>% 
  count(REPORTED_YEAR, COLOR, sort = TRUE, wt = NUMBER_OF_TRIPS, name = "n_trips") %>% 
  group_by(REPORTED_YEAR) %>%
  slice_max(n_trips, n = 5) %>%   
  mutate(pct = n_trips / sum(n_trips)) 
