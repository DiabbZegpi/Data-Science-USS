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
  labs(title = "Número de viajes de TNPS en Chicago",
       x = NULL, y = "Número de viajes")

color_popularity <- 
  clean_data %>% 
  count(REPORTED_YEAR, COLOR, sort = TRUE, wt = NUMBER_OF_TRIPS, name = "n_trips") %>% 
  group_by(REPORTED_YEAR) %>%
  slice_max(n_trips, n = 5) %>%   
  mutate(pct = n_trips / sum(n_trips)) %>% 
  ungroup()

popularity_labs <- clean_data %>% 
  count(REPORTED_YEAR, COLOR, sort = TRUE, wt = NUMBER_OF_TRIPS, name = "n_trips") %>% 
  group_by(REPORTED_YEAR) %>%
  mutate(pct = n_trips / sum(n_trips)) %>% 
  slice_max(n_trips, n = 5) %>% 
  summarise(pct = paste0(round(sum(pct) * 100), "%"))

color_popularity %>% 
  ggplot(aes(factor(REPORTED_YEAR), pct, fill = COLOR)) +
  geom_col(color = "black", alpha = .8, size = 1, show.legend = FALSE) +
  geom_text(data = popularity_labs, aes(x = factor(REPORTED_YEAR), y = 1.05, label = pct),
            inherit.aes = FALSE, size = 7, family = "Amasis MT Std") +
  scale_fill_manual(values = c(
    "Black" = "#111111", "Blue" = "dodgerblue4", "Dark Red" = "firebrick",
    "Grey" = "grey40", "Red" = "red", "Silver" = "#C0C0C0", "White" = "white"
  )) +
  scale_y_continuous(labels = NULL) +
  labs(title = "Los 5 colores más populares por año",
       subtitle = "Cada año, los 5 colores más populares concentraron más del 83% de los viajes",
       x = NULL, y = NULL)

density_labs <- 
  tibble(
    label = c("El 50% de los conductores que están registrados<br>en <b style='color:#7030A0;'>una sola TNPS</b> tienen menos de 192 viajes al mes",
              "En cambio, el 50% de los conductores con<br><b style='color:#51032d;'>múltiples TNPS</b> tienen al menos 237 viajes mensuales"),
    x = c(440, 500),
    y = c(.005, .0035)
  )

clean_data %>% 
  ggplot(aes(NUMBER_OF_TRIPS, fill = MULTIPLE_TNPS)) +
  geom_density(show.legend = FALSE, alpha = .5) + 
  scale_fill_manual(values = c("#7030A0", "#51032d")) +
  geom_richtext(data = density_labs, inherit.aes = FALSE,
                aes(x = x, y = y, label = label), size = 5.5, label.color = NA, fill = NA) +
  annotate(geom = "segment", x = 192, xend = 240, y = .0035, yend = .0045, color = "#7030A0", size = 1) +
  annotate(geom = "segment", x = 180, xend = 700, y = .0045, yend = .0045, color = "#7030A0", size = 1) +
  annotate(geom = "segment", x = 237, xend = 340, y = .0015, yend = .0030, color = "#51032d", size = 1) +
  annotate(geom = "segment", x = 240, xend = 760, y = .0030, yend = .0030, color = "#51032d", size = 1) +
  labs(title = "Densidad del número de viajes mensuales según estrategia",
       x = "Número de viajes", y = "Densidad")
