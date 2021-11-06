library(tidyverse)
library(tidymodels)
library(here)

ridesharing <- read_csv(file = here("Big Data", "Data", "chicago-ridesharing-vehicles.csv"))
ridesharing %>% skimr::skim()

# ---------------------------------------------------------------------------
# Data cleaning
# Get the data here: 
# https://www.kaggle.com/subwaymatch/chicago-uberlyft-vehicles?select=chicago-ridesharing-vehicles.csv
# ---------------------------------------------------------------------------

# COLOR cleaning: remove non-letter or non-spaces
# Filter out 'Name' and '0' 
# Remove `NA`

clean_color_make <- 
  ridesharing %>% 
  mutate(COLOR = str_extract(COLOR, "\\w+(\\s*\\w*)*")) %>% 
  filter(!COLOR %in% c("Name", "0") & !is.na(COLOR)) %>% 

# MAKE cleaning  
  mutate(MAKE = case_when(
    str_detect(MAKE, "Mercedes") ~ "Mercedes Benz",
    MAKE == "Ram Trucks" ~ "Ram",
    MAKE == "Chevy" ~ "Chevrolet",
    TRUE ~ MAKE 
  ))

# MODEL cleaning: replace spaces and non-spaces with hyphens
clean_series <- 
  clean_color_make %>% 
  count(MAKE, MODEL, sort = TRUE) %>% 
  filter(str_detect(MODEL, "[Ss]eries|[Rr]unner")) %>% 
  mutate(serie = str_extract(MODEL, "(.+)(?=[Ss]eries|[Rr]unner)"),
         serie = str_remove_all(serie, "\\s|-"), 
         model = if_else(
           str_detect(MODEL, "[Ss]eries"), paste0(serie, "-Series"), paste0(serie, "-Runner")
         )) %>% 
  distinct(MAKE, model, .keep_all = TRUE) %>% 
  select(MAKE, MODEL, model)

clean_class <- 
  clean_color_make %>% 
  count(MAKE, MODEL, sort = TRUE) %>% 
  filter(str_detect(MODEL, "[Cc]lass"), 
         MAKE == "Mercedes Benz") %>% 
  mutate(class = str_extract(MODEL, "(.*)(?=[Cc]lass)"),
         class = str_remove_all(class, "\\s|-"),
         model = paste0(class, "-Class")) %>% 
  distinct(model, .keep_all = TRUE) %>% 
  select(MAKE, MODEL, model)

clean_data <- 
  clean_color_make %>% 
  left_join(clean_series, by = c("MAKE", "MODEL")) %>% 
  mutate(MODEL = if_else(!is.na(model), model, MODEL)) %>% 
  select(-model) %>% 
  left_join(clean_class, by = c("MAKE", "MODEL")) %>% 
  mutate(MODEL = if_else(!is.na(model), model, MODEL)) %>% 
  select(-model)

# MODEL_YEAR cleaning: impute missing values (vehicles with MODEL_YEAR minor than 1990)
data_to_impute <- 
  clean_data %>% 
  mutate(MODEL_YEAR = if_else(MODEL_YEAR < 1990, NA_real_, MODEL_YEAR),
         MODEL_YEAR = as.integer(MODEL_YEAR),
         id = row_number())

knn_recipe <- 
  recipe(~ ., data = data_to_impute) %>% 
  step_other(MODEL, threshold = 500) %>% 
  step_impute_knn(MODEL_YEAR, impute_with = c("MAKE", "MODEL", "COLOR", "NUMBER_OF_TRIPS"), 
                  neighbors = 10)
  
imputed_model_year <- knn_recipe %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  select(id, model_year = MODEL_YEAR)

clean_data <- 
  data_to_impute %>% 
  left_join(imputed_model_year, by = "id") %>% 
  mutate(MODEL_YEAR = if_else(is.na(MODEL_YEAR), model_year, MODEL_YEAR)) %>% 
  select(-c(model_year, id)) %>% 
  # Create date column
  mutate(DATE = paste(REPORTED_YEAR, REPORTED_MONTH, "01", sep = "-"),
         DATE = as.Date(DATE))

write_csv(clean_data, file = here("Big Data", "Data", "clean_data.csv"))