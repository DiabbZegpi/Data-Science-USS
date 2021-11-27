library(tidyverse)
library(tidymodels)
library(here)
library(sparklyr)
clean_data <- read_csv(file = here("Big Data", "Data", "clean_data.csv")) %>% 
  janitor::clean_names()


sc <- spark_connect(master = "local")
cars_tbl <- copy_to(sc, clean_data)


spark_dplyr <- cars_tbl %>% 
  mutate(reported_day = as.numeric(str_sub(date, -2L)),
         multiple_tnps = if_else(multiple_tnps, 1, 0),
         state_il = if_else(state == "IL", 1, 0),
         make_toyota = if_else(make == "Toyota", 1, 0),
         make_nissan = if_else(make == "Nissan", 1, 0),
         make_honda = if_else(make == "Honda", 1, 0),
         make_chevrolet = if_else(make == "Chevrolet", 1, 0),
         make_hyundai = if_else(make == "Hyundai", 1, 0),
         make_ford = if_else(make == "Ford", 1, 0),
         make_kia = if_else(make == "Kia", 1, 0),
         make_dodge = if_else(make == "Dodge", 1, 0),
         make_volkswagen = if_else(make == "Volkswagen", 1, 0),
         make_jeep = if_else(make == "Jeep", 1, 0),
         make_chrysler = if_else(make == "Chrysler", 1, 0),
         make_lincoln = if_else(make == "Lincoln", 1, 0),
         make_mazda = if_else(make == "Mazda", 1, 0),
         make_acura = if_else(make == "Acura", 1, 0),
         model_camry = if_else(model == "Camry", 1, 0),
         model_corolla = if_else(model == "Corolla", 1, 0),
         model_altima = if_else(model == "Altima", 1, 0),
         model_prius = if_else(model == "Prius", 1, 0),
         model_sentra = if_else(model == "Sentra", 1, 0),
         model_elantra = if_else(model == "Elantra", 1, 0),
         model_sonata = if_else(model == "Sonata", 1, 0),
         model_accord = if_else(model == "Accord", 1, 0),
         model_civic = if_else(model == "Civic", 1, 0),
         model_malibu = if_else(model == "Malibu", 1, 0),
         model_fusion = if_else(model == "Fusion", 1, 0),
         model_rav4 = if_else(model == "Rav4", 1, 0),
         model_rogue = if_else(model == "Rogue", 1, 0),
         model_cruze = if_else(model == "Cruze", 1, 0),
         model_escape = if_else(model == "Escape", 1, 0),
         model_sienna = if_else(model == "Sienna", 1, 0),
         color_black = if_else(color == "Black", 1, 0),
         color_silver = if_else(color == "Silver", 1, 0),
         color_white = if_else(color == "White", 1, 0),
         color_grey = if_else(color == "Grey", 1, 0),
         color_red = if_else(color == "Red", 1, 0),
         color_blue = if_else(color == "Blue", 1, 0),
         number_of_trips = log(number_of_trips)) %>% 
  select(state_il, multiple_tnps, number_of_trips,
         starts_with("reported_"), 
         starts_with("make_"),
         starts_with("model_"),
         starts_with("color_"))


cars_pipeline <- ml_pipeline(sc) %>% 
  ft_dplyr_transformer(tbl = spark_dplyr) %>% 
  ft_r_formula(number_of_trips ~ .) %>% 
  ml_random_forest_regressor()


spark_split <- sdf_random_split(cars_tbl, training = .6, testing = .4)


spark_model <- ml_fit(cars_pipeline, spark_split$training)  
spark_pred <- ml_transform(spark_model, spark_split$testing)


results <- spark_pred %>% 
  select(number_of_trips, prediction,
         starts_with("make_"),
         starts_with("color_")) %>%
  collect()


results %>%   
  mutate(number_of_trips = exp(number_of_trips),
         prediction = exp(prediction)) %>% 
  pivot_longer(cols = starts_with("color_"), names_to = "color") %>%
  filter(value > 0) %>% 
  mutate(color = str_remove(color, "color_") %>% str_to_title(),
         error = number_of_trips - prediction,
         error2 = error ^ 2) %>%
  group_by(color) %>% 
  summarise(rmse = sqrt(mean(error2)), 
            mae = mean(abs(error)),
            std = sd(error),
            skew = moments::skewness(error),
            kurtosis = moments::kurtosis(error),
            n = n(),
            .groups = "drop")
            
            
            