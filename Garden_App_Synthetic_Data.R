#Libraries 
library(dplyr)
library(purrr)
library(lubridate)

#List of available plants 
plants <- c(
  "Apple", "Avocado", "Banana", "Bean", "Blueberry", "Broccoli", "Carrot",
  "Chamomile", "Cherry", "Corn", "Cucumber", "Eggplant", "Garlic",
  "Grape", "Lavender", "Lemon", "Lettuce", "Olive", "Onion", "Orange",
  "Peach", "Pear", "Peas", "Pepper (hot)", "Pepper (sweet)",
  "Pineapple", "Potato", "Pumpkin", "Strawberry", "Sunflower", "Tomato",
  "Watermelon"
)
#List of minimum sun needed for available plants 
min_sun <- c(
  Apple = 6, Avocado = 6, Banana = 6, Bean = 4, Blueberry = 6, Broccoli = 6, Carrot = 6,
  Chamomile = 4, Cherry = 6, Corn = 6, Cucumber = 6, Eggplant = 6, Garlic = 6,
  Grape = 6, Lavender = 6, Lemon = 6, Lettuce = 4, Olive = 6, Onion = 6, Orange = 6,
  Peach = 6, Pear = 6, Peas = 4, `Pepper (hot)` = 6, `Pepper (sweet)` = 6,
  Pineapple = 6, Potato = 6, Pumpkin = 6, Strawberry = 6, Sunflower = 6, Tomato = 6,
  Watermelon = 6
)
#List of maximum sun needed for available plants 
max_sun <- c(
  Apple = 10, Avocado = 10, Banana = 10, Bean = 8, Blueberry = 8, Broccoli = 8, Carrot = 8,
  Chamomile = 6, Cherry = 10, Corn = 10, Cucumber = 8, Eggplant = 10, Garlic = 8,
  Grape = 10, Lavender = 8, Lemon = 10, Lettuce = 6, Olive = 10, Onion = 8, Orange = 10,
  Peach = 10, Pear = 10, Peas = 6, `Pepper (hot)` = 10, `Pepper (sweet)` = 10,
  Pineapple = 10, Potato = 8, Pumpkin = 8, Strawberry = 8, Sunflower = 10, Tomato = 10,
  Watermelon = 10
)
#List of average days to harvest for available plants 
days_harvest <- c(
  Apple = 150, Avocado = 180, Banana = 120, Bean = 60, Blueberry = 90, Broccoli = 70, Carrot = 75,
  Chamomile = 60, Cherry = 120, Corn = 90, Cucumber = 60, Eggplant = 80, Garlic = 180,
  Grape = 150, Lavender = 90, Lemon = 180, Lettuce = 45, Olive = 180, Onion = 120, Orange = 180,
  Peach = 120, Pear = 120, Peas = 60, `Pepper (hot)` = 80, `Pepper (sweet)` = 80,
  Pineapple = 180, Potato = 90, Pumpkin = 120, Strawberry = 90, Sunflower = 90, Tomato = 75,
  Watermelon = 90
)
#List of minutes of watering needed for available plants 
water_schedule <- c(
  Apple = 35, Avocado = 32, Banana = 38, Bean = 30, Blueberry = 33, Broccoli = 36, Carrot = 31,
  Chamomile = 30, Cherry = 37, Corn = 34, Cucumber = 32, Eggplant = 39, Garlic = 30,
  Grape = 36, Lavender = 31, Lemon = 38, Lettuce = 30, Olive = 35, Onion = 30, Orange = 37,
  Peach = 34, Pear = 33, Peas = 30, `Pepper (hot)` = 36, `Pepper (sweet)` = 35,
  Pineapple = 38, Potato = 32, Pumpkin = 39, Strawberry = 33, Sunflower = 34, Tomato = 36,
  Watermelon = 39
)
#List of weight yields for available plants 
yield_weight <- c(
  Apple = 300, Avocado = 250, Banana = 400, Bean = 150, Blueberry = 100, Broccoli = 200, Carrot = 120,
  Chamomile = 50, Cherry = 100, Corn = 300, Cucumber = 250, Eggplant = 300, Garlic = 80,
  Grape = 200, Lavender = 60, Lemon = 150, Lettuce = 200, Olive = 100, Onion = 150, Orange = 250,
  Peach = 200, Pear = 200, Peas = 100, `Pepper (hot)` = 150, `Pepper (sweet)` = 150,
  Pineapple = 500, Potato = 300, Pumpkin = 800, Strawberry = 100, Sunflower = 150, Tomato = 250,
  Watermelon = 1000
)
#List of range of fertilizer makeup for available plants 
fertilizer_npk <- list(
  N = 0:10,  # Nitrogen
  P = 0:10,  # Phosphorus
  K = 0:10   # Potassium
)
#Minimum and maximum days to fertilize available plants 
fertilizer_days <- c(
  min = 7,
  max = 60
)

#Set seed for random reproducibility using the answer to the ultimate question of life, the universe, and everything
set.seed(42)
#Year range for synthetic data
years <- 2019:2024
#Limit to 100 entries per year
entries_per_year <- 100

#Loops throught each year, creates 10x10 grid to match app grid, selects first 100 positions for the year,
#randomly assigns plant to each grid cell, randomly selects planting date between April 23-30
flattened_garden <- lapply(years, function(y) {
  grid <- expand.grid(x = 1:10, y = 1:10)[1:entries_per_year, ]
  plant_choices <- sample(plants, entries_per_year, replace = TRUE)
  planting_dates <- sample(seq(as.Date(paste0(y, "-04-23")), as.Date(paste0(y, "-04-30")), by = "day"), entries_per_year, replace = TRUE)
#Iterates through each of the 100 entries per year, simulates planting, harvesting, and fertilization,
#randomly samples sun exposure within plants required range, simulates waterering time using plants average
  lapply(1:entries_per_year, function(i) {
    plant <- plant_choices[i]
    planting_date <- planting_dates[i]
    sun <- sample(min_sun[plant]:max_sun[plant], 1)
    water <- round(rnorm(1, mean = water_schedule[plant], sd = 5))
    
#Randomly chooses 1-3 harvests per plant, first harvest after plants average maturation time, random gaps between harvests (10-30 days),
#cumulative spacing added to the start date, simulates yield weight using normal distribution
    num_harvests <- sample(1:3, 1)
    harvest_start <- planting_date + days_harvest[plant]
    harvest_spacing <- sample(10:30, num_harvests, replace = TRUE)
    harvest_dates <- harvest_start + cumsum(harvest_spacing)
    harvest_weights <- round(rnorm(num_harvests, mean = yield_weight[plant], sd = 50))
    
#Fertilization occurs every 30 days from planting until harvest
#early fertilization has higher nitrogen and phosphorus, while potassium remains constant, 
#later fertilizations slightly reduced levels
    fert_days <- seq(0, days_harvest[plant], by = 30)
    fert_dates <- planting_date + fert_days
    fert_N <- ifelse(fert_days <= 30, 5, 4)
    fert_P <- ifelse(fert_days <= 30, 7, 9)
    fert_K <- rep(3, length(fert_days))
    
#Ensures that harvest and fertilization data align in row count, repeats values to match longest vector
    max_rows <- max(length(harvest_dates), length(fert_dates))
    harvest_dates <- rep(harvest_dates, length.out = max_rows)
    harvest_weights <- rep(harvest_weights, length.out = max_rows)
    fert_dates <- rep(fert_dates, length.out = max_rows)
    fert_N <- rep(fert_N, length.out = max_rows)
    fert_P <- rep(fert_P, length.out = max_rows)
    fert_K <- rep(fert_K, length.out = max_rows)
#Constructs final data frame     
    data.frame(
      year = y,
      x = grid$x[i],
      y = grid$y[i],
      plant = plant,
      planting_date = planting_date,
      Full_Sun_Exposure = sun,
      Water_Schedule = water,
      Harvest_Date = harvest_dates,
      Yield_grams = harvest_weights,
      Fert_Date = fert_dates,
      N = fert_N,
      P = fert_P,
      K = fert_K
    )
  }) %>% bind_rows()
}) %>% bind_rows()
#Save data frame as RDS
saveRDS(flattened_garden, file = "synthetic_garden.rds")
