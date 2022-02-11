
library(tidyverse)
library(demest)
library(pxweb)


## Download and reformat non-birth data -------------------------------------------------

url <- "https://betabank20.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALCV21.PX"

query <- list("year of birth" = "*",
              "place of birth" = "*",
              gender = c("M", "K"),
              "triangles(lexis)" = "*",
              event = "*",
              time = "*")

data_raw <- pxweb_get_data(url = url, query = query) %>%
    as.data.frame(column.name.type = "text",
                  variable.value.type = "text") %>%
    tibble()



## Download and reformat birth data -------------------------------------------------

url <- "https://betabank20.stat.gl/api/v1/en/Greenland/BE/BE80/BEXFERTR.PX"

query <- list("mother's age" = "*",
              "mother's year of birth" = "*",
              "mother's place of birth" = "*",
              gender = c("M", "K"),
              area = "ALL",
              time = "*")

data_births_raw <- pxweb_get_data(url = url, query = query) %>%
    as.data.frame(column.name.type = "text",
                  variable.value.type = "text") %>%
    tibble()


## Initial reformatting - non-birth data -------------------------------------------------

data_reformatted <- data_raw  %>%
  select(cohort = "year of birth",
         gender,
         triangle = "triangles(lexis)",
         event,
         time,
         count = "Population Accounts") %>%
  mutate(cohort = as.integer(as.character(cohort)),
         time = as.integer(as.character(time))) %>%
  mutate(age = if_else(event == "Population (end of year)", # uses fact that population counts are at end of year
                       time - cohort,
                       time - cohort - (triangle == "Upper"))) %>%
    filter(!is.na(count) & (count > 0)) %>%
    filter(age >= 0)

max_age <- data_reformatted %>%
    pull(age) %>%
    max()
level_max_age <- paste0(max_age, "+")
levels_age <- c(0:(max_age - 1), level_max_age)

levels_age_birth <- data_reformatted %>%
    filter(event == "Birth") %>%
    pull(age) %>%
    unique()

data_reformatted <- data_reformatted %>%
    mutate(age = ifelse(age == max_age, level_max_age, age),
           age = factor(age, levels = levels_age))


## Initial reformatting - births data -------------------------------------------------


data_births_reformatted <- data_births_raw  %>%
    select(cohort = "mother's year of birth",
           age = "mother's age",
           gender,
           time,
           count = "Live births by Greenland's administrative division") %>%
    filter(!is.na(count) & (count > 0)) %>%
    mutate(cohort = as.integer(as.character(cohort)),
           age = as.integer(as.character(age)),
           time = as.integer(as.character(time))) %>%
    mutate(triangle = case_when(age == time - cohort ~ "Lower",
                                age == time - cohort - 1L ~ "Upper",
                                TRUE ~ "Invalid")) %>%
    mutate(age = factor(age, levels = seq(from = min(age), to = max(age))))

## There are 20 cases where the triangle is invalid, but the misclassification
## doesn't affect the accounting, so we convert them to "Upper"

data_births_reformatted <- data_births_reformatted %>%
    mutate(triangle = if_else(triangle == "Invalid", "Upper", triangle))


## Get first year for which we have immigration data --------------------------

min_time_immig <- data_reformatted %>%
    filter(event == "Immigration") %>%
    pull(time) %>%
    min()

## Year to impute -------------------------------------------------------------

year_to_impute <- 1992

## Make components of demographic account -------------------------------------

population_reg <- data_reformatted %>%
    filter(event == "Population (end of year)") %>%
    filter(time >= min_time_immig - 1) %>%
    dtabs(count ~ age + gender + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Points"))

births_reg <- data_births_reformatted %>%
    filter(time >= min_time_immig) %>%
    dtabs(count ~ age + gender + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

deaths_reg <- data_reformatted %>%
    filter(event == "Death") %>%
    filter(time >= min_time_immig) %>%
    dtabs(count ~ age + gender + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

immigration_reg <- data_reformatted %>%
    filter(event == "Immigration") %>%
    mutate(time = factor(time, levels = seq(from = min(time), to = max(time)))) %>%
    complete(age, gender, triangle, time, fill = list(count = 0)) %>%
    mutate(count = if_else(time == year_to_impute, NA_real_, count)) %>%
    dtabs(count ~ age + gender + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))

emigration_reg <- data_reformatted %>%
    filter(event == "Emigration") %>%
    mutate(time = factor(time, levels = seq(from = min(time), to = max(time)))) %>%
    complete(age, gender, triangle, time, fill = list(count = 0)) %>%
    mutate(count = if_else(time == year_to_impute, NA_real_, count)) %>%
    dtabs(count ~ age + gender + triangle + time) %>%
    Counts(dimscales = c(age = "Intervals", time = "Intervals"))


## Make demographic account ---------------------------------------------------

set.seed(0)

immigration_init <- impute(immigration_reg)
emigration_init <- impute(emigration_reg)

account <- Movements(population = population_reg,
                     births = births_reg,
                     entries = list(immigration = immigration_init),
                     exits = list(deaths = deaths_reg,
                                  emigration = emigration_reg)) %>%
    makeConsistent()

system_models  <- list(Model(population ~ Poisson(mean ~ age * gender + time, useExpose = FALSE)),
                       Model(births ~ Poisson(mean ~ age * gender + time)),
                       Model(deaths ~ Poisson(mean ~ age * gender + time)),
                       Model(immigration ~ Poisson(mean ~ age * gender + time)),
                       Model(emigration ~ Poisson(mean ~ age * gender + time)))

data_models <- list(Model(population_reg ~ PoissonBinomial(prob = 0.99), series = "population"),
                    Model(births_reg ~ PoissonBinomial(prob = 0.99), series = "births"),
                    Model(deaths_reg ~ PoissonBinomial(prob = 0.99), series = "deaths"),
                    Model(immigration_reg ~ PoissonBinomial(prob = 0.99), series = "immigration"),
                    Model(emigration_reg ~ PoissonBinomial(prob = 0.99), series = "emigration"))

datasets <- list(population_reg = population_reg,
                 births_reg = births_reg,
                 deaths_reg = deaths_reg,
                 immigration_reg = immigration_reg,
                 emigration_reg = emigration_reg)

filename <- "model.est"
estimateAccount(account = account,
                systemModels = system_models,
                dataModels = data_models,
                datasets = datasets,
                filename = filename,
                nBurnin = 1000,
                nSim = 1000,
                nThin = 5)
options(width = 120)
fetchSummary(filename)


## Extract 

im <- fetch(filename, where = c("account", "immigration")) %>%
    subarray(time == as.character(year_to_impute)) %>%
    collapseIterations(FUN = median) %>%
    round()

em <- fetch(filename, where = c("account", "emigration")) %>%
    subarray(time == as.character(year_to_impute)) %>%
    collapseIterations(FUN = median) %>%
    round()

dplot(~ time | age, fetch(filename, c("ac", "em")))
    
