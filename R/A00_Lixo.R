# Packages ----------------------------------------------------------------


library(finalfit)
library(gt)
library(tidyverse)



# Data --------------------------------------------------------------------


data_surv_broca <-
  readxl::read_excel('Data/Data_All_collections.xlsx',
                     sheet = "Broca")


data_surv_broca <- data_surv_broca |>
  mutate_at(vars(Collection:Area_Type), as_factor) |>
  mutate_at(vars(Time),
            ~ case_match(.,
                         NA ~ as.integer(as_datetime("2023-08-02") - Initial_Date),
                         .default = as.integer(.))) |>
  mutate_at(vars(Status), as.integer)


data_surv_broca |>
  add_column(Insect = factor("Broca")) ->
  data_surv_broca



data_surv_tenebrio <-
  readxl::read_excel('Data/Data_All_collections.xlsx',
                     sheet = "Tenebrio")



data_surv_tenebrio <- data_surv_tenebrio |>
  mutate_at(vars(Collection:Area_Type), as_factor) |>
  mutate_at(vars(Time),
            ~ case_match(.,
                         NA ~ as.integer(as_datetime("2023-08-02") - Initial_Date),
                         .default = as.integer(.))) |>
  mutate_at(vars(Status), as.integer)



data_surv_tenebrio |>
  add_column(Insect = factor("Tenebrio")) ->
  data_surv_tenebrio


data_surv_broca |>
  bind_rows(data_surv_tenebrio) |>
  mutate(Collection_Int = as.integer(as.character(Collection))) |>
  mutate(Area_Type = factor(Area_Type,
                            labels = c(
                              "Conventional",
                              "Agroforestry",
                              "Organic"
                            ))) |>
  arrange(Area_Type) |>
  as.data.frame() ->
  df



# Cox model 2 -------------------------------------------------------------


# Outra forma de fazer o modelo cox
dependent  <- "Surv(time = Time, event = Status)"
explanatory   <- c("Area_Type", "Insect", "strata(Collection_Int)")

df %>%
  finalfit(dependent, explanatory) |>
  gt::gt()


df %>%
  coxphuni(dependent, explanatory) %>%
  finalfit::fit2df() -> coxphdf


coxphdf

df |>
  hr_plot(dependent, explanatory)

