library(afex)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)


ChickWeight |> head()

agg <- ChickWeight |>
  group_by(Chick, Diet, Time) |>
  summarise(weight = mean(weight))

my_aov <- aov_ez(
  id = "Chick",
  dv = "weight",
  data = agg,
  within = c("Time"),
  between = c("Diet"),
  detailed = TRUE,
  return_aov = TRUE
)

str(my_aov)

my_aov$anova_table["Diet", ]$"num Df"

glue("F({my_aov$anova_table['Diet', ]$'num Df'})")
