# map over a bunch of charts


library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
iris0 <- iris %>%  
  group_by(Species) %>%  
  nest() %>%  
  mutate(
    gg1 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Sepal.Width)) + geom_point()),
    gg2 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Petal.Width)) + geom_point()),
    gg3 = purrr::map(data, ~ ggplot(., aes(Sepal.Length, Petal.Length)) + geom_point()),
    g = purrr::pmap(list(gg1, gg2, gg3), ~ gridExtra::grid.arrange(..1, ..2, ..3))
  )