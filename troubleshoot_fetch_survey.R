library(tidyverse)

# tpw111821 <- qualtRics::fetch_survey(stuff in here)

df21 <- map(tpw111821, ~attributes(.x)$problems)


df21 <- df21[!unlist(lapply(df21, is.null))]

names(df21[1])

data_tibble <- tibble(
  variable = map(1:77, ~names(df21[.x])),
  row = map(df21, ~.x$row),
  column = map(df21, ~.x$col),
  expected = map(df21, ~.x$expected),
  actual = map(df21, ~.x$actual)
) |>
  mutate(variable = unlist(variable))
