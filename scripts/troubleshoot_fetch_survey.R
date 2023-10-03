library(tidyverse)

tpw111821 <- qualtRics::fetch_survey(
  surveyID = 'SV_23LMOyXcFo2JqQu',
  force_request = TRUE
)

df21 <- map(tpw111821, ~attributes(.x)$problems)

df21 <- df21[!unlist(lapply(df21, is.null))]

df <- tibble(
  variable = map(1:77, ~names(df21[.x])),
  row = map(df21, ~.x$row),
  what_you_got = map(df21, ~.x$expected),
  actual = map(df21, ~.x$actual)
)

df <- df |> unnest(variable)

tpw <- tpw111821 |>
  select(
    recipient_first_name = RecipientFirstName,
    assessment_screen2 = `Assessment Screen 2`,
    as.vector(df$variable)
  ) |>
  rowid_to_column()


df <- df |> unnest(c(row, what_you_got, actual))


tpw_long <- tpw |>
  mutate(
    across(
      -c(
     rowid,
     recipient_first_name,
     assessment_screen2 
    ),
    ~as.character(.x)
    )
  ) |>
  pivot_longer(
    -c(
     rowid,
     recipient_first_name,
     assessment_screen2 
    ),
    names_to = "variable",
    values_to = "actual"
  ) |>
  rename(row = rowid) |>
  drop_na(actual)


head(tpw_long)
head(df)

tpw_long |>
  filter(variable == "P01b6")

df |>
  filter(variable == "P01b6")

full_join(
  tpw_long,
  df
)
