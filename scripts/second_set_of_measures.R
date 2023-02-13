library(tidyverse)

tpw <- read_csv("C:/Users/cpppe/Desktop/github_projects/TurningPoints4Women/do_not_push/tpw_data.csv") %>%
    janitor::clean_names() %>%
  mutate(
    id = assessment_screen_2
  )

source("C:/Users/cpppe/Desktop/github_projects/TurningPoints4Women/functions/codebook_fun.R")

names(tpw)[1:1000]
names(tpw)[1000:2000]
names(tpw)[2000:2006]


mhc <-
  tpw %>%
  select(
    id,
    matches(
      "^h"
    )
  )

mhc <-
  mhc %>%
  subtract_scale(
    id = 'id',
    value = 1
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 6 ~ -77,
        .x == 7 ~ NA_real_,
        is.na(.x) ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

mhc %>%
  pct_miss_fun(
    id = "id",
    n_items = 14
  )

mhc_calc <-
  mhc %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

mhc_calc


mhc_complete <-
  mhc_calc %>%
  composite_total_avg_fun(
    id = 'id',
    max_value = 5,
    n_items = 14
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  subscale_create(
    total_only = FALSE,
    scale1 = c("h1", "h2", "h3"),
    scale1_nitems = 3,
    scale2 = c("h4", "h5", "h6", "h7", "h8"),
    scale2_nitems = 5,
    scale3 = c("h9", "h10", "h11", "h12", "h13", "h14"),
    scale3_nitems = 6
  ) %>%
  rename(
    mhc_total = sum_values,
    mhc_avg = mean_values,
    mhc_pa_total = total1,
    mhc_pa_avg = avg1,
    mhc_swb_total = total2,
    mhc_swb_avg = avg2,
    mhc_pwb_total = total3,
    mhc_pwb_avg = avg3
  )

mhc_complete %>%
  composite_hist(
    x = mhc_total
  ) +
  labs(title = 'Distribution of Total Scores for MHC-SF Measure')

mhc_complete %>%
  composite_hist(
    x = mhc_pa_total
  ) +
  labs(title = 'Distribution of Total Scores for MHC-SF Positive Affect Subscale')

mhc_complete %>%
  composite_hist(
    x = mhc_swb_total
  ) +
  labs(title = 'Distribution of Total Scores for MHC-SF Social Well-being Subscale')

mhc_complete %>%
  composite_hist(
    x = mhc_pwb_total
  ) +
  labs(title = 'Distribution of Total Scores for MHC-SF Psychological Well-being Subscale')


mhc_alpha <-
  mhc_complete %>%
  select(h1:h14) %>%
  psych::alpha(check.keys = TRUE)

mhc_pa_alpha <-
  mhc_complete %>%
  select(h1:h3) %>%
  psych::alpha(check.keys = TRUE)

mhc_swb_alpha <-
  mhc_complete %>%
  select(h4:h8) %>%
  psych::alpha(check.keys = TRUE)

mhc_pwb_alpha <-
  mhc_complete %>%
  select(h9:h14) %>%
  psych::alpha(check.keys = TRUE)

mhc_alpha_table <-
  tibble(
  Scale = c('MHC-SF',
            'MHC-SF - Positive Affect',
            'MHC-SF - Social Well-being',
            'MHC-SF - Psychological well-being'),
  Alpha = c(round(mhc_alpha$total$raw_alpha, 3),
            round(mhc_pa_alpha$total$raw_alpha, 3),
            round(mhc_swb_alpha$total$raw_alpha, 3),
            round(mhc_pwb_alpha$total$raw_alpha, 3))
)

mhc_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for MHC-SF Entire Scale & Subscales'
  )


# ell measure (self)

ell <-
  tpw %>%
  select(
    id,
    ell1:ell39a
  ) %>%
  rename(
    ell35a  = x35a
  ) %>%
  mutate(
    across(
      matches("a$"),
      ~case_when(
        .x == 7 ~ -77,
        .x == 8 ~ NA_real_,
        is.na(.x) ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

# most participants will respond to the first part of each item
# don't respond to anything on the scale
ell %>%
  pct_miss_fun(
    id = "id",
    n_items = 39
  )




# elliott delinquency (peer)
# reversed all so 1 = "none" and 5 = "all" now

elf <-
  tpw %>%
  select(
    id,
    matches(
      "^elf"
    )
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 1 ~ 5,
        .x == 2 ~ 4,
        .x == 3 ~ 3,
        .x == 4 ~ 2,
        .x == 5 ~ 1,
        .x == 6 ~ 6,
        .x == 7 ~ -77,
        .x == 8 ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  )

elf %>%
  pct_miss_fun(
    id = "id",
    n_items = 39
  )

elf_calc <-
  elf %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        .x == 6 ~ 0,
        TRUE ~ .x
      )
    )
  )

elf_calc


elf_complete <-
  elf_calc %>%
  composite_total_avg_fun(
    id = 'id',
    max_value = 5,
    n_items = 39
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  )
  # subscale_create(
  #   total_only = FALSE,
  #   scale1 = c("h1", "h2", "h3"),
  #   scale1_nitems = 3,
  #   scale2 = c("h4", "h5", "h6", "h7", "h8"),
  #   scale2_nitems = 5,
  #   scale3 = c("h9", "h10", "h11", "h12", "h13", "h14"),
  #   scale3_nitems = 6
  # ) %>%
  # rename(
  #   mhc_total = sum_values,
  #   mhc_avg = mean_values,
  #   mhc_pa_total = total1,
  #   mhc_pa_avg = avg1,
  #   mhc_swb_total = total2,
  #   mhc_swb_avg = avg2,
  #   mhc_pwb_total = total3,
  #   mhc_pwb_avg = avg3
  # )

elf_complete %>%
  composite_hist(
    x = sum_values #elf_total
  ) +
  labs(title = 'Distribution of Total Scores for MHC-SF Measure')


# flourishing index

flo <- tpw %>%
  select(
    id,
    i1:i12
  ) %>%
  subtract_scale(
    id = "id",
    value = 1
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 11 ~ -77,
        .x == 12 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

flo %>%
  pct_miss_fun(
    id = "id",
    n_items = 12
  )

flo_calc <-
  flo %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

flo_calc


flo_complete <-
  flo_calc %>%
  composite_total_avg_fun(
    id = 'id',
    max_value = 10,
    n_items = 12
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  select(
    -mean_values
  ) %>%
  rename(flo_total = sum_values)


flo_complete %>%
  composite_hist(
    x = flo_total
  ) +
  labs(title = 'Distribution of Total Scores for Flourishing Index')
