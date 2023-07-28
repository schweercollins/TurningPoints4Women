library(tidyverse)

source("C:/Users/cpppe/Desktop/github_projects/TurningPoints4Women/functions/codebook_fun.R")


# no bio data now has a P831 and P831B - JP
no_bio <- read_csv("do_not_push/no_bio_data.csv") %>%
  filter(
    id != 'AG400',
    id != 'K000',
    id != 'F000',
    id != 'P100',
    id != '65973269'
  )


# keeping the bio data separate, but if you'd like to join them; `full_join(no_bio, bio, by = "id")` can be used
bio <- read_csv("do_not_push/bio_data.csv") %>%
  filter(
    id != 'AG400',
    id != 'K000',
    id != 'F000',
    id != 'P100',
    id != '65973269'
  )


# removing doubles of p831 from data
p831_inter <- no_bio %>%
  filter(id == 'P831') %>%
  filter(assessment_screen == '11/30/2021')

no_bio <-
  no_bio %>%
  filter(id != 'P831')

no_bio <-
  full_join(no_bio, p831_inter)

# bio data has a double (HR171) -JP
bio %>%
  count(id) %>%
  filter(n > 1)

# arq
arq <- no_bio %>%
  select(
    id,
    rq1:rqd
  ) %>%
  mutate(
    rq1 = case_when(
      rq1 == 5 ~ -77,
      rq1 == 6 ~ NA_real_,
      TRUE ~ rq1
    ),
    across(
      c(
        rqa,
        rqb,
        rqc,
        rqd
      ),
      ~case_when(
        .x == 8 ~ -77,
        .x == 9 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

arq_calc <-
  arq %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

arq_miss <- arq %>%
  pct_miss_fun(
    id = "id",
    n_items = 5
  )

arq_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "ARQ Missing Data",
    subtitle = "By Participant"
  )

arq_miss %>%
  filter(
    miss_pct > 33
  )

arq_remove <- arq_miss %>% filter(miss_pct > 33) %>% pull(id)

map2(
  arq_calc %>%
    filter(!id %in% arq_remove) %>%
    select(-id) %>%
    drop_na(),
  arq_calc %>%
    filter(!id %in% arq_remove) %>%
    select(-id) %>%
    drop_na() %>%
    names(),
  ~ggplot(
    arq_calc %>%
      filter(!id %in% arq_remove) %>%
      select(-id) %>%
      drop_na(),
    aes(
      as.factor(.x)
    )
  ) +
    geom_bar(
      color = "white"
    ) +
    coord_flip() +
    labs(
      title = glue::glue(
        "Distribution for {.y}"
      ),
      x = glue::glue(
        "{.y}"
      )
    )
)

# PRSS
prss <- no_bio %>%
  select(
    id,
    ps9:ps13
  ) %>%
  subtract_scale(
    id = "id",
    value = 1
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 5 ~ -77,
        .x == 6 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

prss_miss <- prss %>%
  pct_miss_fun(
    id = "id",
    n_items = 5
  )

prss_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'PRSS Missing Data',
    subtitle = 'By Participant'
  )

prss_remove <- prss_miss %>% filter(miss_pct >= 20) %>% pull(id)

prss_calc <-
  prss %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

prss_complete <-
  prss_calc %>%
  composite_total_avg_fun(
    id = 'id',
    max_value = 5,
    n_items = 5
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  rename(
    prss_total = sum_values,
    prss_avg = mean_values
  ) %>%
  mutate(
    across(
      c(
        prss_total,
        prss_avg
      ),
      ~case_when(
        id %in% prss_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

prss_complete %>%
  composite_hist(
    x = prss_avg
  ) +
  labs(title = "Distribution of Average Scores for the PRSS")

prss_alpha <- prss_complete %>%
  select(ps9:ps13) %>%
  psych::alpha(check.keys = TRUE)

prss_alpha_table <- tibble(
  Scale = "Posttraumatic Risk Seeking Scale",
  Alpha = round(prss_alpha$total$raw_alpha, 3)
)

prss_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for PRSS Scale'
  )

# SSQ
ssq <- no_bio %>%
  select(
    id,
    ps1:ps8
  ) %>%
  subtract_scale(
    id = "id",
    value = 1
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 5 ~ -77,
        .x == 6 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

ssq_calc <-
  ssq %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

ssq_miss <- ssq %>%
  pct_miss_fun(
    id = "id",
    n_items = 8
  )

ssq_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "SSQ Missing Data",
    subtitle = "By Participant"
  )

ssq_remove <- ssq_miss %>% filter(miss_pct >= 30) %>% pull(id)


ssq_complete <-
  ssq_calc %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c("ps1", "ps2", "ps3", "ps4"),
    scale1_nitems = 4,
    scale2 = c("ps1", "ps2", "ps3", "ps4",
               "ps5", "ps6", "ps7", "ps8"),
    scale2_nitems = 8
  ) %>%
  rename(
    ssq_abuse_shame = total1,
    ssq_total = total2
  ) %>%
  mutate(
    across(
      c(
        ssq_abuse_shame,
        ssq_total
      ),
      ~case_when(
        id %in% ssq_remove ~ NA_real_,
        TRUE ~ .x
      )
    ),
    ssq_total_cutoff = case_when(
      ssq_total >= 16 ~ "high shame",
      ssq_total <= 15 ~ "low shame"
    )
  )

ssq_complete %>%
  composite_hist(
    x = ssq_total
  ) +
  labs(title = "Distribution of Total Scores for the Stigma & Shame Questionnaire")

ssq_complete %>%
  cutoff_plot(
    x = ssq_total,
    cutoff = 16
  ) +
  labs(
    title = "Cutoff Scores for SSQ Total Scores",
    caption = "Cutoff is 16\nSee reference for literature on cutoff score."
  )

ssq_complete %>%
  cutoff_plot(
    x = ssq_abuse_shame,
    cutoff = 3
  ) +
  labs(
    title = "Cutoff Scores for SSQ Abuse-related Shame Scores",
    caption = "Cutoff is 3\nSee reference for literature on cutoff score."
  )

ssq_abuse_shame_alpha <- ssq_complete %>%
  select(
    ps1:ps4
  ) %>%
  psych::alpha(check.keys = TRUE)

ssq_alpha <- ssq_complete %>%
  select(
    ps1:ps8
  ) %>%
  psych::alpha(check.keys = TRUE)

ssq_alpha_table <- tibble(
  Scale = c(
    "SSQ",
    "SSQ - Abuse-related Shame"
  ),
  Alpha = c(
    round(ssq_alpha$total$raw_alpha, 3),
    round(ssq_abuse_shame_alpha$total$raw_alpha, 3)
  )
)

ssq_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for SSQ Entire Scale & Subscales'
  )

# EDS
elf <-
  no_bio %>%
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
        .x == 1 ~ 4,
        .x == 2 ~ 3,
        .x == 3 ~ 2,
        .x == 4 ~ 1,
        .x == 5 ~ 0,
        .x == 6 ~ 0,
        .x == 7 ~ -77,
        .x == 8 ~ NA_real_,
        TRUE ~ NA_real_
      ),
      .names = "{.col}_c"
    )
  )

elf_calc <-
  elf %>%
  mutate(
    across(
      .cols = -c(
        id,
        matches("//d$")
      ),
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )


elf_miss <- elf %>%
  select(
    id,
    matches(
      "_c$"
    )
  ) %>%
  pct_miss_fun(
    id = "id",
    n_items = 39
  )

elf_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "EDS Missing Data",
    subtitle = "By Participant"
  )

elf_remove <- elf_miss %>% filter(miss_pct >= 20) %>% pull(id)

elf_complete <-
  elf_calc %>%
  select(
    id,
    matches(
      "_c$"
    )
  ) %>%
  composite_total_avg_fun(
    id = c(
      'id', "elf1_c", "elf2_c", "elf3_c", "elf7_c",
      "elf12_c", "elf13_c", "elf15_c", "elf17_c", "elf18_c",
      "elf19_c", "elf23_c", "elf26_c", "elf28_c", "elf30_c",
      "elf34_c", "elf35_c", "elf36_c", "elf37_c"
    ),
    max_value = 4,
    n_items = 21
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  rename(
    elf_total = sum_values,
    elf_avg = mean_values
  ) %>%
  mutate(
    mutate(
      across(
        c(
          elf_total,
          elf_avg
        ),
        ~case_when(
          id %in% elf_remove ~ NA_real_,
          TRUE ~ .x
        )
      )
    )
  )


elf_complete %>%
  composite_hist(
    x = elf_total
  ) +
  labs(title = 'Distribution of Total Scores for Elliott Peer Delinquency Scale')

elf_alpha <- elf_complete %>%
  select(
    matches(
      "_c$"
    )
  ) %>%
  drop_na() %>%
  psych::alpha(
    check.keys = TRUE
  )

elf_alpha_table <- tibble(
  Scale = "Elliott Peer Delinquency Scale",
  Alpha = round(elf_alpha$total$raw_alpha, 3)
)

elf_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Alpha Values for Elliott Peer Delinquency Scale"
  )

#Elliott Self
ell <-
  no_bio %>%
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

ell_calc <-
  ell %>%
  select(
    id,
    matches("a$")
  ) %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    ),
    across(
      -id,
      .names = "{.col}a"
    ),
    across(
      c(
        matches("a$"),
        matches("aa$")
      ),
      ~case_when(
        .x == 1 ~ 0,
        .x == 2 ~ 1,
        .x == 3 ~ 2,
        .x == 4 ~ 3,
        .x == 5 ~ 4,
        .x == 6 ~ 5,
        .x == 7 ~ -77,
        .x == 8 ~ NA_real_,
        TRUE ~ NA_real_
      )
    ),
    across(
      c(
        matches("aa$")
      ),
      ~case_when(
        .x >= 4 ~ 4,
        TRUE ~ .x
      )
    )
  )

ell_count_calc <-
  ell %>%
  select(
    id,
    matches("\\d$")
    ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 0 ~ 0,
        .x > 0 ~ 1,
        .x == -77 ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  )


ell_count_miss <- ell_count_calc %>%
  pct_miss_fun(
    id = "id",
    n_items = 39
  )

ell_count_miss %>%
  gt::gt() %>%
  gt::tab_header(title = 'Missing Data',
                 subtitle = 'By Each Participant')

ell_count_remove <- ell_count_miss %>% filter(miss_pct >= 20) %>% pull(id)

ell_complete <-
  ell_calc %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c(
      "ell4a", "ell5a", "ell6a", "ell8a", "ell9a",
      "ell10a", "ell11a", "ell14a", "ell16a", "ell20a",
      "ell21a", "ell22a", "ell24a", "ell25a", "ell27a", "ell29a",
      "ell31a", "ell32a", "ell33a", "ell38a", "ell39a"
    ),
    scale2 = c(
      "ell4aa", "ell5aa", "ell6aa", "ell8aa", "ell9aa",
      "ell10aa", "ell11aa", "ell14aa", "ell16aa", "ell20aa",
      "ell21aa", "ell22aa", "ell24aa", "ell25aa", "ell27aa", "ell29aa",
      "ell31aa", "ell32aa", "ell33aa", "ell38aa", "ell39aa"
    )
  ) %>%
  rename(
    ell_raw_total = total1,
    ell_censor_total = total2
  )


ell_count_complete <-
  ell_count_calc %>%
  composite_total_avg_fun(
    id = c(
      'id', "ell1", "ell2", "ell3", "ell7",
      "ell12", "ell13", "ell15", "ell17", "ell18",
      "ell19", "ell23", "ell26", "ell28", "ell30",
      "ell34", "ell35", "ell36", "ell37"
    ),
    max_value = 1,
    n_items = 21
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  rename(
    ell_count_total = sum_values,
    ell_count_avg = mean_values
  ) %>%
  mutate(
    across(
      c(
        ell_count_total,
        ell_count_avg
      ),
      ~case_when(
        id %in% ell_count_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

ell_complete %>%
  composite_hist(
    x = ell_raw_total
  ) +
  labs(title = 'Distribution of Total Scores for Elliott Self Delinquency Scale (Raw)')

ell_complete %>%
  composite_hist(
    x = ell_censor_total
  ) +
  labs(title = 'Distribution of Total Scores for Elliott Self Delinquency Scale (Censored)')

ell_count_complete %>%
  composite_hist(
    x = ell_count_total
  ) +
  labs(title = "Distribution of Total Scores for Elliott Self Delinquency Scale (Binary Count Data)")


# Mental Health Continuum - SF
mhc <-
  no_bio %>%
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

mhc_miss <- mhc %>%
  pct_miss_fun(
    id = "id",
    n_items = 14
  )

mhc_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Missing Data',
    subtitle = 'By Each Participant'
  )

mhc_remove <- mhc_miss %>% filter(miss_pct >= 20) %>% pull(id)

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
  ) %>%
  mutate(
    across(
      c(
        mhc_total,
        mhc_avg,
        mhc_pa_total,
        mhc_pa_avg,
        mhc_swb_total,
        mhc_swb_avg,
        mhc_pwb_total,
        mhc_pwb_avg
      ),
      ~case_when(
        id %in% mhc_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

mhc_complete <- mhc_complete %>%
  pivot_longer(
    h4:h14,
    names_to = "pre_flourish",
    values_to = "flourish_feel"
  ) %>%
  mutate(
    flourish = case_when(
      flourish_feel %in% c(4, 5) ~ 1,
      !flourish_feel %in% c(4, 5) ~ 0,
      TRUE ~ 0
    ),
    languish = case_when(
      flourish_feel %in% c(0, 1) ~ 1,
      !flourish_feel %in% c(0, 1) ~ 0,
      TRUE ~ 0
    )
  ) %>%
  group_by(id) %>%
  mutate(
    six_or_over = sum(flourish, na.rm = TRUE),
    neg_six_or_more = sum(languish, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    flourish_diagnosis = case_when(
      (h1 %in% c(4, 5) & six_or_over >= 6) |
        (h2 %in% c(4, 5) & six_or_over >= 6) |
        (h3 %in% c(4, 5) & six_or_over >= 6)
      ~ "flourish",
      TRUE ~ "not_flourish"
    ),
    languish_diagnosis = case_when(
      (h1 %in% c(0, 1) & neg_six_or_more >= 6) |
        (h2 %in% c(0, 1) & neg_six_or_more >= 6) |
        (h3 %in% c(0, 1) & neg_six_or_more >= 6)
      ~ "languish",
      TRUE ~ "not_languish"
    )
  ) %>%
  pivot_wider(
    names_from = pre_flourish,
    values_from = flourish_feel
  ) %>%
  distinct(
    id,
    .keep_all = TRUE
  ) %>%
  mutate(
    final_diagnosis = case_when(
      (flourish_diagnosis == "flourish" &
         languish_diagnosis == "not_languish") ~ "flourish",
      (flourish_diagnosis == "not_flourish" &
         languish_diagnosis == "languish") ~ "languish",
      (flourish_diagnosis == "not_flourish" &
         languish_diagnosis == "not_languish") ~ "moderately mentally healthy"
    )
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

mhc_complete %>%
  ggplot(
    aes(
      final_diagnosis
    )
  ) +
  geom_bar(
    aes(
      fill = final_diagnosis
    )
  ) +
  labs(
    x = "",
    y = "Count",
    title = "Counts for the Three Categorical\n Diagnoses for the MHC-SF"
  ) +
  theme(
    legend.position = "none"
  )


mhc_alpha <-
  mhc_complete %>%
  select(h1:h3,
         h4:h14) %>%
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

# Flourishing Index
flo <- no_bio %>%
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

flo_miss <- flo %>%
  pct_miss_fun(
    id = "id",
    n_items = 12
  )

flo_miss %>%
  gt::gt() %>%
  gt::tab_header(title = 'Missing Data',
                 subtitle = 'By Each Participant')

flo_remove <- flo_miss %>% filter(miss_pct >= 20) %>% pull(id)

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
  rename(flo_total = sum_values) %>%
  mutate(
    across(
      c(
        flo_total
      ),
      ~case_when(
        id %in% flo_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )


flo_complete %>%
  composite_hist(
    x = flo_total
  ) +
  labs(title = 'Distribution of Total Scores for Flourishing Index')

flo_alpha <- flo_complete %>%
  select(i1:i12) %>%
  psych::alpha(check.keys = TRUE)

flo_alpha_table <-
  tibble(
    Scale = c('Flourishing Indes'),
    Alpha = c(round(flo_alpha$total$raw_alpha, 3))
  )

flo_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for Flourishing Index'
  )

# ITQ
itq <- no_bio %>%
  select(
    id,
    matches(
      "^itq"
    )
  ) %>%
  subtract_scale(
    id = "id",
    value = 1
  ) %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == 5 ~ -77,
        .x == 6 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )



itq_calc <- itq %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

itq_calc <- itq_calc %>%
  mutate(
    itq_ad_dx = case_when(
      (itq1 > 2 | itq2 > 2) &
        (itq7 > 2 | itq8 > 2 | itq9 > 2) ~ "diagnosis_met",
      (itq1 <= 2 | itq2 <= 2) &
        (itq7 <= 2 | itq8 <= 2 | itq9 <= 2) ~ "diagnosis_not_met",
      TRUE ~ "conditions_not_met"
    ),
    itq_nsc_dx = case_when(
      (itq3 > 2 | itq4 > 2) &
        (itq7 > 2 | itq8 > 2 | itq9 > 2) ~ "diagnosis_met",
      (itq3 <= 2 | itq4 <= 2) &
        (itq7 <= 2 | itq8 <= 2 | itq9 <= 2) ~ "diagnosis_not_met",
      TRUE ~ "conditions_not_met"
    ),
    itq_dr_dx = case_when(
      (itq5 > 2 | itq6 > 2) &
        (itq7 > 2 | itq8 > 2 | itq9 > 2) ~ "diagnosis_met",
      (itq5 <= 2 | itq6 <= 2) &
        (itq7 <= 2 | itq8 <= 2 | itq9 <= 2) ~ "diagnosis_not_met",
      TRUE ~ "conditions_not_met"
    ),
    itq_dso_dx = case_when(
      (itq_ad_dx == "diagnosis_met" &
         itq_nsc_dx == "diagnosis_met" &
         itq_dr_dx == "diagnosis_met") ~ "diagnosis_met",
      TRUE ~ "diagnosis_not_met"
    )
  )

itq_miss <- itq %>%
  pct_miss_fun(
    id = "id",
    n_items = 9
  )

itq_miss %>%
  gt::gt() %>%
  gt::tab_header(title = 'Missing Data',
                 subtitle = 'By Each Participant')

itq_remove <- itq_miss %>% filter(miss_pct >= 20) %>% pull(id)

itq_complete <-
  itq_calc %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c("itq1", "itq2"),
    scale2 = c("itq3", "itq4"),
    scale3 = c("itq5", "itq6")
  ) %>%
  rename(
    itq_ad_total = total1,
    itq_nsc_total = total2,
    itq_dr_total = total3
  ) %>%
  mutate(
    itq_dso = (itq_ad_total + itq_nsc_total + itq_dr_total),
    across(
      c(
        itq_ad_total,
        itq_nsc_total,
        itq_dr_total
      ),
      ~case_when(
        id %in% itq_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

itq_complete %>%
  composite_hist(
    x = itq_ad_total,
    bins = 15
  ) +
  labs(title = "Distribution of Total Scores for the Affective Dysregulation Subscale\nin the International Trauma Questionnaire")

itq_complete %>%
  composite_hist(
    x = itq_nsc_total,
    bins = 15
  ) +
  labs(title = "Distribution of Total Scores for the Negative Self-Concept Subscale\nin the International Trauma Questionnaire")

itq_complete %>%
  composite_hist(
    x = itq_dr_total,
    bins = 15
  ) +
  labs(title = "Distribution of Total Scores for the Disturbances in Relationships Subscale\nin the International Trauma Questionnaire")


itq_complete %>%
  composite_hist(
    x = itq_dso,
    bins = 15
  ) +
  labs(title = "Distribution of Total Scores for the DSO Subscale\nin the International Trauma Questionnaire")


# sexual history
sex <- no_bio %>%
  select(
    id,
    s_pregnancy_screen:s48
  )

sex_calc <- sex %>%
  mutate(
    sex_preg_screen = case_when(
      s_pregnancy_screen == 1 ~ "Yes",
      s_pregnancy_screen == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    sex_current_date = case_when(
      s1 == 1 ~ "Yes",
      s1 == 2 ~ "No",
      s1 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_part_gender_id = case_when(
      s1a == 1 ~ "cisgender female",
      s1a == 2 ~ "female",
      s1a == 3 ~ "cisgender male",
      s1a == 4 ~ "male",
      s1a == 5 ~ "trans female",
      s1a == 6 ~ "tras male",
      s1a == 7 ~ "non-binary",
      s1a == 8 ~ "self-identify",
      s1a == 9 ~ NA_character_,
      s1a == 10 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    sex_current_engage = case_when(
      s3 == 1 ~ "Yes",
      s3 == 2 ~ "No",
      s3 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_current_live = case_when(
      s4 == 1 ~ "Yes",
      s4 == 2 ~ "No",
      s4 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_current_marry = case_when(
      s6 == 1 ~ "Yes",
      s6 == 2 ~ "No",
      s6 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_pressure_stress = case_when(
      s9 == 1 ~ "a great deal",
      s9 == 2 ~ "quite a lot",
      s9 == 3 ~ "some",
      s9 == 4 ~ "not much",
      s9 == 5 ~ "very little",
      s9 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_id_feeling = case_when(
      s10 == 1 ~ "heterosexual",
      s10 == 2 ~ "gay_lesbian",
      s10 == 3 ~ "bisexual",
      s10 == 4 ~ "Other",
      s10 == 5 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_talk_safesex = case_when(
      s11 == 1 ~ "Never",
      s11 == 2 ~ "Rarely",
      s11 == 3 ~ "Sometimes",
      s11 == 4 ~ "Often",
      s11 == 5 ~ "Very Often",
      s11 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_talk_comfort = case_when(
      s12 == 1 ~ "Very Comfortable",
      s12 == 2 ~ "Somewhat Comfortable",
      s12 == 3 ~ "Neutral",
      s12 == 4 ~ "Somewhat uncomfortable",
      s12 == 5 ~ "Very uncomfortable",
      s12 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_other_safesex = case_when(
      s13 == 1 ~ "Yes",
      s13 == 2 ~ "No",
      s13 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_other_person_safesex = case_when(
      s14 == 1 ~ "Friend",
      s14 == 2 ~ "Romantic Partner",
      s14 == 3 ~ "Sister/Brother",
      s14 == 4 ~ "Other Relative",
      s14 == 5 ~ "Counselor/Teacher/Doctor",
      s14 == 6 ~ "Parent",
      s14 == 7 ~ "Other",
      s14 == 8 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_talk_safesex_other_person = case_when(
      s16 == 1 ~ "Never",
      s16 == 2 ~ "Rarely",
      s16 == 3 ~ "Sometimes",
      s16 == 4 ~ "Often",
      s16 == 5 ~ "Very Often",
      s16 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_talk_comfort_other_person = case_when(
      s17 == 1 ~ "Very Comfortable",
      s17 == 2 ~ "Somewhat Comfortable",
      s17 == 3 ~ "Neutral",
      s17 == 4 ~ "Somewhat uncomfortable",
      s17 == 5 ~ "Very uncomfortable",
      s17 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_birthcontrol_sex_with = case_when(
      s18 == 1 ~ "Very Comfortable",
      s18 == 2 ~ "Somewhat Comfortable",
      s18 == 3 ~ "Neutral",
      s18 == 4 ~ "Somewhat Uncomfortable",
      s18 == 5 ~ "Very Uncomfortable",
      s18 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_safesex_sex_with = case_when(
      s19 == 1 ~ "Very Comfortable",
      s19 == 2 ~ "Somewhat Comfortable",
      s19 == 3 ~ "Neutral",
      s19 == 4 ~ "Somewhat Uncomfortable",
      s19 == 5 ~ "Very Uncomfortable",
      s19 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_confident_condom = case_when(
      s20 == 1 ~ "Very confident",
      s20 == 2 ~ "Somewhat confident",
      s20 == 3 ~ "A little confident",
      s20 == 4 ~ "Not at all confident",
      s20 == 5 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_further_sexually = case_when(
      s21 == 1 ~ "Always or almost always",
      s21 == 2 ~ "Often",
      s21 == 3 ~ "About  half the time",
      s21 == 4 ~ "Occasionally",
      s21 == 5 ~ "Never or almost never",
      s21 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_std = case_when(
      s22 == 1 ~ "Yes",
      s22 == 2 ~ "No",
      s22 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_check_out = case_when(
      s23 == 1 ~ "Yes",
      s23 == 2 ~ "No",
      s23 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_std_happen = case_when(
      s24 == 1 ~ "Yes, I had a std (positive diagnosis)",
      s24 == 2 ~ "No STD (negative diagnosis)",
      s24 == 3 ~ "Don't know yet; waiting for results",
      s24 == 4 ~ "Decline",
      TRUE ~ NA_character_
    ),
    across(
      c(
        s25_1, s25_2, s25_3, s25_4, s25_5,
        s25_6, s25_7, s25_8, s25_9, s25_77
      ),
      ~case_when(
        is.na(.x) ~ NA_character_,
        TRUE ~ "Yes"
      )
    ),
    sex_bloodtest_hiv = case_when(
      s26 == 1 ~ "Yes",
      s26 == 2 ~ "No",
      s26 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_might_be_preg = case_when(
      s28 == 1 ~ "Yes",
      s28 == 2 ~ "No",
      s28 == 3 ~ "Decline",
      s28 == 4 ~ "Pregnant prior to last 6 months; knew pregnant prior to 6 month period",
      TRUE ~ NA_character_
    ),
    sex_preg_what_happen = case_when(
      s29 == 1 ~ "Went to dr; positive diagnosis",
      s29 == 2 ~ "Went to dr; negative diagnosis",
      s29 == 3 ~ "Went to dr; don't know",
      s29 == 4 ~ "Took home test; positive",
      s29 == 5 ~ "Took home test; negative",
      s29 == 6 ~ "Took home test; don't know",
      s29 == 7 ~ "Did nothing; period started",
      s29 == 8 ~ "Did nothing, don't know",
      s29 == 9 ~ "Decline",
      TRUE ~ NA_character_
    ),
    across(
      c(
        s32_1, s32_2, s32_3, s32_4, s32_5,
        s32_6, s32_7, s32_8, s32_9, s32_77
      ),
      ~case_when(
        is.na(.x) ~ NA_character_,
        TRUE ~ "Yes"
      )
    ),
    sex_safesex_prev_std = case_when(
      s33 == 1 ~ "Always or almost always",
      s33 == 2 ~ "Often",
      s33 == 3 ~ "About half the time",
      s33 == 4 ~ "Occasionally",
      s33 == 5 ~ "Never or almost never",
      s33 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_percent_sex_relation_safesex_prev_std = case_when(
      s34 == 1 ~ "Never 0%",
      s34 == 2 ~ "Rarely 1-15%",
      s34 == 3 ~ "Occasionally 15-40%",
      s34 == 4 ~ "About half the time 40-60%",
      s34 == 5 ~ "Usually 60-85%",
      s34 == 6 ~ "Most of the time 85-99%",
      s34 == 7 ~ "Every single time 100%",
      s34 == 8 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_sex_relation_justmet = case_when(
      s35 == 1 ~ "Yes",
      s35 == 2 ~ "No",
      s35 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_alcohol_sex_relation = case_when(
      s36 == 1 ~ "Always or almost always",
      s36 == 2 ~ "Often",
      s36 == 3 ~ "About half the time",
      s36 == 4 ~ "Occasionally",
      s36 == 5 ~ "Never or almost never",
      s36 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_cannabis_drugs_sex_relation = case_when(
      s37 == 1 ~ "Always or almost always",
      s37 == 2 ~ "Often",
      s37 == 3 ~ "About half the time",
      s37 == 4 ~ "Occasionally",
      s37 == 5 ~ "Never or almost never",
      s37 == 6 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_sex_relation_w_injects = case_when(
      s38 == 1 ~ "Yes",
      s38 == 2 ~ "No",
      s38 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_intercourse_male_partner = case_when(
      s39 == 1 ~ "Yes",
      s39 == 2 ~ "No",
      s39 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_intercourse_male_nocondom = case_when(
      s40 == 1 ~ "Yes",
      s40 == 2 ~ "No",
      s40 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_percent_w_male_use_birthcontrol = case_when(
      s43 == 1 ~ "Never 0%",
      s43 == 2 ~ "Rarely 1-15%",
      s43 == 3 ~ "Occasionally 15-40%",
      s43 == 4 ~ "About half the time 40-60%",
      s43 == 5 ~ "Usually 60-85%",
      s43 == 6 ~ "Most of the time 85-99%",
      s43 == 7 ~ "Every single time 100%",
      s43 == 8 ~ "Decline",
      s43 == 9 ~ "Don't know",
      s43 == 10 ~ "Not using birth control; currently pregnant",
      TRUE ~ NA_character_
    ),
    sex_intercourse_female_partner = case_when(
      s44 == 1 ~ "Yes",
      s44 == 2 ~ "No",
      s44 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_oralsex_female_partner = case_when(
      s45 == 1 ~ "Yes",
      s45 == 2 ~ "No",
      s45 == 3 ~ "Decline",
      TRUE ~ NA_character_
    ),
    sex_oralsex_dentaldam_female_partner = case_when(
      s46 == 1 ~ "Yes, had oral sex without dental dam",
      s46 == 2 ~ "No, oral sex and always used dental dam",
      s46 == 3 ~ "Decline",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(
    sex_start_see_part = s2,
    sex_been_together_yr = s2a,
    sex_been_together_mth = s2b,
    sex_been_together_day = s2c,
    sex_movein_date = s5,
    sex_live_together_yr = s5a,
    sex_live_together_mth = s5b,
    sex_live_together_day = s5c,
    sex_marry_date = s7,
    sex_part_age = s8,
    sex_id_feeling_text = s10_4_text,
    sex_other_person_text = s14_8_text,
    sex_other_person_age = s15,
    sex_std_herpes_diag = s25_1,
    sex_std_warts_diag = s25_2,
    sex_std_aids_hiv_diag = s25_3,
    sex_std_hepb_diag = s25_4,
    sex_std_gonorrhea_diag = s25_5,
    sex_std_syphilis_diag = s25_6,
    sex_std_chlamydia_diag = s25_7,
    sex_std_other_diag = s25_8,
    sex_std_dontremember = s25_9,
    sex_std_decline = s25_77,
    sex_std_diag_text = s25_8_text,
    sex_bloodtest_hiv_times = s27,
    sex_sex_relations_times = s30,
    sex_number_different_people = s31,
    sex_partner_male_sameage = s32_1,
    sex_partner_male_1to2_older = s32_2,
    sex_partner_male_more2_older = s32_3,
    sex_partner_female_sameage = s32_4,
    sex_partner_female_1to2_older = s32_5,
    sex_partner_female_more2_older = s32_6,
    sex_partner_nonbinary_sameage = s32_7,
    sex_partner_nonbinary_1to2_older = s32_8,
    sex_partner_nonbinary_more2_older = s32_9,
    sex_partner_decline = s32_77,
    sex_intercourse_male_nocondom_times = s41,
    sex_intercourse_male_nocondom_number_partner = s41,
    sex_oralsex_dentaldam_times = s47,
    sex_oralsex_dentaldam_number_parnter = s48
  )


# Rand Health Survey SF-36
sf12 <- bio %>%
  select(
    id,
    r1:r12
  ) %>%
  mutate(
    r1 = case_when(
      r1 == 5 ~ 1,
      r1 == 4 ~ 2,
      r1 == 3 ~ 3,
      r1 == 2 ~ 4,
      r1 == 1 ~ 5,
      r1 == 6 ~ -77,
      TRUE ~ NA_real_
    ),
    across(
      c(r2:r8),
      ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x == 3 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    across(
      c(r9:r12),
      ~case_when(
        .x == 1 ~ 5,
        .x == 2 ~ 4,
        .x == 3 ~ 3,
        .x == 4 ~ 2,
        .x == 5 ~ 1,
        .x == 6 ~ -77,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  rename(
    r2a = r2,
    r2b = r3,
    r3a = r4,
    r3b = r5,
    r4a = r6,
    r4b = r7,
    r5 = r8,
    r6a = r9,
    r6b = r10,
    r6c = r11,
    r7 = r12
  )


sf12_calc <- sf12 %>%
  mutate(
    across(
      -id,
      ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

sf12_miss <- sf12_calc %>%
  pct_miss_fun(
    id = "id",
    n_items = 12
  )

sf12_miss %>%
  gt::gt() %>%
  gt::tab_header(title = 'Missing Data',
                 subtitle = 'By Each Participant')

sf12_remove <- sf12_miss %>% filter(miss_pct >= 20) %>% pull(id)

sf12_complete <- sf12_calc %>%
  mutate(
    sf_gh_total_v2 = case_when(
      r1 == 1 ~ 1,
      r1 == 2 ~ 2,
      r1 == 3 ~ 3.4,
      r1 == 4 ~ 4.4,
      r1 == 5 ~ 5,
      TRUE ~ NA_real_
    ),
    sf_pf_total = r2a + r2b,
    sf_rp_total = r3a + r3b,
    sf_re_total = r4a + r4b,
    sf_mh_total = r6a + r6c,
    sf_bp_total = r5,
    sf_vt_total = r6b,
    sf_sf_total = r7,
    sf_pf_total_v2 = (100*(sf_pf_total - 2))/4,
    sf_rp_total_v2 = (100*(sf_rp_total - 2))/8,
    sf_re_total_v2 = (100*(sf_re_total - 2))/8,
    sf_mh_total_v2 = (100*(sf_mh_total - 2))/8,
    sf_bp_total_v2 = (100*(sf_bp_total - 1))/4,
    sf_vt_total_v2 = (100*(sf_vt_total - 1))/4,
    sf_sf_total_v2 = (100*(sf_sf_total - 1))/4,
    across(
      c(
        sf_gh_total_v2,
        sf_pf_total,
        sf_rp_total,
        sf_re_total,
        sf_mh_total,
        sf_bp_total,
        sf_vt_total,
        sf_sf_total,
        sf_pf_total_v2,
        sf_rp_total_v2,
        sf_re_total_v2,
        sf_mh_total_v2,
        sf_bp_total_v2,
        sf_vt_total_v2,
        sf_sf_total_v2
      ),
      ~case_when(
        id %in% sf12_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

# CINT
cint <- no_bio %>%
  select(
    id,
    pct0:pct3e
  ) %>%
  mutate(
    pct0 = case_when(
      pct0 == 1 ~ "Yes",
      pct0 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    across(
      c(
        pct1:pct2d,
        pct3a:pct3d
      ),
      ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x == 3 ~ -77,
        TRUE ~ NA_real_
      )
    )
  )

cint_calc <- cint %>%
  mutate(
    across(
        -c(id, pct0, pct1, pct2e, pct3e),
        ~case_when(
          .x == -77 ~ NA_real_,
          TRUE ~ .x
        )
    )
  )

cint_miss <- cint_calc %>%
  pct_miss_fun(
    id = c(
      "id",
      "pct0",
      "pct1",
      "pct2e",
      "pct3e"
    ),
    n_items = 8
  )

cint_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Missing Data",
    subtitle = "By Each Participant"
  )

cint_remove <- cint_miss %>% filter(miss_pct >= 20) %>% pull(id)

cint_complete <- cint_calc %>%
  composite_total_avg_fun(
    id = c(
      "id",
      "pct0",
      "pct1",
      "pct2e",
      "pct3e"
    ),
    max_value = 1,
    n_items = 8
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  select(
    -mean_values
  ) %>%
  rename(
    cint_total = sum_values
    ) %>%
  mutate(
    across(
      c(
        cint_total
      ),
      ~case_when(
        id %in% cint_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

cint_complete %>%
  composite_hist(
    x = cint_total,
    bins = 10
  ) +
  labs(
    title = "Distribution of Total Scores for the CINT"
  )


# BSI
bsi <-
  no_bio %>%
  select(
    id,
    matches(
      '^bsi'
    ),
    -bsi20,
    -bsi21,
    -bsi22
  ) %>%
  subtract_scale(
    id = 'id',
    value = 1
  ) %>%
  mutate(
    across(
      .cols = c(bsi1:bsi19),
      .fns = ~case_when(
        .x == 0 ~ 0,
        .x == 1 ~ 1,
        .x == 2 ~ 2,
        .x == 3 ~ 3,
        .x == 4 ~ 4,
        .x == 5 ~ -77,
        TRUE ~ NA_real_
      )
    )
  )

bsi_miss <- bsi %>%
  pct_miss_fun(
    id = 'id',
    n_items = 19
  )

bsi_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Missing Data",
    subtitle = "By Each Participant"
  )

bsi_remove <- bsi_miss %>% filter(miss_pct >= 20) %>% pull(id)

bsi_calc <-
  bsi %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

bsi_complete <-
  bsi_calc %>%
  composite_total_avg_fun(
    id = 'id',
    max_value = 4,
    n_items = 19
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c('bsi2', 'bsi3', 'bsi10',
               'bsi11', 'bsi12', 'bsi13', 'bsi15'),
    scale2 = c('bsi1', 'bsi5', 'bsi9',
               'bsi16', 'bsi17', 'bsi18'),
    scale3 = c('bsi4', 'bsi6', 'bsi7',
               'bsi8', 'bsi14', 'bsi19')
  ) %>%
  rename(
    bsi_total = sum_values,
    bsi_avg = mean_values,
    bsi_soma_total = total1,
    bsi_anx_total = total2,
    bsi_dep_total = total3
  ) %>%
  mutate(
    across(
      c(
        bsi_total,
        bsi_avg,
        bsi_soma_total,
        bsi_anx_total,
        bsi_dep_total
      ),
      ~case_when(
        id %in% bsi_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

bsi_complete %>%
  composite_hist(
    x = bsi_total
  ) +
  labs(title = 'Distribution of Total Scores for BSI Measure')

bsi_complete %>%
  composite_hist(x = bsi_soma_total) +
  labs(title = 'Distribution for BSI Somatization Subscale')

bsi_complete %>%
  composite_hist(x = bsi_anx_total) +
  labs(title = 'Distribution for BSI Anxiety Subscale')

bsi_complete %>%
  composite_hist(x = bsi_dep_total) +
  labs(title = 'Distribution for BSI Depression Subscale')

bsi_complete %>%
  cutoff_plot(
    x = bsi_total,
    cutoff = 0
  ) +
  labs(title = 'Cutoff Value for BSI Measure')

bsi_alpha <-
  bsi_calc %>%
  select(bsi1:bsi19) %>%
  psych::alpha(check.keys = TRUE)

bsi_alpha_som <-
  bsi_calc %>%
  select(bsi2:bsi3,
         bsi10:bsi13,
         bsi15) %>%
  psych::alpha(check.keys = TRUE)

bsi_alpha_anx <-
  bsi_calc %>%
  select(bsi1, bsi5, bsi9, bsi16:bsi18) %>%
  psych::alpha(check.keys = TRUE)

bsi_alpha_dep <-
  bsi_calc %>%
  select(bsi4, bsi6:bsi8, bsi14, bsi19) %>%
  psych::alpha(check.keys = TRUE)

bsi_alpha_table <- tibble(
  Scale = c('BSI',
            'BSI - Somatization',
            'BSI - Anxiety',
            'BSI - Depression'),
  Alpha = c(round(bsi_alpha$total$raw_alpha, 3),
            round(bsi_alpha_som$total$raw_alpha, 3),
            round(bsi_alpha_anx$total$raw_alpha, 3),
            round(bsi_alpha_dep$total$raw_alpha, 3))
)

bsi_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for BSI Entire Scale & Subscales'
  )


# CESD
cesd <- no_bio %>%
  select(
    id,
    matches("^c\\d")
  ) %>%
  subtract_scale(
    id = 'id',
    value = 1
  ) %>%
  mutate(
    across(
      .cols = c(c1:c20),
      .fns = ~case_when(
        .x == 0 ~ 0,
        .x == 1 ~ 1,
        .x == 2 ~ 2,
        .x == 3 ~ 3,
        .x == 4 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    across(
      .cols = c(c21:c24),
      .fns = ~case_when(
        .x == 0 ~ 1,
        .x == 1 ~ 0,
        .x == 2 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    c4_r = case_when(
      c4 == 0 ~ 3,
      c4 == 1 ~ 2,
      c4 == 2 ~ 1,
      c4 == 3 ~ 0,
      c4 == -77 ~ -77,
      TRUE ~ NA_real_
    ),
    c8_r = case_when(
      c8 == 0 ~ 3,
      c8 == 1 ~ 2,
      c8 == 2 ~ 1,
      c8 == 3 ~ 0,
      c8 == -77 ~ -77,
      TRUE ~ NA_real_
    ),
    c12_r = case_when(
      c12 == 0 ~ 3,
      c12 == 1 ~ 2,
      c12 == 2 ~ 1,
      c12 == 3 ~ 0,
      c12 == -77 ~ -77,
      TRUE ~ NA_real_
    ),
    c16_r = case_when(
      c16 == 0 ~ 3,
      c16 == 1 ~ 2,
      c16 == 2 ~ 1,
      c16 == 3 ~ 0,
      c16 == -77 ~ -77,
      TRUE ~ NA_real_
    )
  )

cesd_miss <- cesd %>%
  pct_miss_fun(
    id = c(
      "id",
      "c4",
      "c8",
      "c12",
      "c16"
    ),
    n_items = 20
  )

cesd_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Missing Data",
    subtitle = "By Each Participant"
  )

cesd_remove <- cesd_miss %>% filter(miss_pct >= 20) %>% pull(id)

cesd_calc <-
  cesd %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

cesd_complete <- cesd_calc %>%
  composite_total_avg_fun(
    id = c('id',
           'c4',
           'c8',
           'c12',
           'c16',
           'c21',
           'c22',
           'c23',
           'c24'),
    n_items = 20
  ) %>%
  subscale_create(
    total_only = FALSE,
    scale1 = c('c1', 'c2', 'c3', 'c5',
               'c6', 'c9', 'c10', 'c11',
               'c13', 'c14', 'c15', 'c17',
               'c18', 'c19', 'c20'),
    scale1_nitems = 15,
    scale2 = c('c4_r', 'c8_r', 'c12_r', 'c16_r'),
    scale2_nitems = 4
  ) %>%
  rename(
    cesd_total = sum_values,
    cesd_avg = mean_values,
    cesd_dep_symp_total = total1,
    cesd_dep_symp_avg = avg1,
    cesd_pos_aff_total = total2,
    cesd_pos_aff_avg = avg2
  ) %>%
  distinct(
    across(
      .cols = everything()
    )
  ) %>%
  mutate(
    across(
      c(
        cesd_total,
        cesd_avg,
        cesd_dep_symp_total,
        cesd_dep_symp_avg,
        cesd_pos_aff_total,
        cesd_pos_aff_avg
      ),
      ~case_when(
        id %in% cesd_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

cesd_complete %>%
  composite_hist(cesd_total) +
  labs(
    title = 'CESD Total Score Distribution'
  )

cesd_complete %>%
  composite_hist(
    cesd_dep_symp_total
  ) +
  labs(
    title = 'Total Score Distribution',
    subtitle = 'Depressive Symptoms Subscale'
  )

cesd_complete %>%
  composite_hist(
    cesd_pos_aff_total,
    bins = 10
  ) +
  labs(
    title = 'Total Score Distribution',
    subtitle = 'Depressive Symptoms Subscale'
  )

cesd_complete %>%
  cutoff_plot(
    x = cesd_total,
    cutoff = 16,
    cutoff_other = 23
  ) +
  labs(
    title = 'Cutoff Scores for CESD Total Scores',
    caption = 'Cutoffs are 16 and/or 23\nSee references for literature on cutoff scores.'
  )

cesd_alpha <-
  cesd_complete %>%
  select(c1:c16_r) %>%
  psych::alpha(check.keys = TRUE)

cesd_dep_alpha <-
  cesd_complete %>%
  select(c1:c20) %>%
  psych::alpha(check.keys = TRUE)

cesd_pos_aff_alpha <-
  cesd_complete %>%
  select(c4_r, c8_r, c12_r, c16_r) %>%
  psych::alpha(check.keys = TRUE)


cesd_alpha_table <- tibble(
  scale = c('CESD', 'CESD - Depressive Symptoms',
            'CESD - Positive Affect'),
  alpha = c(round(cesd_alpha$total$raw_alpha, 3),
            round(cesd_dep_alpha$total$raw_alpha, 3),
            round(cesd_pos_aff_alpha$total$raw_alpha, 3)
  )
)

cesd_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for CESD Entire Scale & Subscales'
  )

# COVID-19
ciq <-
  no_bio %>%
  select(
    id,
    bsi20,
    bsi21,
    bsi22
  ) %>%
  rename(
    ciq1 = bsi20,
    ciq2 = bsi21,
    ciq3 = bsi22
  ) %>%
  mutate(
    across(
      .cols = c(ciq1, ciq2, ciq3),
      .fns = ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 2,
        .x == 3 ~ 3,
        .x == 4 ~ 4,
        .x == 5 ~ 5,
        .x == 6 ~ 6,
        .x == 7 ~ 7,
        .x == 8 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    ciq3_r = case_when(
      ciq3 == 1 ~ 7,
      ciq3 == 2 ~ 6,
      ciq3 == 3 ~ 5,
      ciq3 == 4 ~ 4,
      ciq3 == 5 ~ 3,
      ciq3 == 6 ~ 2,
      ciq3 == 7 ~ 1,
      ciq3 == -77 ~ -77,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(
    ciq3_r, .before = ciq3
  )

ciq_miss <- ciq %>%
  select(id,
         ciq1:ciq3_r) %>%
  pct_miss_fun(
    id = 'id',
    n_items = 3
  )

ciq_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Missing Data",
    subtitle = "By Each Participant"
  )

ciq_remove <- ciq_miss %>% filter(miss_pct >= 20) %>% pull(id)

ciq_calc <-
  ciq %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

ciq_complete <-
  ciq_calc %>%
  composite_total_avg_fun(
    id = c('id', 'ciq3'),
    max_value = 7,
    n_items = 3
  ) %>%
  rename(
    ciq_psych_total = sum_values,
    ciq_psych_avg = mean_values
  ) %>%
  mutate(
    across(
      ciq_psych_total,
      ciq_psych_avg
    ),
    ~case_when(
      id %in% ciq_remove ~ NA_real_,
      TRUE ~ .x
    )
  )

ciq_complete %>%
  composite_hist(
    x = ciq_psych_total,
    bins = 10
  ) +
  labs(title = 'Distribution for CIQ Psychological Scale')

ciq_alpha <-
  ciq_complete %>%
  select(ciq1, ciq2, ciq3) %>%
  psych::alpha(check.keys = TRUE)

ciq_alpha_table <- tibble(
  Scale = 'CIQ - Psychological Scale',
  Alpha = round(ciq_alpha$total$raw_alpha, 3)
)

ciq_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for CIQ - Psychological Scale'
  )

# DAST
dast <-
  no_bio %>%
  select(
    id,
    matches('^d\\d')
  ) %>%
  mutate(
    preg_screen = case_when(
      d00 == 1 ~ 'pregnant',
      d00 == 2 ~ 'not_pregnant',
      TRUE ~ NA_character_
    ),
    d00 = case_when(
      d00 == 1 ~ 1,
      d00 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  relocate(preg_screen, .before = d00) %>%
  mutate(
    across(
      .cols = c(d1a:d20),
      ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x == 3 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    across(
      .cols = c(d4, d5),
      .fns = ~if_else(
        .x == 0, 1, 0
      ),
      .names = '{.col}_r'
    )
  )

dast_miss <- dast %>%
  pct_miss_fun(
    id = c(
      'id',
      "preg_screen",
      "d00",
      "d4",
      "d5"
    ),
    n_items = 21
  )

dast_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'DAST Missing Data',
    subtitle = 'By Participant'
  )

dast_remove <- dast_miss %>% filter(miss_pct >= 20) %>% pull(id)

dast_calc <- dast %>%
  mutate(
    across(
      .cols = c(d1a:d5_r),
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

dast_complete <-
  dast_calc %>%
  composite_total_avg_fun(
    id = c('id',
           'preg_screen',
           'd00',
           'd4',
           'd5'),
    max_value = 1,
    n_items = 21
  ) %>%
  select(-mean_values) %>%
  rename(
    dast_total = sum_values
  ) %>%
  mutate(
    dast_total = case_when(
      id %in% dast_remove ~ NA_real_,
      TRUE ~ dast_total
    )
  )

dast_complete %>%
  composite_hist(
    x = dast_total
  ) +
  labs(
    title = 'Distribution of Total Scores for DAST Measure'
  )

dast_complete %>%
  severity_plot(
    x = dast_total,
    bins = 20,
    low_xmin = 1,
    low_xmax = 5.5,
    medium_xmin = 5.5,
    medium_xmax = 10.5,
    large_xmin = 10.5,
    large_xmax = 15.5,
    critical_xmin = 15.5,
    critical_xmax = 20) +
  annotate(
    geom = 'text',
    color = 'Black',
    x = 3,
    y = -1.5,
    label = 'Low'
  ) +
  annotate(
    geom = 'text',
    color = 'Black',
    x = 8,
    y = -1.5,
    label = 'Intermediate'
  ) +
  annotate(
    geom = 'text',
    color = 'Black',
    x = 13,
    y = -1.5,
    label = 'Substantial'
  ) +
  annotate(
    geom = 'text',
    color = 'Black',
    x = 18,
    y = -1.5,
    label = 'Severe'
  ) +
  labs(
    title = 'Severity Categories',
    subtitle = 'Based on DAST',
    caption = 'Cutoffs are:\nLow = 1-5,\nIntermediate = 6-10,\nSubstantial = 11-15,\nSevere = 16-20'
  )

dast_alpha <-
  dast_complete %>%
  select(
    -id:-d00,
    -d4,
    -d5,
    -dast_total
  ) %>%
  psych::alpha(check.keys = TRUE)


dast_alpha_table <-
  tibble(
    Scale = 'DAST',
    Alpha = round(dast_alpha$total$raw_alpha, 3)
  )

dast_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for DAST Scale'
  )

#BCAP
bcap <- no_bio %>%
  select(
    id,
    matches('^q\\d'),
    -q1461
  ) %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x == 3 ~ -77,
        TRUE ~ NA_real_
      )
    ),
    across(
      .cols = c(q1, q2, q23, q29),
      .fns = ~case_when(
        .x == 1 ~ 0,
        .x == 0 ~ 1,
        TRUE ~ .x
      ),
      .names = '{.col}_r'
    )
  )

bcap_miss <- bcap %>%
  pct_miss_fun(
    id = c(
      "id",
      "q1",
      "q2",
      "q23",
      "q29"
    ),
    n_items = 34
  )

bcap_remove <- bcap_miss %>% filter(miss_pct >= 20) %>% pull(id)


bcap_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'BCAP Missing Data',
    subtitle = 'By Participant'
  )

bcap_calc <-
  bcap %>%
  mutate(
    across(
      .cols = -id,
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

bcap_complete <-
  bcap_calc %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c('q1_r', 'q23_r', 'q29_r',
               'q3', 'q25', 'q33',
               'q5', 'q12', 'q22', 'q31',
               'q6', 'q13', 'q17',
               'q7', 'q14', 'q20', 'q32',
               'q8', 'q11', 'q16', 'q19', 'q27',
               'q10', 'q30'),
    scale2 = c('q1_r', 'q23_r', 'q29_r'),
    scale3 = c('q3', 'q25', 'q33'),
    scale4 = c('q5', 'q12', 'q22', 'q31'),
    scale5 = c('q6', 'q13', 'q17'),
    scale6 = c('q7', 'q14', 'q20', 'q32'),
    scale7 = c('q8', 'q11', 'q16', 'q19', 'q27'),
    scale8 = c('q10', 'q30'),
    scale9 = c('q4', 'q9', 'q15', 'q21', 'q26', 'q34'),
    scale10 = c('q2_r', 'q18', 'q28')
  ) %>%
  rename(
    bcap_risk_total = total1,
    bcap_happy_total = total2,
    bcap_feel_pers_total = total3,
    bcap_lonely_total = total4,
    bcap_fam_conf_total = total5,
    bcap_rigid_total = total6,
    bcap_distress_total = total7,
    bcap_poverty_total = total8,
    bcap_lie_total = total9,
    bcap_random_total = total10
  ) %>%
  mutate(
    across(
      c(
        bcap_risk_total,
        bcap_happy_total,
        bcap_feel_pers_total,
        bcap_lonely_total,
        bcap_fam_conf_total,
        bcap_rigid_total,
        bcap_distress_total,
        bcap_poverty_total,
        bcap_lie_total,
        bcap_random_total
      ),
      ~case_when(
        id %in% bcap_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

bcap_complete %>%
  composite_hist(
    x = bcap_risk_total
  ) +
  labs(
    title = 'Distribution of Total Scores for BCAP Risk Scale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_happy_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Happiness Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_feel_pers_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Feelings of Persecution Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_lonely_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Loneliness Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_fam_conf_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Family Conflict Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_rigid_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Rigidity Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_distress_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Distress Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_poverty_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Poverty Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_lie_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Lying Subscale'
  )

bcap_complete %>%
  composite_hist(
    x = bcap_random_total,
    bins = 5
  ) +
  labs(
    title = 'Distribution For BCAP Random Responding Subscale'
  )

bcap_complete %>%
  cutoff_plot(
    x = bcap_risk_total,
    cutoff = 9,
    cutoff_other = 12
  ) +
  labs(
    title = 'Cutoff Values for BCAP Scale',
    caption = 'Cutoffs are 9 and 12\nSee references for literature on cutoff scores.'
  )

bcap_complete %>%
  cutoff_plot(
    x = bcap_lie_total,
    cutoff = 4
  ) +
  labs(
    title = 'Cutoff Values for BCAP Lie Subscale',
    caption = 'If 4 or more (score of 4) items are endorsed,\ncase may be invalid'
  )

bcap_complete %>%
  cutoff_plot(
    x = bcap_random_total,
    cutoff = 0
  ) +
  labs(
    title = 'Cutoff Values for BCAP Random Responding Subscale',
    caption = 'If any of these items are endorsed (score > 0),\ncase may be invalid'
  )

bcap_alpha <-
  bcap_complete %>%
  select(
    q1_r,
    q2_r,
    q3:q22,
    q23_r,
    q24:q28,
    q29_r,
    q30:q34
  ) %>%
  psych::alpha(check.keys = TRUE)

bcap_risk_alpha <-
  bcap_complete %>%
  select(
    q1_r,
    q3,
    q5:q8,
    q10:q14,
    q16:q17,
    q19:q20,
    q22,
    q23_r,
    q24:q25,
    q27,
    q29_r,
    q30:q33
  ) %>%
  psych::alpha(check.keys = TRUE)

bcap_happy_alpha <-
  bcap_complete %>%
  select(q1_r, q23_r, q29_r) %>%
  psych::alpha(check.keys = TRUE)

bcap_feel_pers_alpha <-
  bcap_complete %>%
  select(q3, q25, q33) %>%
  psych::alpha(check.keys = TRUE)

bcap_lonely_alpha <-
  bcap_complete %>%
  select(q5, q12, q22, q31) %>%
  psych::alpha(check.keys = TRUE)

bcap_fam_conf_alpha <-
  bcap_complete %>%
  select(q6, q13, q17) %>%
  psych::alpha(check.keys = TRUE)

bcap_rigid_alpha <-
  bcap_complete %>%
  select(q7, q14, q20, q32) %>%
  psych::alpha(check.keys = TRUE)

bcap_distress_alpha <-
  bcap_complete %>%
  select(q8, q11, q16, q19, q27) %>%
  psych::alpha(check.keys = TRUE)

bcap_poverty_alpha <-
  bcap_complete %>%
  select(q10, q30) %>%
  psych::alpha(check.keys = TRUE)

bcap_lie_alpha <-
  bcap_complete %>%
  select(q4, q9, q15, q21, q26, q34) %>%
  psych::alpha(check.keys = TRUE)

bcap_random_alpha <-
  bcap_complete %>%
  select(q2_r, q19, q28) %>%
  psych::alpha(check.keys = TRUE)

bcap_alpha_table <-
  tibble(
    Scale = c('BCAP Complete Scale', 'BCAP Risk Scale',
              'Happiness Subscale', 'Feelings of Persecution Subscale',
              'Loneliness Subscale', 'Family Conflict Subscale',
              'Rigidity Subscale', 'Distress Subscale',
              'Poverty Subscale',
              'Lying Subscale', 'Random Responding Subscale'),
    Alpha = c(round(bcap_alpha$total$raw_alpha, 3),
              round(bcap_risk_alpha$total$raw_alpha, 3),
              round(bcap_happy_alpha$total$raw_alpha, 3),
              round(bcap_feel_pers_alpha$total$raw_alpha, 3),
              round(bcap_lonely_alpha$total$raw_alpha, 3),
              round(bcap_fam_conf_alpha$total$raw_alpha, 3),
              round(bcap_rigid_alpha$total$raw_alpha, 3),
              round(bcap_distress_alpha$total$raw_alpha, 3),
              round(bcap_poverty_alpha$total$raw_alpha, 3),
              round(bcap_lie_alpha$total$raw_alpha, 3),
              round(bcap_random_alpha$total$raw_alpha, 3))
  )

bcap_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for BCAP Scale & Subscales'
  )

# DYADC

dyadc <-
  no_bio %>%
    select(
      id,
      dyadc_screener,
      matches(
        "^e\\d"
      )
    ) %>%
    mutate(
      dyadc_screen = case_when(
        dyadc_screener == 1 ~ 'yes_relationship',
        dyadc_screener == 2 ~ 'no_relationship'
      )
    ) %>%
    select(-dyadc_screener)

dyadc <-
    dyadc %>%
    mutate(
      across(
        .cols = c(e1:e15),
        .fns = ~case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x == 3 ~ 2,
          .x == 4 ~ 3,
          .x == 5 ~ 4,
          .x == 6 ~ 5,
          .x == 7 ~ -77,
          .x == 8 ~ NA_real_
        )
      ),
      across(
        .cols = c(e16, e17, e20:e22),
        .fns = ~case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x == 3 ~ 2,
          .x == 4 ~ 3,
          .x == 5 ~ 4,
          .x == 6 ~ 5,
          .x == 7 ~ -77,
          .x == 8 ~ NA_real_
        )
      ),
      e18 = case_when(
        e18 == 1 ~ 5,
        e18 == 2 ~ 4,
        e18 == 3 ~ 3,
        e18 == 4 ~ 2,
        e18 == 5 ~ 1,
        e18 == 6 ~ 0,
        e18 == 7 ~ -77,
        TRUE ~ NA_real_
      ),
      e19 = case_when(
        e19 == 1 ~ 5,
        e19 == 2 ~ 4,
        e19 == 3 ~ 3,
        e19 == 4 ~ 2,
        e19 == 5 ~ 1,
        e19 == 6 ~ 0,
        e19 == 7 ~ -77,
        TRUE ~ NA_real_
      ),
      across(
        .cols = c(e23:e24),
        .fns = ~case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x == 3 ~ 2,
          .x == 4 ~ 3,
          .x == 5 ~ 4,
          .x == 6 ~ -77,
          TRUE ~ NA_real_
        )
      ),
      across(
        .cols = c(e25:e28),
        .fns = ~case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x == 3 ~ 2,
          .x == 4 ~ 3,
          .x == 5 ~ 4,
          .x == 6 ~ 5,
          .x == 7 ~ -77,
          TRUE ~ NA_real_
        )
      ),
      across(
        .cols = c(e29, e30),
        .fns = ~case_when(
          .x == 1 ~ 0,
          .x == 2 ~ 1,
          .x == 3 ~ -77,
          TRUE ~ NA_real_
        )
      ),
      e31 = case_when(
        e31 == 1 ~ 0,
        e31 == 2 ~ 1,
        e31 == 3 ~ 2,
        e31 == 4 ~ 3,
        e31 == 5 ~ 4,
        e31 == 6 ~ 5,
        e31 == 7 ~ 6,
        e31 == 8 ~ -77,
        TRUE ~ NA_real_
      ),
      e32 = case_when(
        e32 == 1 ~ 5,
        e32 == 2 ~ 4,
        e32 == 3 ~ 3,
        e32 == 4 ~ 2,
        e32 == 5 ~ 1,
        e32 == 6 ~ 0,
        e32 == 7 ~ -77,
        TRUE ~ NA_real_
      )
    )

dyadc_miss <- dyadc %>%
  pct_miss_fun(
    id = c('id', 'dyadc_screen'),
    n_items = 32
  )

dyadc_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'DYADC Missing Data',
    subtitle = 'By Participant'
  )

dyadc_remove <- dyadc_miss %>%
  filter(
    miss_pct >= 20
  ) %>%
  pull(id)

dyadc_calc <-
  dyadc %>%
  mutate(
    across(
      .cols = c(e1:e32),
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

dyadc_complete <-
  dyadc_calc %>%
  composite_total_avg_fun(
    id = c('id', 'dyadc_screen'),
    n_items = 32
  ) %>%
  rename(
    dyadc_total = sum_values,
    dyadc_avg = mean_values
  ) %>%
  subscale_create(
    total_only = FALSE,
    scale1 = c('e1', 'e2', 'e3', 'e4', 'e5',
               'e6', 'e7', 'e8', 'e9', 'e10',
               'e11', 'e12', 'e13'),
    scale1_nitems = 13,
    scale2 = c('e4', 'e6',
               'e29', 'e30'),
    scale2_nitems = 4,
    scale3 = c('e16', 'e17', 'e18', 'e19', 'e20',
               'e21', 'e22', 'e23', 'e31', 'e32'),
    scale3_nitems = 10,
    scale4 = c('e24', 'e25', 'e26', 'e27', 'e28'),
    scale4_nitems = 5
  ) %>%
  rename(
    dyadc_con_total = total1,
    dyadc_con_avg = avg1,
    dyadc_aff_exp_total = total2,
    dyadc_aff_exp_avg = avg2,
    dyadc_satis_total = total3,
    dyadc_satis_avg = avg3,
    dyadc_cohes_total = total4,
    dyadc_cohes_avg = avg4
  ) %>%
  mutate(
    across(
      c(
        dyadc_total,
        dyadc_avg,
        dyadc_con_total,
        dyadc_con_avg,
        dyadc_aff_exp_total,
        dyadc_aff_exp_avg,
        dyadc_satis_total,
        dyadc_satis_avg,
        dyadc_cohes_total,
        dyadc_cohes_avg
      ),
      ~case_when(
        id %in% dyadc_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

dyadc_complete %>%
  composite_hist(
    x = dyadc_total
  ) +
  labs(
    title = 'Distribution for DYADC Scale'
  )

dyadc_complete %>%
  composite_hist(
    x = dyadc_con_total
  ) +
  labs(
    title = 'Distribution for DYADC Consensus Subscale'
  )

dyadc_complete %>%
  composite_hist(
    x = dyadc_aff_exp_total,
    bins = 10
  ) +
  labs(
    title = 'Distribution for DYADC Affectional Expression Subscale'
  )

dyadc_complete %>%
  composite_hist(
    x = dyadc_satis_total
  ) +
  labs(
    title = 'Distribution for DYADC Satisfaction Subscale'
  )

dyadc_complete %>%
  composite_hist(
    x = dyadc_cohes_total
  ) +
  labs(
    title = 'Distribution for DYADC Cohesion Subscale'
  )

dyadc_complete %>%
  cutoff_plot(
    x = dyadc_total,
    cutoff = 92,
    cutoff_other = 107
  ) +
  labs(
    title = 'Cutoff Values for DYADC Scale',
    caption = 'Cutoffs are 92 and 107\nSee references for literature on cutoff scores.'
  )

dyadc_alpha <-
  dyadc_complete %>%
  select(
    e1:e32
  ) %>%
  psych::alpha(check.keys = TRUE)

dyadc_con_alpha <-
  dyadc_complete %>%
  select(
    e1:e13
  ) %>%
  psych::alpha(check.keys = TRUE)

dyadc_aff_exp_alpha <-
  dyadc_complete %>%
  select(
    e4, e6, e29, e30
  ) %>%
  psych::alpha(check.keys = TRUE)

dyadc_satis_alpha <-
  dyadc_complete %>%
  select(
    e16:e23,
    e31,
    e32
  ) %>%
  psych::alpha(check.keys = TRUE)

dyadc_cohes_alpha <-
  dyadc_complete %>%
  select(
    e24:e28
  ) %>%
  psych::alpha(check.keys = TRUE)

dyadc_alpha_table <-
  tibble(
    Scale = c('DYADC',
              'DYADC - Consensus',
              'DYADC - Affectional Expression',
              'DYADC - Satisfaction',
              'DYADC - Cohesion'

    ),
    Alpha = c(round(dyadc_alpha$total$raw_alpha, 3),
              round(dyadc_con_alpha$total$raw_alpha, 3),
              round(dyadc_aff_exp_alpha$total$raw_alpha, 3),
              round(dyadc_satis_alpha$total$raw_alpha, 3),
              round(dyadc_cohes_alpha$total$raw_alpha, 3))
  )

dyadc_alpha_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'Alpha Values for DYADC Scale & Subscales'
  )

# CTS

cts <-
  no_bio %>%
  select(
    id,
    cts_screen,
    matches('^ct\\d')
  ) %>%
  mutate(
    cts_screen = case_when(
      cts_screen == 1 ~ 'in_relationship',
      cts_screen == 2 ~ 'no_relationship',
      TRUE ~ NA_character_
    ),
    across(
      .cols = c(ct1:ct20),
      .fns = ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 2,
        .x == 3 ~ 3,
        .x == 4 ~ 4,
        .x == 5 ~ 5,
        .x == 6 ~ 6,
        .x == 7 ~ 7,
        .x == 8 ~ 8,
        .x == 9 ~ -77,
        TRUE ~ NA_real_
      )
    )
  )

cts_miss <- cts %>%
  pct_miss_fun(
    id = c('id', 'cts_screen'),
    n_items = 20
  )

cts_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'CTS2S Missing Data',
    subtitle = 'By Participant'
  )

cts_remove <- cts_miss %>%
  filter(
    miss_pct >= 20
  ) %>%
  pull(id)

cts_calc <-
  cts %>%
  mutate(
    across(
      .cols = c(ct1:ct20),
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        TRUE ~ .x
      )
    )
  ) %>%
  mutate(
    across(
      .cols = c(ct1, ct2, ct7, ct8),
      .fns = ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 2,
        .x == 3 ~ 4,
        .x == 4 ~ 8,
        .x == 5 ~ 16,
        .x == 6 ~ 20,
        .x == 7 ~ 0,
        .x == 8 ~ 0,
        TRUE ~ NA_real_
      ),
      .names = '{col}_midpoint'
    )
  )

cts_complete <-
  cts_calc %>%
  mutate(
    across(
      .cols = c(ct1:ct20),
      .fns = ~case_when(
        .x %in% c(1, 2, 3, 4, 5, 6) ~ 1,
        .x %in% c(7, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      .names = '{col}_dum'
    )
  ) %>%
  subscale_create(
    total_only = TRUE,
    scale1 = c('ct3_dum', 'ct13_dum'),
    scale2 = c('ct4_dum', 'ct14_dum'),
    scale3 = c('ct5_dum', 'ct15_dum'),
    scale4 = c('ct6_dum', 'ct16_dum'),
    scale5 = c('ct9_dum', 'ct11_dum'),
    scale6 = c('ct10_dum', 'ct12_dum'),
    scale7 = c('ct17_dum', 'ct19_dum'),
    scale8 = c('ct18_dum', 'ct20_dum'),
    scale9 = c('ct1_midpoint', 'ct7_midpoint'),
    scale10 = c('ct2_midpoint', 'ct8_midpoint')
  ) %>%
  rename(
    cts_psy_agg_ind_sev = total1,
    cts_psy_agg_part_sev = total2,
    cts_injury_ind_sev = total3,
    cts_injury_part_sev = total4,
    cts_assault_ind_sev = total5,
    cts_assault_part_sev = total6,
    cts_sex_ind_sev = total7,
    cts_sex_part_sev = total8,
    cts_negotiate_ind = total9,
    cts_negotiate_part = total10
  ) %>%
  mutate(
    across(
      c(
        cts_psy_agg_ind_sev,
        cts_psy_agg_part_sev,
        cts_injury_ind_sev,
        cts_injury_part_sev,
        cts_assault_ind_sev,
        cts_assault_part_sev,
        cts_sex_ind_sev,
        cts_sex_part_sev,
        cts_negotiate_ind,
        cts_negotiate_part
      ),
      ~case_when(
        id %in% cts_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  ) %>%
  mutate(
    cts_psy_agg_ind_prev = case_when(
      is.na(cts_psy_agg_ind_sev) ~ NA_real_,
      cts_psy_agg_ind_sev == 0 ~ 0,
      cts_psy_agg_ind_sev != 0 ~ 1
    ),
    cts_psy_agg_part_prev = case_when(
      is.na(cts_psy_agg_part_sev) ~ NA_real_,
      cts_psy_agg_part_sev == 0 ~ 0,
      cts_psy_agg_part_sev != 0 ~ 1
    ),
    cts_injury_ind_prev = case_when(
      is.na(cts_injury_ind_sev) ~ NA_real_,
      cts_injury_ind_sev == 0 ~ 0,
      cts_injury_ind_sev != 0 ~ 1
    ),
    cts_injury_part_prev = case_when(
      is.na(cts_injury_part_sev) ~ NA_real_,
      cts_injury_part_sev == 0 ~ 0,
      cts_injury_part_sev != 0 ~ 1
    ),
    cts_assault_ind_prev = case_when(
      is.na(cts_assault_ind_sev) ~ NA_real_,
      cts_assault_ind_sev == 0 ~ 0,
      cts_assault_ind_sev != 0 ~ 1
    ),
    cts_assault_part_prev = case_when(
      is.na(cts_assault_part_sev) ~ NA_real_,
      cts_assault_part_sev == 0 ~ 0,
      cts_assault_part_sev != 0 ~ 1
    ),
    cts_sex_ind_prev = case_when(
      is.na(cts_sex_ind_sev) ~ NA_real_,
      cts_sex_ind_sev == 0 ~ 0,
      cts_sex_ind_sev != 0 ~ 1
    ),
    cts_sex_part_prev = case_when(
      is.na(cts_sex_part_sev) ~ NA_real_,
      cts_sex_part_sev == 0 ~ 0,
      cts_sex_part_sev != 0 ~ 1
    ),
    cts_psy_agg_mutual = case_when(
      (cts_psy_agg_ind_prev == 1 &
         cts_psy_agg_part_prev == 1) ~ 3,
      (cts_psy_agg_ind_prev == 0 &
         cts_psy_agg_part_prev == 1) ~ 1,
      (cts_psy_agg_ind_prev == 1 &
         cts_psy_agg_part_prev == 0) ~ 2,
      (cts_psy_agg_ind_prev == 0 &
         cts_psy_agg_part_prev == 0) ~ 0
    ),
    cts_injury_mutual = case_when(
      (cts_injury_ind_prev == 1 &
         cts_injury_part_prev == 1) ~ 3,
      (cts_injury_ind_prev == 0 &
         cts_injury_part_prev == 1) ~ 1,
      (cts_injury_ind_prev == 1 &
         cts_injury_part_prev == 0) ~ 2,
      (cts_injury_ind_prev == 0 &
         cts_injury_part_prev == 0) ~ 0
    ),
    cts_assault_mutual = case_when(
      (cts_assault_ind_prev == 1 &
         cts_assault_part_prev == 1) ~ 3,
      (cts_assault_ind_prev == 0 &
         cts_assault_part_prev == 1) ~ 1,
      (cts_assault_ind_prev == 1 &
         cts_assault_part_prev == 0) ~ 2,
      (cts_assault_ind_prev == 0 &
         cts_assault_part_prev == 0) ~ 0
    ),
    cts_sex_mutual = case_when(
      (cts_sex_ind_prev == 1 &
         cts_sex_part_prev == 1) ~ 3,
      (cts_sex_ind_prev == 0 &
         cts_sex_part_prev == 1) ~ 1,
      (cts_sex_ind_prev == 1 &
         cts_sex_part_prev == 0) ~ 2,
      (cts_sex_ind_prev == 0 &
         cts_sex_part_prev == 0) ~ 0
    )
  )

cts_complete %>%
  mutate(
    cts_psy_agg_ind_sev = case_when(
      cts_psy_agg_ind_sev == 0 ~ 'No Psychological Aggression',
      cts_psy_agg_ind_sev == 1 ~ 'Minor Psychological Aggression',
      cts_psy_agg_ind_sev == 2 ~ 'Severe Psychological Aggression'
    ),
    cts_psy_agg_ind_sev = as.factor(cts_psy_agg_ind_sev),
    cts_psy_agg_ind_sev = relevel(cts_psy_agg_ind_sev, 'No Psychological Aggression')
  ) %>%
  ggplot(
    aes(
      cts_psy_agg_ind_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Individual (TC) Psychological Aggression Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_injury_ind_sev = case_when(
      cts_injury_ind_sev == 0 ~ 'No Injury',
      cts_injury_ind_sev == 1 ~ 'Minor Injury',
      cts_injury_ind_sev == 2 ~ 'Severe Injury'
    ),
    cts_injury_ind_sev = as.factor(cts_injury_ind_sev),
    cts_injury_ind_sev = relevel(cts_injury_ind_sev, 'No Injury')
  ) %>%
  ggplot(
    aes(
      cts_injury_ind_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Individual (TC) Physical Injury Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_assault_ind_sev = case_when(
      cts_assault_ind_sev == 0 ~ 'No Assault',
      cts_assault_ind_sev == 1 ~ 'Minor Assault',
      cts_assault_ind_sev == 2 ~ 'Severe Assault'
    ),
    cts_assault_ind_sev = as.factor(cts_assault_ind_sev),
    cts_assault_ind_sev = relevel(cts_assault_ind_sev, 'No Assault')
  ) %>%
  ggplot(
    aes(
      cts_assault_ind_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Individual (TC) Assault Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_sex_ind_sev = case_when(
      cts_sex_ind_sev == 0 ~ 'No Sexual Cohesion',
      cts_sex_ind_sev == 1 ~ 'Minor Sexual Cohesion',
      cts_sex_ind_sev == 2 ~ 'Severe Sexual Cohesion'
    ),
    cts_sex_ind_sev = as.factor(cts_sex_ind_sev),
    cts_sex_ind_sev = relevel(cts_sex_ind_sev, 'No Sexual Cohesion')
  ) %>%
  ggplot(
    aes(
      cts_sex_ind_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Individual (TC) Sexual Cohesion Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_psy_agg_part_sev = case_when(
      cts_psy_agg_part_sev == 0 ~ 'No Psychological Aggression',
      cts_psy_agg_part_sev == 1 ~ 'Minor Psychological Aggression',
      cts_psy_agg_part_sev == 2 ~ 'Severe Psychological Aggression'
    ),
    cts_psy_agg_part_sev = as.factor(cts_psy_agg_part_sev),
    cts_psy_agg_part_sev = relevel(cts_psy_agg_part_sev, 'No Psychological Aggression')
  ) %>%
  ggplot(
    aes(
      cts_psy_agg_part_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Partner Psychological Aggression Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_injury_part_sev = case_when(
      cts_injury_part_sev == 0 ~ 'No Injury',
      cts_injury_part_sev == 1 ~ 'Minor Injury',
      cts_injury_part_sev == 2 ~ 'Severe Injury'
    ),
    cts_injury_part_sev = as.factor(cts_injury_part_sev),
    cts_injury_part_sev = relevel(cts_injury_part_sev, 'No Injury')
  ) %>%
  ggplot(
    aes(
      cts_injury_part_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Partner Physical Injury Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_assault_part_sev = case_when(
      cts_assault_part_sev == 0 ~ 'No Assault',
      cts_assault_part_sev == 1 ~ 'Minor Assault',
      cts_assault_part_sev == 2 ~ 'Severe Assault'
    ),
    cts_assault_part_sev = as.factor(cts_assault_part_sev),
    cts_assault_part_sev = relevel(cts_assault_part_sev, 'No Assault')
  ) %>%
  ggplot(
    aes(
      cts_assault_part_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Partner Assault Severity',
       x = '')

cts_complete %>%
  mutate(
    cts_sex_part_sev = case_when(
      cts_sex_part_sev == 0 ~ 'No Sexual Cohesion',
      cts_sex_part_sev == 1 ~ 'Minor Sexual Cohesion',
      cts_sex_part_sev == 2 ~ 'Severe Sexual Cohesion'
    ),
    cts_sex_part_sev = as.factor(cts_sex_part_sev),
    cts_sex_part_sev = relevel(cts_sex_part_sev, 'No Sexual Cohesion')
  ) %>%
  ggplot(
    aes(
      cts_sex_part_sev
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Partner Sexual Cohesion Severity',
       x = '')

cts_complete %>%
  pivot_longer(
    cols = c(cts_psy_agg_ind_prev, cts_psy_agg_part_prev),
    names_to = 'ind_part',
    values_to = 'prevalence'
  ) %>%
  mutate(
    ind_part = case_when(
      ind_part == 'cts_psy_agg_ind_prev' ~ 'Individual (TC)',
      ind_part == 'cts_psy_agg_part_prev' ~ 'Partner'
    )
  ) %>%
  group_by(ind_part) %>%
  mutate(
    sum_prev = sum(prevalence)
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      ind_part, sum_prev
    )
  ) +
  geom_col(
    aes(fill = ind_part),
    position = 'dodge'
  ) +
  labs(title = 'Distribution of Psychological Aggression Prevalence')

cts_complete %>%
  pivot_longer(
    cols = c(cts_injury_ind_prev, cts_injury_part_prev),
    names_to = 'ind_part',
    values_to = 'prevalence'
  ) %>%
  mutate(
    ind_part = case_when(
      ind_part == 'cts_injury_ind_prev' ~ 'Individual (TC)',
      ind_part == 'cts_injury_part_prev' ~ 'Partner'
    )
  ) %>%
  group_by(ind_part) %>%
  mutate(
    sum_prev = sum(prevalence)
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      ind_part, sum_prev
    )
  ) +
  geom_col(
    aes(fill = ind_part),
    position = 'dodge'
  ) +
  labs(title = 'Distribution of Physical Injury Prevalence')

cts_complete %>%
  pivot_longer(
    cols = c(cts_assault_ind_prev, cts_assault_part_prev),
    names_to = 'ind_part',
    values_to = 'prevalence'
  ) %>%
  mutate(
    ind_part = case_when(
      ind_part == 'cts_assault_ind_prev' ~ 'Individual (TC)',
      ind_part == 'cts_assault_part_prev' ~ 'Partner'
    )
  ) %>%
  group_by(ind_part) %>%
  mutate(
    sum_prev = sum(prevalence)
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      ind_part, sum_prev
    )
  ) +
  geom_col(
    aes(fill = ind_part),
    position = 'dodge'
  ) +
  labs(title = 'Distribution of Assault Prevalence')

cts_complete %>%
  pivot_longer(
    cols = c(cts_sex_ind_prev, cts_sex_part_prev),
    names_to = 'ind_part',
    values_to = 'prevalence'
  ) %>%
  mutate(
    ind_part = case_when(
      ind_part == 'cts_sex_ind_prev' ~ 'Individual (TC)',
      ind_part == 'cts_sex_part_prev' ~ 'Partner'
    )
  ) %>%
  group_by(ind_part) %>%
  mutate(
    sum_prev = sum(prevalence)
  ) %>%
  ungroup() %>%
  ggplot(
    aes(
      ind_part, sum_prev
    )
  ) +
  geom_col(
    aes(fill = ind_part),
    position = 'dodge'
  ) +
  labs(title = 'Distribution of Sexual Cohesion Prevalence')

cts_complete %>%
  mutate(
    cts_psy_agg_mutual = case_when(
      cts_psy_agg_mutual == 0 ~ 'Neither',
      cts_psy_agg_mutual == 1 ~ 'Male Only',
      cts_psy_agg_mutual == 2 ~ 'Female Only',
      cts_psy_agg_mutual == 3 ~ 'Both'
    ),
    cts_psy_agg_mutual = as.factor(cts_psy_agg_mutual),
    cts_psy_agg_mutual = relevel(cts_psy_agg_mutual, 'Neither')
  ) %>%
  ggplot(
    aes(
      cts_psy_agg_mutual
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Mutual Psychological Aggression')

cts_complete %>%
  mutate(
    cts_injury_mutual = case_when(
      cts_injury_mutual == 0 ~ 'Neither',
      cts_injury_mutual == 1 ~ 'Male Only',
      cts_injury_mutual == 2 ~ 'Female Only',
      cts_injury_mutual == 3 ~ 'Both'
    ),
    cts_injury_mutual = as.factor(cts_injury_mutual),
    cts_injury_mutual = relevel(cts_injury_mutual, 'Neither')
  ) %>%
  ggplot(
    aes(
      cts_injury_mutual
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Mutual Injury')

cts_complete %>%
  mutate(
    cts_assault_mutual = case_when(
      cts_assault_mutual == 0 ~ 'Neither',
      cts_assault_mutual == 1 ~ 'Male Only',
      cts_assault_mutual == 2 ~ 'Female Only',
      cts_assault_mutual == 3 ~ 'Both'
    ),
    cts_assault_mutual = as.factor(cts_assault_mutual),
    cts_assault_mutual = relevel(cts_assault_mutual, 'Neither')
  ) %>%
  ggplot(
    aes(
      cts_assault_mutual
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Mutual Assault')

cts_complete %>%
  mutate(
    cts_sex_mutual = case_when(
      cts_sex_mutual == 0 ~ 'Neither',
      cts_sex_mutual == 1 ~ 'Male Only',
      cts_sex_mutual == 2 ~ 'Female Only',
      cts_sex_mutual == 3 ~ 'Both'
    ),
    cts_sex_mutual = as.factor(cts_sex_mutual),
    cts_sex_mutual = relevel(cts_sex_mutual, 'Neither')
  ) %>%
  ggplot(
    aes(
      cts_sex_mutual
    )
  ) +
  geom_bar() +
  labs(title = 'Distribution of Mutual Sexual Cohesion')

cts_complete %>%
  composite_hist(
    cts_negotiate_ind,
    bins = 10
  ) +
  labs(title = 'CTS2S Negotiation Subscale Distribution',
       subtitle = 'Individual (TC)')

cts_complete %>%
  composite_hist(
    cts_negotiate_part,
    bins = 10
  ) +
  labs(title = 'CTS2S Negotiation Subscale Distribution',
       subtitle = 'Partner')

