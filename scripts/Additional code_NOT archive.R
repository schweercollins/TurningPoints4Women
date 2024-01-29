# Additional code that we are not archive 
##########################################################################################
#### mch --------
## let's chat about this tomorrow; I think I want to remove all diagnoses from the codebook but let's retain this code elsewhere for our "internal purposes"

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
  ) %>%
  relocate(h4:h14, .after = "h3")