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

###########################################################################################
 # PCL rename ####
# we decided to combine all relationship question for archive
## c1 == relation0-5
## c2 == relation6-11
## c3 == relation12-19
## c4 == relationadult
## d == learned_relation_allAge
## all text just add - txt
##########################################################################################
pcl <- pcl %>%
  rename_with(
  .fn = ~ gsub("pcl(\\d+)c1", "pcl\\1_relation0-5", .),
  .cols = matches("pcl\\d+c1$")
) %>%
rename_with(
  .fn = ~ gsub("pcl(\\d+)c2", "pcl\\1_relation6-11", .),
  .cols = matches("pcl(\\d+)c2")
) %>%
rename_with(
  .fn = ~ gsub("pcl(\\d+)c3", "pcl\\1_relation12-19", .),
  .cols = matches("pcl(\\d+)c3")
) %>%
rename_with(
  .fn = ~ gsub("pcl(\\d+)c4", "pcl\\1_relationadult", .),
  .cols = matches("pcl(\\d+)c4")
) %>%

rename_with(
  .fn = ~ gsub("pcl(\\d+)d", "pcl\\1_learned_relation_allAge", .),
  .cols = matches("pcl(\\d+)d")
  )


# keep the relationship columns seperated by age
pcl <- pcl %>%

  # PCL1
  mutate(pcl1c1 = combine_columns(., c("pcl1c1_1", "pcl1c1_2", "pcl1c1_3", "pcl1c1_9999", "pcl1c1_77"), sep = ";") )%>%
  mutate(pcl1c2 = combine_columns(., c("pcl1c2_1", "pcl1c2_2", "pcl1c2_3", "pcl1c2_9999", "pcl1c2_77"), sep = ";") )%>%
  mutate(pcl1c3 = combine_columns(., c("pcl1c3_1", "pcl1c3_2", "pcl1c3_3", "pcl1c3_9999", "pcl1c3_77"), sep = ";") )%>%
  mutate(pcl1c4 = combine_columns(., c("pcl1c4_1", "pcl1c4_2", "pcl1c4_3", "pcl1c4_9999", "pcl1c4_77"), sep = ";") )%>%
  mutate(pcl1d = combine_columns(., c("pcl1d_1", "pcl1d_2", "pcl1d_3", "pcl1d_77"), sep = ";") )%>%
  relocate(pcl1c1, .after = "pcl1c1_77") %>%
  relocate(pcl1c2, .after = "pcl1c2_77") %>%
  relocate(pcl1c3, .after = "pcl1c3_77") %>%
  relocate(pcl1c4, .after = "pcl1c4_77") %>%
  relocate(pcl1d, .after = "pcl1d_77") %>%
  select(-c("pcl1c1_1", "pcl1c1_2", "pcl1c1_3", "pcl1c1_9999", "pcl1c1_77",
            "pcl1c2_1", "pcl1c2_2", "pcl1c2_3", "pcl1c2_9999", "pcl1c2_77",
            "pcl1c3_1", "pcl1c3_2", "pcl1c3_3", "pcl1c3_9999", "pcl1c3_77",
            "pcl1c4_1", "pcl1c4_2", "pcl1c4_3", "pcl1c4_9999", "pcl1c4_77",
            "pcl1d_1", "pcl1d_2", "pcl1d_3", "pcl1c1_77")) %>%
  # PCL2
  mutate(pcl2c1 = combine_columns(., c("pcl2c1_1", "pcl2c1_2", "pcl2c1_3", "pcl2c1_9999", "pcl2c1_77"), sep = ";") )%>%
  mutate(pcl2c2 = combine_columns(., c("pcl2c2_1", "pcl2c2_2", "pcl2c2_3", "pcl2c2_9999", "pcl2c2_77"), sep = ";") )%>%
  mutate(pcl2c3 = combine_columns(., c("pcl2c3_1", "pcl2c3_2", "pcl2c3_3", "pcl2c3_9999", "pcl2c3_77"), sep = ";") )%>%
  mutate(pcl2c4 = combine_columns(., c("pcl2c4_1", "pcl2c4_2", "pcl2c4_3", "pcl2c4_9999", "pcl2c4_77"), sep = ";") )%>%
  mutate(pcl2d = combine_columns(., c("pcl2d_1", "pcl2d_2", "pcl2d_3", "pcl2d_77"), sep = ";") )%>%
  relocate(pcl2c1, .after = "pcl2c1_77") %>%
  relocate(pcl2c2, .after = "pcl2c2_77") %>%
  relocate(pcl2c3, .after = "pcl2c3_77") %>%
  relocate(pcl2c4, .after = "pcl2c4_77") %>%
  relocate(pcl2d, .after = "pcl2d_77") %>%
  select(-c("pcl2c1_1", "pcl2c1_2", "pcl2c1_3", "pcl2c1_9999", "pcl2c1_77",
            "pcl2c2_1", "pcl2c2_2", "pcl2c2_3", "pcl2c2_9999", "pcl2c2_77",
            "pcl2c3_1", "pcl2c3_2", "pcl2c3_3", "pcl2c3_9999", "pcl2c3_77",
            "pcl2c4_1", "pcl2c4_2", "pcl2c4_3", "pcl2c4_9999", "pcl2c4_77",
            "pcl2d_1", "pcl2d_2", "pcl2d_3", "pcl2d_77")) %>%
  # PCL3
  mutate(pcl3c1 = combine_columns(., c("pcl3c1_1", "pcl3c1_2", "pcl3c1_3", "pcl3c1_9999", "pcl3c1_77"), sep = ";") )%>%
  mutate(pcl3c2 = combine_columns(., c("pcl3c2_1", "pcl3c2_2", "pcl3c2_3", "pcl3c2_9999", "pcl3c2_77"), sep = ";") )%>%
  mutate(pcl3c3 = combine_columns(., c("pcl3c3_1", "pcl3c3_2", "pcl3c3_3", "pcl3c3_9999", "pcl3c3_77"), sep = ";") )%>%
  mutate(pcl3c4 = combine_columns(., c("pcl3c4_1", "pcl3c4_2", "pcl3c4_3", "pcl3c4_9999", "pcl3c4_77"), sep = ";") )%>%
  mutate(pcl3d = combine_columns(., c("pcl3d_1", "pcl3d_2", "pcl3d_3", "pcl3d_77"), sep = ";") )%>%
  relocate(pcl3c1, .after = "pcl3c1_77") %>%
  relocate(pcl3c2, .after = "pcl3c2_77") %>%
  relocate(pcl3c3, .after = "pcl3c3_77") %>%
  relocate(pcl3c4, .after = "pcl3c4_77") %>%
  relocate(pcl3d, .after = "pcl3d_77") %>%
  select(-c("pcl3c1_1", "pcl3c1_2", "pcl3c1_3", "pcl3c1_9999", "pcl3c1_77",
            "pcl3c2_1", "pcl3c2_2", "pcl3c2_3", "pcl3c2_9999", "pcl3c2_77",
            "pcl3c3_1", "pcl3c3_2", "pcl3c3_3", "pcl3c3_9999", "pcl3c3_77",
            "pcl3c4_1", "pcl3c4_2", "pcl3c4_3", "pcl3c4_9999", "pcl3c4_77",
            "pcl3d_1", "pcl3d_2", "pcl3d_3", "pcl3d_77")) %>%
  # PCL4
  mutate(pcl4c1 = combine_columns(., c("pcl4c1_1", "pcl4c1_2", "pcl4c1_3", "pcl4c1_9999", "pcl4c1_77"), sep = ";") )%>%
  mutate(pcl4c2 = combine_columns(., c("pcl4c2_1", "pcl4c2_2", "pcl4c2_3", "pcl4c2_9999", "pcl4c2_77"), sep = ";") )%>%
  mutate(pcl4c3 = combine_columns(., c("pcl4c3_1", "pcl4c3_2", "pcl4c3_3", "pcl4c3_9999", "pcl4c3_77"), sep = ";") )%>%
  mutate(pcl4c4 = combine_columns(., c("pcl4c4_1", "pcl4c4_2", "pcl4c4_3", "pcl4c4_9999", "pcl4c4_77"), sep = ";") )%>%
  mutate(pcl4d = combine_columns(., c("pcl4d_1", "pcl4d_2", "pcl4d_3", "pcl4d_77"), sep = ";") )%>%
  relocate(pcl4c1, .after = "pcl4c1_77") %>%
  relocate(pcl4c2, .after = "pcl4c2_77") %>%
  relocate(pcl4c3, .after = "pcl4c3_77") %>%
  relocate(pcl4c4, .after = "pcl4c4_77") %>%
  relocate(pcl4d, .after = "pcl4d_77") %>%
  select(-c("pcl4c1_1", "pcl4c1_2", "pcl4c1_3", "pcl4c1_9999", "pcl4c1_77",
            "pcl4c2_1", "pcl4c2_2", "pcl4c2_3", "pcl4c2_9999", "pcl4c2_77",
            "pcl4c3_1", "pcl4c3_2", "pcl4c3_3", "pcl4c3_9999", "pcl4c3_77",
            "pcl4c4_1", "pcl4c4_2", "pcl4c4_3", "pcl4c4_9999", "pcl4c4_77",
            "pcl4d_1", "pcl4d_2", "pcl4d_3", "pcl4d_77")) %>%
  # PCL5
  mutate(pcl5c1 = combine_columns(., c("pcl5c1_1", "pcl5c1_2", "pcl5c1_3", "pcl5c1_9999", "pcl5c1_77"), sep = ";") )%>%
  mutate(pcl5c2 = combine_columns(., c("pcl5c2_1", "pcl5c2_2", "pcl5c2_3", "pcl5c2_9999", "pcl5c2_77"), sep = ";") )%>%
  mutate(pcl5c3 = combine_columns(., c("pcl5c3_1", "pcl5c3_2", "pcl5c3_3", "pcl5c3_9999", "pcl5c3_77"), sep = ";") )%>%
  mutate(pcl5c4 = combine_columns(., c("pcl5c4_1", "pcl5c4_2", "pcl5c4_3", "pcl5c4_9999", "pcl5c4_77"), sep = ";") )%>%
  mutate(pcl5d = combine_columns(., c("pcl5d_1", "pcl5d_2", "pcl5d_3", "pcl5d_77"), sep = ";") )%>%
  relocate(pcl5c1, .after = "pcl5c1_77") %>%
  relocate(pcl5c2, .after = "pcl5c2_77") %>%
  relocate(pcl5c3, .after = "pcl5c3_77") %>%
  relocate(pcl5c4, .after = "pcl5c4_77") %>%
  relocate(pcl5d, .after = "pcl5d_77") %>%
  select(-c("pcl5c1_1", "pcl5c1_2", "pcl5c1_3", "pcl5c1_9999", "pcl5c1_77",
            "pcl5c2_1", "pcl5c2_2", "pcl5c2_3", "pcl5c2_9999", "pcl5c2_77",
            "pcl5c3_1", "pcl5c3_2", "pcl5c3_3", "pcl5c3_9999", "pcl5c3_77",
            "pcl5c4_1", "pcl5c4_2", "pcl5c4_3", "pcl5c4_9999", "pcl5c4_77",
            "pcl5d_1", "pcl5d_2", "pcl5d_3", "pcl5d_77")) %>%
  # PCL6
  mutate(pcl6c1 = combine_columns(., c("pcl6c1_1", "pcl6c1_2", "pcl6c1_3", "pcl6c1_9999", "pcl6c1_77"), sep = ";") )%>%
  mutate(pcl6c2 = combine_columns(., c("pcl6c2_1", "pcl6c2_2", "pcl6c2_3", "pcl6c2_9999", "pcl6c2_77"), sep = ";") )%>%
  mutate(pcl6c3 = combine_columns(., c("pcl6c3_1", "pcl6c3_2", "pcl6c3_3", "pcl6c3_9999", "pcl6c3_77"), sep = ";") )%>%
  mutate(pcl6c4 = combine_columns(., c("pcl6c4_1", "pcl6c4_2", "pcl6c4_3", "pcl6c4_9999", "pcl6c4_77"), sep = ";") )%>%
  mutate(pcl6d = combine_columns(., c("pcl6d_1", "pcl6d_2", "pcl6d_3", "pcl6d_77"), sep = ";") )%>%
  relocate(pcl6c1, .after = "pcl6c1_77") %>%
  relocate(pcl6c2, .after = "pcl6c2_77") %>%
  relocate(pcl6c3, .after = "pcl6c3_77") %>%
  relocate(pcl6c4, .after = "pcl6c4_77") %>%
  relocate(pcl6d, .after = "pcl6d_77") %>%
  select(-c("pcl6c1_1", "pcl6c1_2", "pcl6c1_3", "pcl6c1_9999", "pcl6c1_77",
            "pcl6c2_1", "pcl6c2_2", "pcl6c2_3", "pcl6c2_9999", "pcl6c2_77",
            "pcl6c3_1", "pcl6c3_2", "pcl6c3_3", "pcl6c3_9999", "pcl6c3_77",
            "pcl6c4_1", "pcl6c4_2", "pcl6c4_3", "pcl6c4_9999", "pcl6c4_77",
            "pcl6d_1", "pcl6d_2", "pcl6d_3", "pcl6d_77")) %>%
  # PCL7
  mutate(pcl7c1 = combine_columns(., c("pcl7c1_1", "pcl7c1_2", "pcl7c1_3", "pcl7c1_9999", "pcl7c1_77"), sep = ";") )%>%
  mutate(pcl7c2 = combine_columns(., c("pcl7c2_1", "pcl7c2_2", "pcl7c2_3", "pcl7c2_9999", "pcl7c2_77"), sep = ";") )%>%
  mutate(pcl7c3 = combine_columns(., c("pcl7c3_1", "pcl7c3_2", "pcl7c3_3", "pcl7c3_9999", "pcl7c3_77"), sep = ";") )%>%
  mutate(pcl7c4 = combine_columns(., c("pcl7c4_1", "pcl7c4_2", "pcl7c4_3", "pcl7c4_9999", "pcl7c4_77"), sep = ";") )%>%
  mutate(pcl7d = combine_columns(., c("pcl7d_1", "pcl7d_2", "pcl7d_3", "pcl7d_77"), sep = ";") )%>%
  relocate(pcl7c1, .after = "pcl7c1_77") %>%
  relocate(pcl7c2, .after = "pcl7c2_77") %>%
  relocate(pcl7c3, .after = "pcl7c3_77") %>%
  relocate(pcl7c4, .after = "pcl7c4_77") %>%
  relocate(pcl7d, .after = "pcl7d_77") %>%
  select(-c("pcl7c1_1", "pcl7c1_2", "pcl7c1_3", "pcl7c1_9999", "pcl7c1_77",
            "pcl7c2_1", "pcl7c2_2", "pcl7c2_3", "pcl7c2_9999", "pcl7c2_77",
            "pcl7c3_1", "pcl7c3_2", "pcl7c3_3", "pcl7c3_9999", "pcl7c3_77",
            "pcl7c4_1", "pcl7c4_2", "pcl7c4_3", "pcl7c4_9999", "pcl7c4_77",
            "pcl7d_1", "pcl7d_2", "pcl7d_3", "pcl7d_77")) %>%
  # PCL8
  mutate(pcl8c1 = combine_columns(., c("pcl8c1_1", "pcl8c1_2", "pcl8c1_3", "pcl8c1_9999", "pcl8c1_77"), sep = ";") )%>%
  mutate(pcl8c2 = combine_columns(., c("pcl8c2_1", "pcl8c2_2", "pcl8c2_3", "pcl8c2_9999", "pcl8c2_77"), sep = ";") )%>%
  mutate(pcl8c3 = combine_columns(., c("pcl8c3_1", "pcl8c3_2", "pcl8c3_3", "pcl8c3_9999", "pcl8c3_77"), sep = ";") )%>%
  mutate(pcl8c4 = combine_columns(., c("pcl8c4_1", "pcl8c4_2", "pcl8c4_3", "pcl8c4_9999", "pcl8c4_77"), sep = ";") )%>%
  mutate(pcl8d = combine_columns(., c("pcl8d_1", "pcl8d_2", "pcl8d_3", "pcl8d_77"), sep = ";") )%>%
  relocate(pcl8c1, .after = "pcl8c1_77") %>%
  relocate(pcl8c2, .after = "pcl8c2_77") %>%
  relocate(pcl8c3, .after = "pcl8c3_77") %>%
  relocate(pcl8c4, .after = "pcl8c4_77") %>%
  relocate(pcl8d, .after = "pcl8d_77") %>%
  select(-c("pcl8c1_1", "pcl8c1_2", "pcl8c1_3", "pcl8c1_9999", "pcl8c1_77",
            "pcl8c2_1", "pcl8c2_2", "pcl8c2_3", "pcl8c2_9999", "pcl8c2_77",
            "pcl8c3_1", "pcl8c3_2", "pcl8c3_3", "pcl8c3_9999", "pcl8c3_77",
            "pcl8c4_1", "pcl8c4_2", "pcl8c4_3", "pcl8c4_9999", "pcl8c4_77",
            "pcl8d_1", "pcl8d_2", "pcl8d_3", "pcl8d_77")) %>%
  # PCL9
  mutate(pcl9c1 = combine_columns(., c("pcl9c1_1", "pcl9c1_2", "pcl9c1_3", "pcl9c1_9999", "pcl9c1_77"), sep = ";") )%>%
  mutate(pcl9c2 = combine_columns(., c("pcl9c2_1", "pcl9c2_2", "pcl9c2_3", "pcl9c2_9999", "pcl9c2_77"), sep = ";") )%>%
  mutate(pcl9c3 = combine_columns(., c("pcl9c3_1", "pcl9c3_2", "pcl9c3_3", "pcl9c3_9999", "pcl9c3_77"), sep = ";") )%>%
  mutate(pcl9c4 = combine_columns(., c("pcl9c4_1", "pcl9c4_2", "pcl9c4_3", "pcl9c4_9999", "pcl9c4_77"), sep = ";") )%>%
  mutate(pcl9d = combine_columns(., c("pcl9d_1", "pcl9d_2", "pcl9d_3", "pcl9d_77"), sep = ";") )%>%
  relocate(pcl9c1, .after = "pcl9c1_77") %>%
  relocate(pcl9c2, .after = "pcl9c2_77") %>%
  relocate(pcl9c3, .after = "pcl9c3_77") %>%
  relocate(pcl9c4, .after = "pcl9c4_77") %>%
  relocate(pcl9d, .after = "pcl9d_77") %>%
  select(-c("pcl9c1_1", "pcl9c1_2", "pcl9c1_3", "pcl9c1_9999", "pcl9c1_77",
            "pcl9c2_1", "pcl9c2_2", "pcl9c2_3", "pcl9c2_9999", "pcl9c2_77",
            "pcl9c3_1", "pcl9c3_2", "pcl9c3_3", "pcl9c3_9999", "pcl9c3_77",
            "pcl9c4_1", "pcl9c4_2", "pcl9c4_3", "pcl9c4_9999", "pcl9c4_77",
            "pcl9d_1", "pcl9d_2", "pcl9d_3", "pcl9d_77")) %>%
  # PCL10
  mutate(pcl10c1 = combine_columns(., c("pcl10c1_1", "pcl10c1_2", "pcl10c1_3", "pcl10c1_9999", "pcl10c1_77"), sep = ";") )%>%
  mutate(pcl10c2 = combine_columns(., c("pcl10c2_1", "pcl10c2_2", "pcl10c2_3", "pcl10c2_9999", "pcl10c2_77"), sep = ";") )%>%
  mutate(pcl10c3 = combine_columns(., c("pcl10c3_1", "pcl10c3_2", "pcl10c3_3", "pcl10c3_9999", "pcl10c3_77"), sep = ";") )%>%
  mutate(pcl10c4 = combine_columns(., c("pcl10c4_1", "pcl10c4_2", "pcl10c4_3", "pcl10c4_9999", "pcl10c4_77"), sep = ";") )%>%
  mutate(pcl10d = combine_columns(., c("pcl10d_1", "pcl10d_2", "pcl10d_3", "pcl10d_77"), sep = ";") )%>%
  relocate(pcl10c1, .after = "pcl10c1_77") %>%
  relocate(pcl10c2, .after = "pcl10c2_77") %>%
  relocate(pcl10c3, .after = "pcl10c3_77") %>%
  relocate(pcl10c4, .after = "pcl10c4_77") %>%
  relocate(pcl10d, .after = "pcl10d_77") %>%
  select(-c("pcl10c1_1", "pcl10c1_2", "pcl10c1_3", "pcl10c1_9999", "pcl10c1_77",
            "pcl10c2_1", "pcl10c2_2", "pcl10c2_3", "pcl10c2_9999", "pcl10c2_77",
            "pcl10c3_1", "pcl10c3_2", "pcl10c3_3", "pcl10c3_9999", "pcl10c3_77",
            "pcl10c4_1", "pcl10c4_2", "pcl10c4_3", "pcl10c4_9999", "pcl10c4_77",
            "pcl10d_1", "pcl10d_2", "pcl10d_3", "pcl10d_77")) %>%
  # PCL11
  mutate(pcl11c1 = combine_columns(., c("pcl11c_1", "pcl11c_2", "pcl11c_3", "pcl11c_9999", "pcl11c_77"), sep = ";") )%>%
  mutate(pcl11c2 = combine_columns(., c("pcl11c2_1", "pcl11c2_2", "pcl11c2_3", "pcl11c2_9999", "pcl11c2_77"), sep = ";") )%>%
  mutate(pcl11c3 = combine_columns(., c("pcl11c3_1", "pcl11c3_2", "pcl11c3_3", "pcl11c3_9999", "pcl11c3_77"), sep = ";") )%>%
  mutate(pcl11c4 = combine_columns(., c("pcl11c4_1", "pcl11c4_2", "pcl11c4_3", "pcl11c4_9999", "pcl11c4_77"), sep = ";") )%>%
  mutate(pcl11d = combine_columns(., c("pcl11d_1", "pcl11d_2", "pcl11d_3", "pcl11d_77"), sep = ";") )%>%
  relocate(pcl11c1, .after = "pcl11c_77") %>%
  relocate(pcl11c2, .after = "pcl11c2_77") %>%
  relocate(pcl11c3, .after = "pcl11c3_77") %>%
  relocate(pcl11c4, .after = "pcl11c4_77") %>%
  relocate(pcl11d, .after = "pcl11d_77") %>%
  select(-c("pcl11c_1", "pcl11c_2", "pcl11c_3", "pcl11c_9999", "pcl11c_77",
            "pcl11c2_1", "pcl11c2_2", "pcl11c2_3", "pcl11c2_9999", "pcl11c2_77",
            "pcl11c3_1", "pcl11c3_2", "pcl11c3_3", "pcl11c3_9999", "pcl11c3_77",
            "pcl11c4_1", "pcl11c4_2", "pcl11c4_3", "pcl11c4_9999", "pcl11c4_77",
            "pcl11d_1", "pcl11d_2", "pcl11d_3", "pcl11d_77")) %>%
  # PCL12
  mutate(pcl12c1 = combine_columns(., c("pcl12c1_1", "pcl12c1_2", "pcl12c1_3", "pcl12c1_9999", "pcl12c1_77"), sep = ";") )%>%
  mutate(pcl12c2 = combine_columns(., c("pcl12c2_1", "pcl12c2_2", "pcl12c2_3", "pcl12c2_9999", "pcl12c2_77"), sep = ";") )%>%
  mutate(pcl12c3 = combine_columns(., c("pcl12c3_1", "pcl12c3_2", "pcl12c3_3", "pcl12c3_9999", "pcl12c3_77"), sep = ";") )%>%
  mutate(pcl12c4 = combine_columns(., c("pcl12c4_1", "pcl12c4_2", "pcl12c4_3", "pcl12c4_9999", "pcl12c4_77"), sep = ";") )%>%
  mutate(pcl12d = combine_columns(., c("pcl12d_1", "pcl12d_2", "pcl12d_3", "pcl12d_77"), sep = ";") )%>%
  relocate(pcl12c1, .after = "pcl12c1_77") %>%
  relocate(pcl12c2, .after = "pcl12c2_77") %>%
  relocate(pcl12c3, .after = "pcl12c3_77") %>%
  relocate(pcl12c4, .after = "pcl12c4_77") %>%
  relocate(pcl12d, .after = "pcl12d_77") %>%
  select(-c("pcl12c1_1", "pcl12c1_2", "pcl12c1_3", "pcl12c1_9999", "pcl12c1_77",
            "pcl12c2_1", "pcl12c2_2", "pcl12c2_3", "pcl12c2_9999", "pcl12c2_77",
            "pcl12c3_1", "pcl12c3_2", "pcl12c3_3", "pcl12c3_9999", "pcl12c3_77",
            "pcl12c4_1", "pcl12c4_2", "pcl12c4_3", "pcl12c4_9999", "pcl12c4_77",
            "pcl12d_1", "pcl12d_2", "pcl12d_3", "pcl12d_77")) %>%
  # PCL13
  mutate(pcl13c1 = combine_columns(., c("pcl13c1_1", "pcl13c1_2", "pcl13c1_3", "pcl13c1_9999", "pcl13c1_77"), sep = ";") )%>%
  mutate(pcl13c2 = combine_columns(., c("pcl13c2_1", "pcl13c2_2", "pcl13c2_3", "pcl13c2_9999", "pcl13c2_77"), sep = ";") )%>%
  mutate(pcl13c3 = combine_columns(., c("pcl13c3_1", "pcl13c3_2", "pcl13c3_3", "pcl13c3_9999", "pcl13c3_77"), sep = ";") )%>%
  mutate(pcl13c4 = combine_columns(., c("pcl13c4_1", "pcl13c4_2", "pcl13c4_3", "pcl13c4_9999", "pcl13c4_77"), sep = ";") )%>%
  mutate(pcl13d = combine_columns(., c("pcl13d_1", "pcl13d_2", "pcl13d_3", "pcl13d_77"), sep = ";") )%>%
  relocate(pcl13c1, .after = "pcl13c1_77") %>%
  relocate(pcl13c2, .after = "pcl13c2_77") %>%
  relocate(pcl13c3, .after = "pcl13c3_77") %>%
  relocate(pcl13c4, .after = "pcl13c4_77") %>%
  relocate(pcl13d, .after = "pcl13d_77") %>%
  select(-c("pcl13c1_1", "pcl13c1_2", "pcl13c1_3", "pcl13c1_9999", "pcl13c1_77",
            "pcl13c2_1", "pcl13c2_2", "pcl13c2_3", "pcl13c2_9999", "pcl13c2_77",
            "pcl13c3_1", "pcl13c3_2", "pcl13c3_3", "pcl13c3_9999", "pcl13c3_77",
            "pcl13c4_1", "pcl13c4_2", "pcl13c4_3", "pcl13c4_9999", "pcl13c4_77",
            "pcl13d_1", "pcl13d_2", "pcl13d_3", "pcl13d_77")) %>%
  # PCL14
  mutate(pcl14c1 = combine_columns(., c("pcl14c1_1", "pcl14c1_2", "pcl14c1_3", "pcl14c1_9999", "pcl14c1_77"), sep = ";") )%>%
  mutate(pcl14c2 = combine_columns(., c("pcl14c2_1", "pcl14c2_2", "pcl14c2_3", "pcl14c2_9999", "pcl14c2_77"), sep = ";") )%>%
  mutate(pcl14c3 = combine_columns(., c("pcl14c3_1", "pcl14c3_2", "pcl14c3_3", "pcl14c3_9999", "pcl14c3_77"), sep = ";") )%>%
  mutate(pcl14c4 = combine_columns(., c("pcl14c4_1", "pcl14c4_2", "pcl14c4_3", "pcl14c4_9999", "pcl14c4_77"), sep = ";") )%>%
  mutate(pcl14d = combine_columns(., c("pcl14d_1", "pcl14d_2", "pcl14d_3", "pcl14d_77"), sep = ";") )%>%
  relocate(pcl14c1, .after = "pcl14c1_77") %>%
  relocate(pcl14c2, .after = "pcl14c2_77") %>%
  relocate(pcl14c3, .after = "pcl14c3_77") %>%
  relocate(pcl14c4, .after = "pcl14c4_77") %>%
  relocate(pcl14d, .after = "pcl14d_77") %>%
  select(-c("pcl14c1_1", "pcl14c1_2", "pcl14c1_3", "pcl14c1_9999", "pcl14c1_77",
            "pcl14c2_1", "pcl14c2_2", "pcl14c2_3", "pcl14c2_9999", "pcl14c2_77",
            "pcl14c3_1", "pcl14c3_2", "pcl14c3_3", "pcl14c3_9999", "pcl14c3_77",
            "pcl14c4_1", "pcl14c4_2", "pcl14c4_3", "pcl14c4_9999", "pcl14c4_77",
            "pcl14d_1", "pcl14d_2", "pcl14d_3", "pcl14d_77")) %>%
  # PCL15
  mutate(pcl15c1 = combine_columns(., c("pcl15c1_1", "pcl15c1_2", "pcl15c1_3", "pcl15c1_9999", "pcl15c1_77"), sep = ";") )%>%
  mutate(pcl15c2 = combine_columns(., c("pcl15c2_1", "pcl15c2_2", "pcl15c2_3", "pcl15c2_9999", "pcl15c2_77"), sep = ";") )%>%
  mutate(pcl15c3 = combine_columns(., c("pcl15c3_1", "pcl15c3_2", "pcl15c3_3", "pcl15c3_9999", "pcl15c3_77"), sep = ";") )%>%
  mutate(pcl15c4 = combine_columns(., c("pcl15c4_1", "pcl15c4_2", "pcl15c4_3", "pcl15c4_9999", "pcl15c4_77"), sep = ";") )%>%
  mutate(pcl15d = combine_columns(., c("pcl15d_1", "pcl15d_2", "pcl15d_3", "pcl15d_77"), sep = ";") )%>%
  relocate(pcl15c1, .after = "pcl15c1_77") %>%
  relocate(pcl15c2, .after = "pcl15c2_77") %>%
  relocate(pcl15c3, .after = "pcl15c3_77") %>%
  relocate(pcl15c4, .after = "pcl15c4_77") %>%
  relocate(pcl15d, .after = "pcl15d_77") %>%
  select(-c("pcl15c1_1", "pcl15c1_2", "pcl15c1_3", "pcl15c1_9999", "pcl15c1_77",
            "pcl15c2_1", "pcl15c2_2", "pcl15c2_3", "pcl15c2_9999", "pcl15c2_77",
            "pcl15c3_1", "pcl15c3_2", "pcl15c3_3", "pcl15c3_9999", "pcl15c3_77",
            "pcl15c4_1", "pcl15c4_2", "pcl15c4_3", "pcl15c4_9999", "pcl15c4_77",
            "pcl15d_1", "pcl15d_2", "pcl15d_3", "pcl15d_77")) %>%
  # PCL16
  mutate(pcl16c1 = combine_columns(., c("pcl16c1_1", "pcl16c1_2", "pcl16c1_3", "pcl16c1_9999", "pcl16c1_77"), sep = ";") )%>%
  mutate(pcl16c2 = combine_columns(., c("pcl16c2_1", "pcl16c2_2", "pcl16c2_3", "pcl16c2_9999", "pcl16c2_77"), sep = ";") )%>%
  mutate(pcl16c3 = combine_columns(., c("pcl16c3_1", "pcl16c3_2", "pcl16c3_3", "pcl16c3_9999", "pcl16c3_77"), sep = ";") )%>%
  mutate(pcl16c4 = combine_columns(., c("pcl16c4_1", "pcl16c4_2", "pcl16c4_3", "pcl16c4_9999", "pcl16c4_77"), sep = ";") )%>%
  mutate(pcl16d = combine_columns(., c("pcl16d_1", "pcl16d_2", "pcl16d_3", "pcl16d_77"), sep = ";") )%>%
  relocate(pcl16c1, .after = "pcl16c1_77") %>%
  relocate(pcl16c2, .after = "pcl16c2_77") %>%
  relocate(pcl16c3, .after = "pcl16c3_77") %>%
  relocate(pcl16c4, .after = "pcl16c4_77") %>%
  relocate(pcl16d, .after = "pcl16d_77") %>%
  select(-c("pcl16c1_1", "pcl16c1_2", "pcl16c1_3", "pcl16c1_9999", "pcl16c1_77",
            "pcl16c2_1", "pcl16c2_2", "pcl16c2_3", "pcl16c2_9999", "pcl16c2_77",
            "pcl16c3_1", "pcl16c3_2", "pcl16c3_3", "pcl16c3_9999", "pcl16c3_77",
            "pcl16c4_1", "pcl16c4_2", "pcl16c4_3", "pcl16c4_9999", "pcl16c4_77",
            "pcl16d_1", "pcl16d_2", "pcl16d_3", "pcl16d_77")) %>%
  # PCL17
  mutate(pcl17c1 = combine_columns(., c("pcl17c1_1", "pcl17c1_2", "pcl17c1_3", "pcl17c1_9999", "pcl17c1_77"), sep = ";") )%>%
  mutate(pcl17c2 = combine_columns(., c("pcl17c2_1", "pcl17c2_2", "pcl17c2_3", "pcl17c2_9999", "pcl17c2_77"), sep = ";") )%>%
  mutate(pcl17c3 = combine_columns(., c("pcl17c3_1", "pcl17c3_2", "pcl17c3_3", "pcl17c3_9999", "pcl17c3_77"), sep = ";") )%>%
  mutate(pcl17c4 = combine_columns(., c("pcl17c4_1", "pcl17c4_2", "pcl17c4_3", "pcl17c4_9999", "pcl17c4_77"), sep = ";") )%>%
  mutate(pcl17d = combine_columns(., c("pcl17d_1", "pcl17d_2", "pcl17d_3", "pcl17d_77"), sep = ";") )%>%
  relocate(pcl17c1, .after = "pcl17c1_77") %>%
  relocate(pcl17c2, .after = "pcl17c2_77") %>%
  relocate(pcl17c3, .after = "pcl17c3_77") %>%
  relocate(pcl17c4, .after = "pcl17c4_77") %>%
  relocate(pcl17d, .after = "pcl17d_77") %>%
  select(-c("pcl17c1_1", "pcl17c1_2", "pcl17c1_3", "pcl17c1_9999", "pcl17c1_77",
            "pcl17c2_1", "pcl17c2_2", "pcl17c2_3", "pcl17c2_9999", "pcl17c2_77",
            "pcl17c3_1", "pcl17c3_2", "pcl17c3_3", "pcl17c3_9999", "pcl17c3_77",
            "pcl17c4_1", "pcl17c4_2", "pcl17c4_3", "pcl17c4_9999", "pcl17c4_77",
            "pcl17d_1", "pcl17d_2", "pcl17d_3", "pcl17d_77")) %>%
  # PCL18
  mutate(pcl18c1 = combine_columns(., c("pcl18c1_1", "pcl18c1_2", "pcl18c1_3", "pcl18c1_9999", "pcl18c1_77"), sep = ";") )%>%
  mutate(pcl18c2 = combine_columns(., c("pcl18c2_1", "pcl18c2_2", "pcl18c2_3", "pcl18c2_9999", "pcl18c2_77"), sep = ";") )%>%
  mutate(pcl18c3 = combine_columns(., c("pcl18c3_1", "pcl18c3_2", "pcl18c3_3", "pcl18c3_9999", "pcl18c3_77"), sep = ";") )%>%
  mutate(pcl18c4 = combine_columns(., c("pcl18c4_1", "pcl18c4_2", "pcl18c4_3", "pcl18c4_9999", "pcl18c4_77"), sep = ";") )%>%
  mutate(pcl18d = combine_columns(., c("pcl18d_1", "pcl18d_2", "pcl18d_3", "pcl18d_77"), sep = ";") )%>%
  relocate(pcl18c1, .after = "pcl18c1_77") %>%
  relocate(pcl18c2, .after = "pcl18c2_77") %>%
  relocate(pcl18c3, .after = "pcl18c3_77") %>%
  relocate(pcl18c4, .after = "pcl18c4_77") %>%
  relocate(pcl18d, .after = "pcl18d_77") %>%
  select(-c("pcl18c1_1", "pcl18c1_2", "pcl18c1_3", "pcl18c1_9999", "pcl18c1_77",
            "pcl18c2_1", "pcl18c2_2", "pcl18c2_3", "pcl18c2_9999", "pcl18c2_77",
            "pcl18c3_1", "pcl18c3_2", "pcl18c3_3", "pcl18c3_9999", "pcl18c3_77",
            "pcl18c4_1", "pcl18c4_2", "pcl18c4_3", "pcl18c4_9999", "pcl18c4_77",
            "pcl18d_1", "pcl18d_2", "pcl18d_3", "pcl18d_77")) %>%
  # PCL19
  mutate(pcl19c1 = combine_columns(., c("pcl19c1_1", "pcl19c1_2", "pcl19c1_3", "pcl19c1_9999", "pcl19c1_77"), sep = ";") )%>%
  mutate(pcl19c2 = combine_columns(., c("pcl19c2_1", "pcl19c2_2", "pcl19c2_3", "pcl19c2_9999", "pcl19c2_77"), sep = ";") )%>%
  mutate(pcl19c3 = combine_columns(., c("pcl19c3_1", "pcl19c3_2", "pcl19c3_3", "pcl19c3_9999", "pcl19c3_77"), sep = ";") )%>%
  mutate(pcl19c4 = combine_columns(., c("pcl19c4_1", "pcl19c4_2", "pcl19c4_3", "pcl19c4_9999", "pcl19c4_77"), sep = ";") )%>%
  mutate(pcl19d = combine_columns(., c("pcl19d_1", "pcl19d_2", "pcl19d_3", "pcl19d_77"), sep = ";") )%>%
  relocate(pcl19c1, .after = "pcl19c1_77") %>%
  relocate(pcl19c2, .after = "pcl19c2_77") %>%
  relocate(pcl19c3, .after = "pcl19c3_77") %>%
  relocate(pcl19c4, .after = "pcl19c4_77") %>%
  relocate(pcl19d, .after = "pcl19d_77") %>%
  select(-c("pcl19c1_1", "pcl19c1_2", "pcl19c1_3", "pcl19c1_9999", "pcl19c1_77",
            "pcl19c2_1", "pcl19c2_2", "pcl19c2_3", "pcl19c2_9999", "pcl19c2_77",
            "pcl19c3_1", "pcl19c3_2", "pcl19c3_3", "pcl19c3_9999", "pcl19c3_77",
            "pcl19c4_1", "pcl19c4_2", "pcl19c4_3", "pcl19c4_9999", "pcl19c4_77",
            "pcl19d_1", "pcl19d_2", "pcl19d_3", "pcl19d_77"))

# We are not drop columns for archive
## Removing any columns in which there are only NAs
## this drop 105 columns that aren't necessary
# pcltest <- pcl %>%
#   select(-which(sapply(pcl, function(col) all(is.na(col) | col == "NA"))))





# PCT OPTION 2 #########################################################################
pct_complete <- pct_calc %>%
  mutate(
    pct_preg_total = rowSums(select(., pct2a:pct2d), na.rm = TRUE),
    pct_childbirth_total = rowSums(select(., pct3a:pct3d), na.rm = TRUE)
  ) %>%
  # making those with missing data have appropriate missing data codes
  mutate(
    pct_preg_total = ifelse(p00a == 0, "-99", ifelse(is.na(pct_preg_total), "-99", as.character(pct_preg_total))),
    pct_childbirth_total = ifelse(p00a == 0, "-99", ifelse(is.na(pct_childbirth_total), "-99", as.character(pct_childbirth_total)))
  ) %>%
  mutate_at(vars(pct_childbirth_total), ~case_when(id =="P836" ~ -99, TRUE ~.))
# Adding back in -55 and -77
mutate_at(vars(pct1:pct3d), ~ifelse(p00a == 0, "-55", as.character(.))) %>%
  mutate_at(vars(pct2a:pct3d), ~ifelse(pct1 == 0, "-55", as.character(.))) %>%
  mutate(
    pct3a = ifelse(id == "P836" & is.na(pct3a), -77, pct3a),
    pct3b = ifelse(id == "P836" & is.na(pct3b), -77, pct3b),
    pct3c = ifelse(id == "P836" & is.na(pct3c), -77, pct3c),
    pct3d = ifelse(id == "P836" & is.na(pct3d), -77, pct3d))


# MHC diagnoses #########################################################################
mhc_complete <- mhc_complete %>%
  pivot_longer(
    h4:h14,
    names_to = "pre_flourish",
    values_to = "flourish_feel"
  ) %>%
  mutate(
    mhc_flourish = case_when(
      flourish_feel %in% c(4, 5) ~ 1,
      !flourish_feel %in% c(4, 5) ~ 0,
      TRUE ~ 0
    ),
    mhc_languish = case_when(
      flourish_feel %in% c(0, 1) ~ 1,
      !flourish_feel %in% c(0, 1) ~ 0,
      TRUE ~ 0
    )
  ) %>%
  group_by(id) %>%
  mutate(
    mhc_six_or_over = sum(mhc_flourish, na.rm = TRUE),
    mhc_neg_six_or_more = sum(mhc_languish, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    mhc_flourish_diagnosis = case_when(
      (h1 %in% c(4, 5) & mhc_six_or_over >= 6) |
        (h2 %in% c(4, 5) & mhc_six_or_over >= 6) |
        (h3 %in% c(4, 5) & mhc_six_or_over >= 6)
      ~ "flourish",
      TRUE ~ "not_flourish"
    ),
    mhc_languish_diagnosis = case_when(
      (h1 %in% c(0, 1) & mhc_neg_six_or_more >= 6) |
        (h2 %in% c(0, 1) & mhc_neg_six_or_more >= 6) |
        (h3 %in% c(0, 1) & mhc_neg_six_or_more >= 6)
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
    mhc_final_diagnosis = case_when(
      (mhc_flourish_diagnosis == "flourish" &
         mhc_languish_diagnosis == "not_languish") ~ "flourish",
      (mhc_flourish_diagnosis == "not_flourish" &
         mhc_languish_diagnosis == "languish") ~ "languish",
      (mhc_flourish_diagnosis == "not_flourish" &
         mhc_languish_diagnosis == "not_languish") ~ "moderately mentally healthy"
    )
  ) %>%
  relocate(h4:h14, .after = "h3")


# BCAP SCORING #########################################################################
### BCAP Missing Data
# There are missing data in BCAP and missing rule not applied.

bcap %>%
  select(-c(q1,q2,q23,q29)) %>%
  mutate(across(-id, as.numeric)) %>%
  pct_miss_fun(
    id = 'id',
    n_items = 34
  ) %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'BCAP Missing Data',
    subtitle = 'By Participant'
  )


# Min can you save this in another internal file with the following note. I am going to archive these data without any subscales because we don't have the correct information for scoring.

## Need to update scoring to include only three subscales according to BCAP reference, these should be:

#  The following items make up the BCAP random responding scale:  31, 53, and 61, with 31 being reverse-scored.  If any one of these items is endorsed, the protocol may be invalid.

# The following items make up the BCAP lie scale:  12, 44, 57, 66, 106, and 146.  If four or more are endorsed, the protocol may be invalid (although subsequent research with urban parents has suggested that the BCAP may discriminate between known high-risk and control parents better when the lie and random responding scales are not used).

# Main scale is The remaining items make up the BCAP risk scale, with items 14, 75, and 107—the three items all loading on the “happiness” factor—being reverse-scored (as per CAP instructions).  Thus, the BCAP risk scale score can range from 0 to 24.  In the development study cited above, a BCAP cutoff of 9 best predicted the risk distinction of the full CAP using the cutoff of 166, and a BCAP cutoff of 12 best predicted the CAP cutoff of 215.
bcap_calc <-
  bcap %>%
  pct_miss_fun(
    id = 'id',
    n_items = 34
  ) %>%
  full_join(bcap,
            by = 'id') %>%
  filter(
    is.na(miss_pct) |
      miss_pct < 20
  ) %>%
  mutate(
    across(
      .cols = c(-id, -miss_pct, -missing_n),
      .fns = ~case_when(
        .x == -77 ~ NA_real_,
        .x == -55 ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

bcap_complete <-
  bcap_calc %>%
  select(
    -missing_n,
    -miss_pct
  ) %>%
  # index_total_fun(
  #   id = 'id'
  # ) %>%
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
    bcap_risk = total1,
    bcap_happy = total2,
    bcap_feel_pers = total3,
    bcap_lonely = total4,
    bcap_fam_conf = total5,
    bcap_rigid = total6,
    bcap_distress = total7,
    bcap_poverty = total8,
    bcap_lie = total9,
    bcap_random = total10
  )
