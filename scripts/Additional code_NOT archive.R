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
### BCAP Reliability from codebook  #########################################################################
#Calculate internal reliability
# Total
bcap_alpha <-
  complete %>%
  select(
    bcap1_r,
    bcap2_r,
    bcap3:bcap22,
    bcap23_r,
    bcap24:bcap28,
    bcap29_r,
    bcap30:bcap34
  ) %>%
  psych::alpha(check.keys = TRUE)

bcap_risk_alpha <-
  complete %>%
  select(
    bcap1_r,
    bcap3,
    bcap5:bcap8,
    bcap10:bcap14,
    bcap16:bcap17,
    bcap19:bcap20,
    bcap22,
    bcap23_r,
    bcap24:bcap25,
    bcap27,
    bcap29_r,
    bcap30:bcap33
  ) %>%
  psych::alpha(check.keys = TRUE)

bcap_happy_alpha <-
  complete %>%
  select(bcap1_r, bcap23_r, bcap29_r) %>%
  psych::alpha(check.keys = TRUE)

bcap_feel_pers_alpha <-
  complete %>%
  select(bcap3, bcap25, bcap33) %>%
  psych::alpha(check.keys = TRUE)

bcap_lonely_alpha <-
  complete %>%
  select(bcap5, bcap12, bcap22, bcap31) %>%
  psych::alpha(check.keys = TRUE)

bcap_fam_conf_alpha <-
  complete %>%
  select(bcap6, bcap13, bcap17) %>%
  psych::alpha(check.keys = TRUE)

bcap_rigid_alpha <-
  complete %>%
  select(bcap7, bcap14, bcap20, bcap32) %>%
  psych::alpha(check.keys = TRUE)

bcap_distress_alpha <-
  complete %>%
  select(bcap8, bcap11, bcap16, bcap19, bcap27) %>%
  psych::alpha(check.keys = TRUE)

bcap_poverty_alpha <-
  complete %>%
  select(bcap10, bcap30) %>%
  psych::alpha(check.keys = TRUE)

bcap_lie_alpha <-
  complete %>%
  select(bcap4, bcap9, bcap15, bcap21, bcap26, bcap34) %>%
  psych::alpha(check.keys = TRUE)

bcap_random_alpha <-
  complete %>%
  select(bcap2_r, bcap19, bcap28) %>%
  psych::alpha(check.keys = TRUE)
# BCAP Scale table
# Create the table as a data frame
bcap_scale_df <- data.frame(
  var_name = c("bcap_total", "bcap_risk", "bcap_happy", "bcap_feel_pers", "bcap_lonely", "bcap_fam_conf", "bcap_rigid","bcap_distress", "bcap_poverty", "bcap_lie", "bcap_random"),
  scale = c("BCAP complete Scale (34)", "BCAP Risk Scale (26)", "Happiness (3)", "Feelings of persecution", "Loneliness (4)", "Family conflict (3)", "Rigidity (4)", "Distress (5)", "Financial Insecurity (2)", "Lie (6)", "Random responding (3)"),
  scale_construction = c("`bcap1_r`,`bcap2_r`, `bcap3`, `bcap4`, `bcap5`, `bcap6`, `bcap7`, `bcap8`, `bcap9`, `bcap10`, `bcap11`, `bcap12`, `bcap13`, `bcap14`, `bcap15`, `bcap16`, `bcap17`, `bcap18`, `bcap19`, `bcap20`, `bcap21`, `bcap22`, `bcap23_r`, `bcap24`, `bcap25`, `bcap26`, `bcap27`, `bcap28`, `bcap29_r`, `bcap30`, `bcap31`, `bcap32`, `bcap33`, `bcap34`",
                         "`bcap1_r`, `bcap3`, `bcap4`, `bcap5`, `bcap6`, `bcap7`, `bcap8`, `bcap10`, `bcap11`, `bcap12`, `bcap13`, `bcap14`, `bcap16`, `bcap17`, `bcap19`, `bcap20`, `bcap22`, `bcap23_r`, `bcap24`, `bcap25`, `bcap27`, `bcap29_r`, `bcap30`, `bcap31`, `bcap32`, `bcap33`",
                         "`bcap1_r`, `bcap23_r`, `bcap29_r`",
                         "`bcap3`, `bcap25`, `bcap33`",
                         "`bcap5`, `bcap12`, `bcap22`, `bcap31`",
                         "`bcap6`, `bcap13`, `bcap17`",
                         "`bcap7`, `bcap14`, `bcap20`, `bcap32`",
                         "`bcap8`, `bcap11`, `bcap16`, `bcap19`, `bcap27`",
                         "`bcap10`, `bcap30`",
                         "`bcap4`, `bcap9`, `bcap15`, `bcap21`, `bcap26`, `bcap34`,",
                         "`bcap2`, `bcap18`, `bcap28`"),
  scale_range = c("0-24","","","","","","","","","", ""), #TODO fill in range
  alpha = c(round(bcap_alpha$total$raw_alpha, 3),
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

# Create the gt table object
bcap_scale_table <- gt(bcap_scale_df) %>%
  cols_align(columns = c("scale_range"), align = "center") %>%
  cols_label(var_name = "Variable Name",
             scale = "Scale (# of items)",
             scale_construction = "Scale Construction",
             scale_range = "Range*",
             alpha = "Alpha") %>%
  tab_footnote(footnote = "*Range of possible values; `_r` Reverse scored variable") %>%
  tab_options(table.border.bottom.style = "hidden") %>%
  #cols_width(scale_construction ~ px(135)) %>%
  tab_options(table.width = pct(100))%>%
  apply_tbl_theme()

# Print the table
print(bcap_scale_table)


# For continuous total and subscale scores:


# Create subset of bcap items
bcap_data_subset <- complete %>%
  select(
    bcap_risk, bcap_happy, bcap_feel_pers, bcap_lonely, bcap_fam_conf, bcap_rigid, bcap_distress, bcap_poverty, bcap_lie, bcap_random)

# Calculate the summary statistics for each variable
bcap_summary_data <- lapply(bcap_data_subset, function(x) {
  mean_val <- sprintf("%.2f", round(mean(x, na.rm = TRUE), 2))
  sd_val <- round(sd(x, na.rm = TRUE), 2)
  range_val <- paste(min(x, na.rm = TRUE), max(x, na.rm = TRUE), sep = "-")
  miss_val <- round(mean(is.na(x)) * 100, 2)
  c(Mean = mean_val, SD = sd_val, Range = range_val, Missing = paste(miss_val, "%", sep = ""))
})

# Convert summary_data to a data frame
bcap_summary_data <- as.data.frame(bcap_summary_data)

# Set items as first row
bcap_summary_data <- rbind(names(bcap_summary_data), bcap_summary_data) %>%
  t() %>%
  as.data.frame()

# Create the gt table
bcap_tbl <- bcap_summary_data %>%
  gt() %>%
  cols_label(`1` = "Variable",
             Range = "Range*",
             Missing = "% Missing") %>%
  cols_width(1 ~ px(165),2:5 ~ px(115)) %>%
  tab_options(table.width = pct(100))%>%
  cols_align(columns = c("Mean", "SD", "Range", "Missing"), align = "center") %>%
  tab_footnote("*Range of data values") %>%
  tab_options(table.border.bottom.style = "hidden") %>%
  apply_tbl_theme()

# Render the table
bcap_tbl



#### Distributions

# Distributions are presented below for the total score and each subscale:

 # ```{r bcap-distributions, class.source='fold', echo = FALSE}
#Risk
p_bcap_risk <- complete %>%
  composite_hist(
    x = bcap_risk
  ) +
  labs(
    title = 'Risk'
  )

# Happiness
p_bcap_happy <- complete %>%
  composite_hist(
    x = bcap_happy,
    bins = 5
  ) +
  labs(
    title = 'Happiness'
  )

# Feelings
p_bcap_pers <- complete %>%
  composite_hist(
    x = bcap_feel_pers,
    bins = 5
  ) +
  labs(
    title = 'Feelings of Persecution'
  )

# Loneliness
p_bacp_lonely <- complete %>%
  composite_hist(
    x = bcap_lonely,
    bins = 5
  ) +
  labs(
    title = 'Loneliness'
  )

# Family Conflict
p_bcap_fam_c<-complete %>%
  composite_hist(
    x = bcap_fam_conf,
    bins = 5
  ) +
  labs(
    title = 'Family Conflict'
  )

# Rigidity
p_bcap_rig <- complete %>%
  composite_hist(
    x = bcap_rigid,
    bins = 5
  ) +
  labs(
    title = 'Rigidity'
  )

# Distress
p_bcap_dis <- complete %>%
  composite_hist(
    x = bcap_distress,
    bins = 5
  ) +
  labs(
    title = 'Distress'
  )

# Poverty
p_bcap_prov <- complete %>%
  composite_hist(
    x = bcap_poverty,
    bins = 5
  ) +
  labs(
    title = 'Poverty'
  )

# Lying
p_bcap_lie <- complete %>%
  composite_hist(
    x = bcap_lie,
    bins = 5
  ) +
  labs(
    title = 'Lying'
  )

# Random Responding
p_bcap_rand <- complete %>%
  composite_hist(
    x = bcap_random,
    bins = 5
  ) +
  labs(
    title = 'Random Responding'
  )

ggarrange(p_bcap_risk, p_bcap_happy + rremove("ylab"),
          p_bcap_pers+ rremove("ylab"), p_bacp_lonely+ rremove("ylab"),
          p_bcap_fam_c, p_bcap_rig+ rremove("ylab"),
          p_bcap_dis+ rremove("ylab"), p_bcap_dis+ rremove("ylab"),
          p_bcap_prov, p_bcap_lie+ rremove("ylab"),
          p_bcap_rand+ rremove("ylab")
) %>%
  annotate_figure(top = text_grob( "Distribution for bcap Subscale \n(x-axis and y-axis are in different scales)"))



### EDS-F Calculation & Subscale #########################################################################
elf_miss <- elf %>%
  mutate(across(-id, as.numeric)) %>%
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
## Min can you remove any computation of scales for the EDS-F measure?
## Please save this in a separate file for our internal work
## there should be no distributions in the codebook then; no alpha either; you can remove these notes once you clean it up
elf_calc <-
  elf %>%
  mutate(
    across(
      .cols = -c(
        id
      ),
      .fns = ~case_when(
        .x < 0 ~ NA,
        TRUE ~ .x
      )
    )
  ) %>%
  mutate(across(-id, as.numeric))

elf_complete <-
  elf_calc %>%
  composite_total_avg_fun(
    id = c('id', "elf1", "elf2", "elf3", "elf7",
           "elf12", "elf13", "elf15", "elf17", "elf18",
           "elf19", "elf23", "elf26", "elf28", "elf30",
           "elf34", "elf35", "elf36", "elf37"),
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
    elf_avg = mean_values #TODO: Maria do we need average for this measure
  ) %>%
  mutate(
    mutate(
      across(
        -c(id
        ),
        ~case_when(
          id %in% elf_remove ~ NA_real_,
          TRUE ~ .x
        )
      )
    )
  ) %>%
  relocate(elf1:elf3, .before = "elf4") %>%
  relocate(elf7, .before = "elf8") %>%
  relocate(elf12:elf13, .before = "elf14") %>%
  relocate(elf15, .before = "elf16") %>%
  relocate(elf17:elf19, .before = "elf20") %>%
  relocate(elf23, .before = "elf24") %>%
  relocate(elf26, .before = "elf27") %>%
  relocate(elf28, .before = "elf29") %>%
  relocate(elf30, .before = "elf31") %>%
  relocate(elf34:elf37, .before = "elf38")

# composite_total_avg_fun(
#   id = c(
#     'id', "elf1_c", "elf2_c", "elf3_c", "elf7_c",
#     "elf12_c", "elf13_c", "elf15_c", "elf17_c", "elf18_c",
#     "elf19_c", "elf23_c", "elf26_c", "elf28_c", "elf30_c",
#     "elf34_c", "elf35_c", "elf36_c", "elf37_c"
#   ),
#   max_value = 4,
#   n_items = 21
# ) %>%
# distinct(
#   across(
#     .cols = everything()
#   )
# ) %>%
# rename(
#   elf_total = sum_values,
#   elf_avg = mean_values)


#### Scale and Subscales
# From Code book
#Information on the total score and subscale variables are below.
#Variables were c onstructed by summing included items.

# EDS #########################################################################################
#Calculate internal reliability


#Calculate internal reliability
eds_alpha <- complete %>%
  select(elf1:elf39) %>%
  drop_na() %>%  # TODO: Shaina: I don't know why does including NA doesn't work for the alpha function.
  psych::alpha(check.keys = TRUE)

#```

#TODO: all revers coded column is 0, need to double check, I used
#non-revers coded score for now

#```{r EDS-scale-table, results = 'asis'}


# Create the table as a data frame
eds_scale_df <- data.frame(
  var_name = c("eds_total"),
  scale = c("EDS Total score (39)"),
  scale_construction = c("`elf1`, `elf2`, `elf3`, `elf4`, `elf5`, `elf6`, `elf7`,
  `elf8`, `elf9`, `elf10`, `elf11`, `elf12`, `elf13`, `elf14`, `elf15`, `elf16`,
  `elf17`, `elf18`, `elf19`, `elf20`, `elf21`, `elf22`, `elf23`, `elf24`, `elf25`,
  `elf26`, `elf27`, `elf28`, `elf29`, `elf30`, `elf31`, `elf32`, `elf33`, `elf34`,
                         `elf35`, `elf36`, `elf37`, `elf38`, `elf39`"),
  scale_range = c("39-159"), #TODO fill in range
  alpha = c(round(eds_alpha$total$raw_alpha, 3))
)

# Create the gt table object
eds_scale_df <- gt(eds_scale_df) %>%
  cols_align(columns = c("scale_range"), align = "center") %>%
  cols_label(var_name = "Variable Name",
             scale = "Scale (# of items)",
             scale_construction = "Scale Construction",
             scale_range = "Range*",
             alpha = "Alpha") %>%
  tab_footnote(footnote = "*Range of possible values; `_r` Reverse scored variable") %>%
  tab_options(table.border.bottom.style = "hidden") %>%
  cols_width(scale ~ px(135)) %>%
  apply_tbl_theme()

# Print the table
print(eds_scale_df)

#### Distributions

# Distributions are presented below for the total score and each subscale:

#  ```{r eds-distributions, class.source='fold', echo = FALSE}

eds %>%
  composite_hist(
    x = elf_total
  ) +
  labs(title = "Distribution of Total Scores for the Stigma & Shame Questionnaire")

#```


#socinf_sub  ####################################
socinf_sub_miss <- socinf_sub %>%
  pct_miss_fun(
    id = "id",
    n_items = 49
  )

socinf_sub_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'socinf_sub Missing Data',
    subtitle = 'By Participant')
#socinf_gen
socinf_gen_miss <- socinf_gen %>%
  mutate(across(-c(id), as.numeric))%>%
  pct_miss_fun(
    id = c("id"),
    n_items = 17
  )


socinf_gen_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'SOCINF General Missing Data',
    subtitle = 'By Participant')

# CINT _SUBSTANCE
# cint_substance_miss <- cint_substance %>%
#   mutate(across(-c(id, matches("_6_text$")), as.numeric))%>%
#   pct_miss_fun(
#     id = c("id", matches("_6_text$")),
#     n_items = 47
#   )
#
# cint_substance_miss%>% gt::gt() %>%
#   gt::tab_header(
#     title = 'CINT- Subtance use Missing Data',
#     subtitle = 'By Participant')
#
#
#
# # Below is the code that treats -77, -55 values as NA
# cint_substance_complete <-
#   cint_substance %>%
# mutate(across(-c(id, matches("_6_text$"), cint84), as.numeric)) %>% # cint84 is multiple choice
#   mutate(
#     across(
#       .cols = -c(id, matches("_6_text$")),
#       .fns = ~case_when(
#         .x < 0 ~ NA,
#         TRUE ~ .x
#       )
#     ),
#     across(
#       .cols = matches("_6_text$"),
#       .fns = ~case_when(
#         .x == -55 ~ NA,
#         TRUE ~ .x
#       )
#     )
#   )

# ELL ######################

# '{.col}_c' delinquency behavior coded count (if delinquecy reported then coded as 1)
# "{.col}_r" re-code with cap at 7
ell <- ell %>%
  # TODO: Maria, this section need special attention
  #  select(-ell23a) %>%
  mutate(
    # Create ell count columns
    across(
      .cols = matches("\\d$"),
      .fns = ~case_when(
        .x == 0 ~ 0,
        .x > 0 ~ 1, # everyone reported delinquency behavior coded as 1
        TRUE ~ .x
      ),
      .names = '{.col}_c'
    ),
    # TODO :Maria:JP also covert number larger than 4 to 4 in this code, but the SPSS Syntax cap at 7 which is what I used below. If this is correct, please delete this comment.
    across(
      .cols = matches("\\d$"),   # TODO: Maria, JP used column end with a to create this recoded column, but based on spss I think it should be column without a, this is what I used for this code. Please confirm this is correct.
      .names = "{.col}_r"
    ),
    across(
      c(
        matches("_r$")
      ),
      ~case_when(
        .x >= 7 ~ 7, # cap at 7
        TRUE ~ .x
      )
    )
  )



# TODO: Min, you can remove code pertaining the the physical aggression; you can also save the other code in our internal file with a note that the general delinquency needs to be z scored to deal with different reporting waves across the study (12 vs 6 month recall) before using in conjunction with other waves
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
      "ell4_r", "ell5_r",
      "ell6_r", "ell8_r", "ell9_r",
      "ell10_r", "ell11_r", "ell14_r", "ell16_r", "ell20_r",
      "ell21_r", "ell22_r", "ell24_r", "ell25_r", "ell27_r", "ell29_r",
      "ell31_r", "ell32_r", "ell33_r", "ell38_r", "ell39_r"
    )
  ) %>%
  rename(
    ell_raw_total = total1,
    ell_censor_total = total2
  )


ell_count_complete <-
  ell_complete %>%
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
      -c(id
      ),
      ~case_when(
        id %in% ell_count_remove ~ NA_real_,
        TRUE ~ .x
      )
    )
  )

ell_count_complete <- ell_count_complete%>%
  select(
    order(names(.))
  ) %>%
  relocate(ell1a:ell2_r, .after = "ell1_r") %>%
  relocate(ell3a:ell7_r, .after = "ell3_r") %>%
  relocate(ell7a:ell9a, .after = "ell7_r") %>%
  relocate(id, .before = "ell1") %>%
  relocate(ell_censor_total:ell_raw_total, .after = "ell39a")



# SUS ###########################
# We decided to including all variable even they are empty for entire columns
## Now we will drop columns that have no data (only -55)
# Check if every value in each column is equal to -55 or -99 or -77 or some combo thereof
# columns_to_drop <- names(sus)[sapply(sus, function(col)
#   all(col %in% c("-55", "-99", "-77")) ||
#   all(col == "-55") ||
#   all(col == "-99") ||
#   all(col == "-77") ||
#   (all(col %in% c("-55", "-99")) && any(col %in% "-77")) ||
#   (all(col %in% c("-55", "-77")) && any(col %in% "-99")) ||
#   (all(col %in% c("-99", "-77")) && any(col %in% "-55"))
# )]
#
#
# # Drop the columns where every value is -55 or -99 or -77 or combo
# # this should drop 10 empty comlumns
# sus <- sus[, !names(sus) %in% columns_to_drop]




# pct_miss <- pct %>%
#   #mutate(across(-c(id), as.numeric)) %>%
#   pct_miss_fun(
#     id = c("id"),
#     n_items = 13
#   )
#
# pct_miss %>%
#   gt::gt() %>%
#   gt::tab_header(
#     title = 'PCT Missing Data',
#     subtitle = 'by Participant'
#   )
# # primarily missing due to not having prior pregnancy (p00a) or due to not having a traumatic childbirth (pct1)
# pct_remove <- pct_miss %>% filter(miss_pct >= 20) %>% filter(id != "P836") %>% pull(id)
# # above we retain P836 because they are only missing items from one subscale

# IBQ Missing ###########################
ibq_miss <- ibq %>%
  mutate(across(-c(id,ibq10_other_text), as.numeric))%>%
  select(id, ibq1:ibq7) %>%  # only ibq1:ibq7 was used to calculated ibq_total
  pct_miss_fun(
    id = c("id"),
    n_items = 7
  )

ibq_miss %>%
  gt::gt() %>%
  gt::tab_header(
    title = 'IBQ Missing Data',
    subtitle = 'By Participant')

ibq_remove <- ibq_miss %>% filter(miss_pct >= 30) %>% pull(id)


# pcl code find better code
# pcl <- pcl %>%
#   mutate(pcl1_happen = case_when(
#     pcl1_77 == -77 ~ "-77",
#     is.na(pcl1_happen) & is.na(pcl1_77) ~ "0",
#     TRUE ~ pcl1_happen
#   ),
#   pcl2_happen = case_when(
#     pcl2_77 == -77 ~ "-77",
#     is.na(pcl2_happen) & is.na(pcl2_77) ~ "0",
#     TRUE ~ pcl2_happen
#   ),
#   pcl3_happen = case_when(
#     pcl3_77 == -77 ~ "-77",
#     is.na(pcl3_happen) & is.na(pcl3_77) ~ "0",
#     TRUE ~ pcl3_happen
#   ),
#   pcl4_happen = case_when(
#     pcl4_77 == -77 ~ "-77",
#     is.na(pcl4_happen) & is.na(pcl4_77) ~ "0",
#     TRUE ~ pcl4_happen
#   ),
#   pcl5_happen = case_when(
#     pcl5_77 == -77 ~ "-77",
#     is.na(pcl5_happen) & is.na(pcl5_77) ~ "0",
#     TRUE ~ pcl5_happen
#   ),
#   pcl6_happen = case_when(
#     pcl6_77 == -77 ~ "-77",
#     is.na(pcl6_happen) & is.na(pcl6_77) ~ "0",
#     TRUE ~ pcl6_happen
#   ),
#   pcl7_happen = case_when(
#     pcl7_77 == -77 ~ "-77",
#     is.na(pcl7_happen) & is.na(pcl7_77) ~ "0",
#     TRUE ~ pcl7_happen
#   ),
#   pcl8_happen = case_when(
#     pcl8_77 == -77 ~ "-77",
#     is.na(pcl8_happen) & is.na(pcl8_77) ~ "0",
#     TRUE ~ pcl8_happen
#   ),
#   pcl9_happen = case_when(
#     pcl9_77 == -77 ~ "-77",
#     is.na(pcl9_happen) & is.na(pcl9_77) ~ "0",
#     TRUE ~ pcl9_happen
#   ),
#   pcl10_happen = case_when(
#     pcl10_77 == -77 ~ "-77",
#     is.na(pcl10_happen) & is.na(pcl10_77) ~ "0",
#     TRUE ~ pcl10_happen
#   ),
#   pcl11_happen = case_when(
#     pcl11_77 == -77 ~ "-77",
#     is.na(pcl11_happen) & is.na(pcl11_77) ~ "0",
#     TRUE ~ pcl11_happen
#   ),
#   pcl12_happen = case_when(
#     pcl12_77 == -77 ~ "-77",
#     is.na(pcl12_happen) & is.na(pcl12_77) ~ "0",
#     TRUE ~ pcl12_happen
#   ),
#   pcl13_happen = case_when(
#     pcl13_77 == -77 ~ "-77",
#     is.na(pcl13_happen) & is.na(pcl13_77) ~ "0",
#     TRUE ~ pcl13_happen
#   ),
#   pcl14_happen = case_when(
#     pcl14_77 == -77 ~ "-77",
#     is.na(pcl14_happen) & is.na(pcl14_77) ~ "0",
#     TRUE ~ pcl14_happen
#   ),
#   pcl15_happen = case_when(
#     pcl15_77 == -77 ~ "-77",
#     is.na(pcl15_happen) & is.na(pcl15_77) ~ "0",
#     TRUE ~ pcl15_happen
#   ),
#   pcl16_happen = case_when(
#     pcl16_77 == -77 ~ "-77",
#     is.na(pcl16_happen) & is.na(pcl16_77) ~ "0",
#     TRUE ~ pcl16_happen
#   ),
#   pcl17_happen = case_when(
#     pcl17_77 == -77 ~ "-77",
#     is.na(pcl17_happen) & is.na(pcl17_77) ~ "0",
#     TRUE ~ pcl17_happen
#   ),
#   pcl18_happen = case_when(
#     pcl18_77 == -77 ~ "-77",
#     is.na(pcl18_happen) & is.na(pcl18_77) ~ "0",
#     TRUE ~ pcl18_happen
#   ),
#   pcl19_happen = case_when(
#     pcl19_77 == -77 ~ "-77",
#     is.na(pcl19_happen) & is.na(pcl19_77) ~ "0",
#     TRUE ~ pcl19_happen
#   )
#   )
#
#
#

# cts un used code ####
# cts_complete <- cts_complete %>%
# add in -99 for midpoint ranges that cannot be computed due to -55 codes
#  mutate(
#   ct1_midpoint = if_else(is.na(ct1_midpoint), -99, ct1_midpoint),
#   ct2_midpoint = if_else(is.na(ct2_midpoint), -99, ct2_midpoint),
#   ct7_midpoint = if_else(is.na(ct7_midpoint), -99, ct7_midpoint),
#   ct8_midpoint = if_else(is.na(ct8_midpoint), -99, ct8_midpoint)
# ) %>%
# CTS PLOTS ######
```{r CTS-Severity-TC-distributions, class.source='fold', echo = FALSE}
# Psychological Aggression
p_cts_psy_agg_ind_sev <- complete %>%
  mutate(
    cts_psy_agg_ind_sev = case_when(
      cts_psy_agg_ind_sev == 0 ~ 'No Psychological Aggression',  #TODO: <Maria> Can I short this to No Psy. Aggre. etc...
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10),guide = guide_axis(n.dodge=3)) +
  labs(title = 'Individual (TC) Psychological Aggression Severity',
       y = "Number of Participants") +
  THEME


# Psychical Injury
p_cts_injury_ind_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Individual (TC) Physical Injury Severity',
       y = "Number of Participants") +
  THEME

# Assault
p_cts_assault_ind_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Individual (TC) Assault Severity',
       y = 'Number of Participants') +
  THEME

# Sexual Cohesion
p_cts_sex_ind_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Individual (TC) Sexual Cohesion Severity',
       y = "Number of Participants") +
  THEME

#TODO: <Min FIX the x axis overlap>
ggarrange(p_cts_psy_agg_ind_sev, p_cts_injury_ind_sev + rremove("ylab"),
          p_cts_assault_ind_sev, p_cts_sex_ind_sev + rremove("ylab")) %>%
  annotate_figure(top = text_grob( "Distribution for CTS Individual (TC) Serverity \n(x-axis and y-axis are in different scales)"))
```



```{r CTS-Severity-Partner-distributions, class.source='fold', echo = FALSE}
# Psychological Aggression
p_cts_psy_agg_part_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Partner Psychological Aggression Severity',
       y = "Number of Participants") +
  THEME +
  theme(axis.text.x = element_text(angle = -45))

# Physical Injury
p_cts_injury_part_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Partner Physical Injury Severity',
       y = "Number of Participants") +
  THEME


# Assault
p_cts_assault_part_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Partner Assault Severity',
       y = "Number of Participants") +
  THEME

# Sexual Cohesion
p_cts_sex_part_sev <- complete %>%
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
  bar_style +
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = 'Partner Sexual Cohesion Severity',
       y = "Number of Participants") +
  THEME

# TODO: Min fix x axis overlap
ggarrange(p_cts_psy_agg_part_sev, p_cts_injury_part_sev + rremove("ylab"),
          p_cts_assault_part_sev, p_cts_sex_part_sev + rremove("ylab")) %>%
  annotate_figure(top = text_grob( "Distribution for CTS Partner Serverity \n(x-axis and y-axis are in different scales)"))
```

##### Prevalence distributions

<MIN> TODO NA was removed when generating following Prevalence plot
MZ note: I think NA was removed when colculate the prevalence score.

```{r CTS-Prevalence-distributions, class.source='fold', echo = FALSE}
# Psychological Aggression
p_cts_psy_agg_pre <- complete %>%
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
    sum_prev = sum(prevalence, na.rm = T)
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
  labs(title = 'Psychological Aggression Prevalence') +
  scale_fill_manual(values = Two_color) +
  THEME

# Physical Injury
p_cts_inj_pre <- complete %>%
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
    sum_prev = sum(prevalence, na.rm = T)
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
  labs(title = 'Physical Injury Prevalence') +
  scale_fill_manual(values = Two_color) +
  THEME

# Assault
p_cts_ass_pre <- complete %>%
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
    sum_prev = sum(prevalence, na.rm = T)
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
  labs(title = 'Assault Prevalence') +
  scale_fill_manual(values = Two_color) +
  THEME

# Sexual Cohesion
p_cts_sex_pre <- complete %>%
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
    sum_prev = sum(prevalence, na.rm = T)
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
  labs(title = 'Sexual Cohesion Prevalence') +
  scale_fill_manual(values = Two_color) +
  THEME

ggarrange(p_cts_psy_agg_pre + rremove("legend"), p_cts_inj_pre + rremove("ylab"),
          p_cts_ass_pre + rremove("legend"), p_cts_sex_pre + rremove("ylab")) %>%
  annotate_figure(top = text_grob( "Distribution for CTS Prevalence \n(x-axis and y-axis are in different scales)"))
```



##### Mutuality distributions

```{r CTS-Mutuality-distributions, class.source='fold', echo = FALSE}

# Psychological Aggression
p_cts_psy_mut <- complete %>%
  mutate(
    cts_psy_agg_mutual = case_when(
      cts_psy_agg_mutual == 0 ~ 'Neither',
      cts_psy_agg_mutual == 1 ~ 'Male',
      cts_psy_agg_mutual == 2 ~ 'Female',
      cts_psy_agg_mutual == 3 ~ 'Both'
    ),
    cts_psy_agg_mutual = as.factor(cts_psy_agg_mutual),
    cts_psy_agg_mutual = relevel(cts_psy_agg_mutual, 'Male')
  ) %>%
  ggplot(
    aes(
      cts_psy_agg_mutual
    )
  ) +
  bar_style +
  labs(title = 'Mutual Psychological Aggression',
       y = "Number of Participants") +
  THEME



# Physical Injury
p_cts_ing_mut <-
  complete %>%
  mutate(
    cts_injury_mutual = case_when(
      cts_injury_mutual == 0 ~ 'Neither',
      cts_injury_mutual == 1 ~ 'Male',
      cts_injury_mutual == 2 ~ 'Female',
      cts_injury_mutual == 3 ~ 'Both'
    ),
    cts_injury_mutual = as.factor(cts_injury_mutual),
    cts_injury_mutual = relevel(cts_injury_mutual, 'Female')
  ) %>%
  ggplot(
    aes(
      cts_injury_mutual
    )
  ) +
  bar_style +
  labs(title = 'Mutual Physical Injury',
       y = "Number of Participants") +
  THEME


# Assault
p_cts_ass_mut <- complete %>%
  mutate(
    cts_assault_mutual = case_when(
      cts_assault_mutual == 0 ~ 'Neither',
      cts_assault_mutual == 1 ~ 'Male',
      cts_assault_mutual == 2 ~ 'Female',
      cts_assault_mutual == 3 ~ 'Both'
    ),
    cts_assault_mutual = as.factor(cts_assault_mutual),
    cts_assault_mutual = relevel(cts_assault_mutual, 'Male')
  ) %>%
  ggplot(
    aes(
      cts_assault_mutual
    )
  ) +
  bar_style +
  labs(title = 'Mutual Assault',
       y = "Number of Participants") +
  THEME

# Sexual Cohesion
p_cts_sex_mut <- complete %>%
  mutate(
    cts_sex_mutual = case_when(
      cts_sex_mutual == 0 ~ 'Neither',
      cts_sex_mutual == 1 ~ 'Male',
      cts_sex_mutual == 2 ~ 'Female',
      cts_sex_mutual == 3 ~ 'Both'
    ),
    cts_sex_mutual = as.factor(cts_sex_mutual),
    cts_sex_mutual = relevel(cts_sex_mutual, 'Male')
  ) %>%
  ggplot(
    aes(
      cts_sex_mutual
    )
  ) +
  bar_style +
  labs(title = 'Mutual Sexual Cohesion',
       y = "Number of Participants") +
  THEME

ggarrange(p_cts_psy_mut, p_cts_ing_mut + rremove("ylab"),
          p_cts_ass_mut, p_cts_sex_mut + rremove("ylab")) %>%
  annotate_figure(top = text_grob( "Distribution for CTS Mutual Subscale \n(x-axis and y-axis are in different scales)"))
```



