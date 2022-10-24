custody_df <-
  comb_long_custody %>%
  select(
    id,
    custody_name,
    custody
  ) %>%
  separate(
    col = custody_name,
    into = c('kid_name', 'rest', 'remove')
  ) %>%
  select(-remove) %>%
  unite(
    col = 'kid_name',
    c(kid_name, rest),
    sep = '_'
  ) %>%
  mutate(
    kid_name = str_remove(
      kid_name,
      '_custody'
    ),
    kid_name = str_replace(
      kid_name,
      'pc_bc',
      'already_born'
    ),
    kid_name = str_replace(
      kid_name,
      'bc',
      'new_birth'
    ),
    kid_name = str_replace(
      kid_name,
      'other_sc',
      'other_step_child'
    ),
    kid_name = str_replace(
      kid_name,
      'sm_sc',
      'step_mother_step_child'
    )
  )

living_df <-
  comb_long_custody %>%
  select(id,
         kid_living,
         kid_live_yesno) %>%
  separate(
    col = kid_living,
    into = c('kid_name', 'rest', 'remove'),
    sep = '_'
  ) %>%
  select(-remove) %>%
  unite(
    col = 'kid_name',
    c(kid_name, rest),
    sep = '_'
  ) %>%
  mutate(
    kid_name = str_replace(
      kid_name,
      'pc_bc',
      'already_born'
    ),
    kid_name = str_replace(
      kid_name,
      'bc',
      'new_birth'
    ),
    kid_name = str_replace(
      kid_name,
      'other_sc',
      'other_step_child'
    ),
    kid_name = str_replace(
      kid_name,
      'sm_sc',
      'step_mother_step_child'
    )
  )

combined <-
  full_join(
    custody_df,
    living_df
  ) %>%
  arrange(id) %>%
  rename(
    who_custody = custody,
    living_with_bi = kid_live_yesno
  )

latino_df <-
  comb_long_lat %>%
  select(
    id,
    are_latino,
    latino_yn,
    specific_latino
  ) %>%
  mutate(
    are_latino = str_replace(
      are_latino,
      'pc_bc',
      'already_born'
    ),
    are_latino = str_replace(
      are_latino,
      'bc',
      'new_birth'
    ),
    are_latino = str_replace(
      are_latino,
      'other_sc',
      'other_step_child'
    ),
    are_latino = str_replace(
      are_latino,
      'sm_sc',
      'step_mother_step_child'
    ),
    latino_group = case_when(
      specific_latino == 'Mexican, Mexican American, Chicano' ~ 'Mexican',
      specific_latino == 'Puerto Rican' ~ 'Puerto Rican',
      specific_latino == 'Cuban' ~ 'Cuban',
      specific_latino == 'Other (such as Argentinean, Colombian, Dominican, Nicaraguan, Salvadoran, Spaniard) Specify:' ~ 'Other',
      specific_latino == 'Don\'t know' ~ 'Don\'t know',
      specific_latino == 'Decline to Answer' ~ 'Decline',
      TRUE ~ NA_character_
    ),
    specific_latino_group = case_when(
      specific_latino == 'Chilean' ~ 'Chilean',
      specific_latino %in% c('Colombian', 'Columbian') ~ 'Colombian',
      specific_latino == 'Guatemalan' ~ 'Guatemalan',
      specific_latino == 'Honduran' ~ 'Honduran',
      specific_latino %in% c('Spaniard', 'Spanish') ~ 'Spanish',
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(id) %>%
    select(-specific_latino)

latino_multi_df <-
  latino_df %>%
    group_by(
      id,
      are_latino
      ) %>%
    mutate(
      lat_group_count = n()
    ) %>%
    ungroup() %>%
    mutate(
      multi_latino = case_when(
        lat_group_count > 1 ~ 'multi',
        lat_group_count == 1 ~ 'one',
        TRUE ~ NA_character_
      )
    ) %>%
    select(-lat_group_count) %>%
    group_by(id) %>%
    mutate(
      latino_group_multi = case_when(
        latino_group == 'Mexican' & multi_latino == 'multi' ~ 'Multi-ethnic',
        latino_group == 'Mexican' & multi_latino == 'one' ~ 'Mexican',
        latino_group == 'Puerto Rican' & multi_latino == 'multi' ~ 'Multi-ethnic',
        latino_group == 'Puerto Rican' & multi_latino == 'one' ~ 'Puerto Rican',
        latino_group == 'Cuban' & multi_latino == 'multi' ~ 'Multi-ethnic',
        latino_group == 'Cuban' & multi_latino == 'one' ~ 'Cuban',
        # latino_group == 'Other' & multi_latino == 'multi' ~ 'multi-ethnic',
        latino_group == 'Other' & multi_latino == 'one' ~ 'Other',
        latino_group == 'Don\'t know' ~ 'Don\'t know',
        latino_group == 'Decline' ~ 'Decline',
        TRUE ~ specific_latino_group
      )
    ) %>%
    ungroup() %>%
  rename(kid_name = are_latino)

combined <-
  combined %>%
  full_join(
    latino_multi_df
  ) %>%
  arrange(id)
# need to look back into where I lost the No responses for latino_yn

combined <-
  full_join(
    combined,
    comb_long_gender
    )

comb_long_dob <-
  comb_long_dob %>%
  mutate(
    kid_name = str_remove(
      kid_name,
      '_dob$'
    )
  )

combined <-
full_join(
  combined,
  comb_long_dob
  )

comb_long_numkid <-
  comb_long_numkid %>%
  select(-n) %>%
  janitor::adorn_totals(where = 'col') %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = -c(id,
              total),
    names_to = 'kid_name',
    values_to = 'kid_ind_count'
  ) %>%
  drop_na(kid_ind_count) %>%
  separate(
    col = kid_name,
    into = c('kid_name', 'rest', 'remove'),
    sep = '_'
  ) %>%
  select(-remove) %>%
  unite(
    col = 'kid_name',
    c(kid_name, rest),
    sep = '_'
  ) %>%
  mutate(
    kid_name = str_replace(
      kid_name,
      'pc_bc',
      'already_born'
    ),
    kid_name = str_replace(
      kid_name,
      'bc',
      'new_birth'
    ),
    kid_name = str_replace(
      kid_name,
      'other_sc',
      'other_step_child'
    ),
    kid_name = str_replace(
      kid_name,
      'sm_sc',
      'step_mother_step_child'
    ),
    kid_name = str_remove(
      kid_name,
      '_name$'
    )
  )

combined <-
  full_join(
  combined,
  comb_long_numkid
  ) %>%
  arrange(id)

comb_long_preg <-
  comb_long_preg %>%
  separate(
    col = preg_result_name,
    into = c('kid_name', 'remove'),
    sep = '_'
  ) %>%
  select(-remove) %>%
  mutate(
    kid_name = str_replace(
      kid_name,
      'bc',
      'new_birth'
    )
  ) %>%
  drop_na()

combined <-
  full_join(
  combined,
  comb_long_preg
) %>%
  arrange(id)


# issues with HR119
comb_long_age <-
  comb_long_age %>%
  select(-preg_result) %>%
  rename(preg_result = preg_result_value) %>%
  pivot_longer(
    cols = -c(id,
              preg_result),
    names_to = 'kid_name',
    values_to = 'tc_age'
  ) %>%
  separate(
    col = kid_name,
    into = c('kid_name', 'remove', 'remove1', 'remove2')
  ) %>%
  select(-remove:-remove2) %>%
  mutate(
    kid_name = str_replace(
      kid_name,
      'bc',
      'new_birth'
    )
  ) %>%
  drop_na() %>%
    group_by(id) %>%
    distinct(kid_name,
             .keep_all = TRUE) %>%
    ungroup()


combined <-
  full_join(
  combined,
  comb_long_age
) %>%
  arrange(id)


combined <-
  full_join(
  combined,
  name_dob
) %>%
  arrange(id)


# start here, may want to remove the latino group here and only use races here
comb_long_race %>%
  separate(
    col = race_name,
    into = c('kid_name', 'rest', 'race_name', 'more', 'more_more')
  ) %>%
  count(race_name, more, more_more)

# combined %>%
  # rename(
    # total_kid_count = total
  # )

# write.csv(
  # combined,
  # 'interview_data_cleaned.csv'
  # )
