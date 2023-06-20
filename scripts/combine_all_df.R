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
multi_race <-
  comb_long_race %>%
  separate(
    col = race_name,
    into = c('kid_name', 'rest', 'race_name', 'more', 'more_more')
  ) %>%
  unite(
    col = 'kid_name',
    c('kid_name', 'rest', 'race_name', 'more'),
    sep = '_'
  ) %>%
  filter(
    !str_detect(
      kid_name,
      'latino$'
      ) &
      !str_detect(
        kid_name,
        'latino_other$'
      )
    ) %>%
  mutate(
    kid_name = str_remove(
      kid_name,
      '_NA$'
    ),
    kid_name = str_remove(
      kid_name,
      '_amer_ind$'
    ),
    kid_name = str_remove(
      kid_name,
      '_black$'
    ),
    kid_name = str_remove(
      kid_name,
      '_filipino$'
    ),
    kid_name = str_remove(
      kid_name,
      '_other_asian$'
    ),
    kid_name = str_remove(
      kid_name,
      '_other_pacific$'
    ),
    kid_name = str_remove(
      kid_name,
      '_race_other$'
    ),
    kid_name = str_remove(
      kid_name,
      '_samoan$'
    ),
    kid_name = str_remove(
      kid_name,
      '_white$'
    ),
    kid_name = str_remove(
      kid_name,
      '_race_pacific$'
    ),
    kid_name = str_remove(
      kid_name,
      '_race_tribe$'
    ),
    kid_name = str_remove(
      kid_name,
      '_native_hawaii$'
    ),
    kid_name = str_remove(
      kid_name,
      '_amer'
    ),
    kid_name = str_remove(
      kid_name,
      '_race$'
    ),
    kid_name = str_remove(
      kid_name,
      '_other$'
    )
  ) %>%
  select(-more_more) %>%
  mutate(
    primary_race = case_when(
      race == 'American Indian or Alaska Native, specify enrolled principal tribe:' ~ 'american_indian',
      race == 'Black or African American' ~ 'black',
      race == 'Blackfoot' ~ 'specific_tribe',
      race == 'Bulgarian' ~ 'specific_tribe',
      race == 'Cherokee' ~ 'specific_tribe',
      race %in% c('Chicana', 'Chicano') ~ 'other_text',
      race %in% c('Coquille', 'Coquille Indian') ~ 'specific_tribe',
      race == 'Creek Indian' ~ 'specific_tribe',
      race == 'Filipino' ~ 'filipino',
      race == 'Grand Ronde' ~ 'specific_tribe',
      race %in% c('Half Mexican Half Colombian', 'Half mexican half columbain', 'Half Mexican Half Columbian') ~ 'other_text',
      race == 'he is a quarter black' ~ 'black',
      race == 'Hispanic' ~ 'other_text',
      race == 'Indigenous: Pima, Pit River, Apache, Klamath, Modoc' ~ 'specific_tribe',
      race == 'Indigenous: Pima, Pit River, Apache, Klamath, Modoc, Masagwa' ~ 'specific_tribe',
      race == 'Indigenous: Pima, Pit River, Apache, Klamath, Modoc, Ojibwe' ~ 'specific_tribe',
      race == 'Irish' ~ 'other_text',
      race == 'Irish, White, German, Dutch' ~ 'other_text',
      race == 'Italian' ~ 'other_text',
      race == 'Klatsop and Modock' ~ 'specific_tribe',
      race == 'Kruer?' ~ 'specific_tribe',
      race == 'Kwe? same as previous' ~ 'specific_tribe',
      race == 'Lakoda' ~ 'specific_tribe',
      race %in% c('Latino', 'Latino/Hispanic') ~ 'other_text',
      race == 'Lower Siox' ~ 'specific_tribe',
      race == 'Mexican' ~ 'other_text',
      race == 'Mexican, Native' ~ 'other_text',
      race %in% c('n/a', 'N/A', 'None') ~ 'none',
      race == 'Not enrolled but a few different tribes, grandmother is enrolled.' ~ 'other_text',
      race == 'Other (for ex: Fijian, Tonga), specify:' ~ 'other_asian_text',
      race == 'Other Asian, (check all that apply)' ~ 'other_asian_header',
      race == 'Puerto Rican' ~ 'other_pacific_island_text',
      race == 'Qwe? same as previous' ~ 'specific_tribe',
      race == 'Samoan' ~ 'samoan',
      race %in% c('Selet', 'Selets') ~ 'specific_tribe',
      race == 'Selet and FlatHead' ~ 'specific_tribe',
      race == 'Siletz' ~ 'specific_tribe',
      race == 'Soboba' ~ 'specific_tribe',
      race == 'Some other race, specify:' ~ 'other_race_text',
      race == 'White' ~ 'white',
      race == 'Yakama' ~ 'specific_tribe',
      race == 'Yes' ~ 'none',
      race %in% c('I don\'t know', 'Unknown', 'Unknown (Tribe out of New Mexico possibly)') ~ 'dont_know'
      ),
    specific_race = case_when(
      race == 'American Indian or Alaska Native, specify enrolled principal tribe:' ~ NA_character_,
      race == 'Black or African American' ~ NA_character_,
      race == 'Filipino' ~ NA_character_,
      race == 'he is a quarter black' ~ NA_character_,
      race == 'Samoan' ~ NA_character_,
      race %in% c('n/a', 'N/A', 'None') ~ 'none',
      race == 'White' ~ NA_character_,
      race == 'Yes' ~ 'none',
      race %in% c('I don\'t know', 'Unknown', 'Unknown (Tribe out of New Mexico possibly)') ~ 'dont_know',
      TRUE ~ race
    )
  ) %>%
  arrange(
    id,
    kid_name
    )

multi_race <-
  multi_race %>%
  mutate(
    race_count = case_when(
      primary_race %in% c('american_indian',
                          'black',
                          'filipino',
                          'other_asian_text',
                          'other_pacific_island_text',
                          'other_text',
                          'samoan',
                          'white') ~ 1,
      primary_race %in% c('specific_tribe',
                          'dont_know',
                          'other_asian_header',
                          'other_race_text',
                          'none') ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(
    id,
    kid_name
  ) %>%
  mutate(
    race_group_count = sum(race_count)
  ) %>%
  ungroup() %>%
  mutate(
    multi_race = case_when(
      race_group_count > 1 ~ 'multi',
      race_group_count == 1 ~ 'one',
      TRUE ~ NA_character_
    )
  ) %>%
  select(-race_group_count) %>%
  mutate(
     race_group_multi = case_when(
      primary_race == 'american_indian' & multi_race == 'multi' ~ 'Multi-racial',
      primary_race == 'american_indian' & multi_race == 'one' ~ 'Mexican',
      primary_race == 'black' & multi_race == 'multi' ~ 'Multi-racial',
      primary_race == 'black' & multi_race == 'one' ~ 'black',
      primary_race == 'filipino' & multi_race == 'multi' ~ 'Multi-racial',
      primary_race == 'filipino' & multi_race == 'one' ~ 'filipino',
      primary_race == 'white' & multi_race == 'multi' ~ 'Multi-racial',
      primary_race == 'white' & multi_race == 'one' ~ 'white',
      primary_race == 'samoan' & multi_race == 'multi' ~ 'Multi-racial',
      primary_race == 'samoan' & multi_race == 'one' ~ 'samoan',
      primary_race == 'other_asian_header' ~ 'other_asian_header',
      primary_race == 'other_asian_text' ~ 'other_asian_text',
      primary_race == 'other_pacific_island_text' ~ 'other_pacific_island_text',
      primary_race == 'other_race_text' ~ 'other_race_text',
      primary_race == 'other_text' ~ 'other_text',
      primary_race == 'none' ~ 'none',
      primary_race == 'dont_know' ~ 'dont_know',
      TRUE ~ specific_race
    )
  ) %>%
  select(
    -race_count,
    -multi_race
  )

final <-
  full_join(
  combined,
  multi_race
)

final <-
  final %>%
  rename(total_kid_count_per_tc = total)


date_diff_df_sub <-
  date_diff_df %>%
  rename(
    tc_dob = dob
    ) %>%
  select(
    id,
    tc_dob,
    first_assessment:month_diff
  )

final <-
  final %>%
  full_join(
    date_diff_df_sub
  )

final <-
  final %>%
  mutate(
    kid_code = case_when(
      kid_name == "already_born1" ~ "1ab",
      kid_name == "already_born2" ~ "2ab",
      kid_name == "already_born3" ~ "3ab",
      kid_name == "already_born4" ~ "4ab",
      kid_name == "already_born5" ~ "5ab",
      kid_name == "new_birth1" ~ "1nb",
      kid_name == "new_birth2" ~ "2nb",
      kid_name == "new_birth3" ~ "3nb",
      kid_name == "new_birth4" ~ "4nb",
      kid_name == "new_birth5" ~ "5nb",
      kid_name == "new_birth6" ~ "6nb",
      kid_name == "new_birth7" ~ "7nb",
      kid_name == "other_kid1" ~ "1ok",
      kid_name == "other_kid2" ~ "2ok",
      kid_name == "other_step_child1" ~ "1osc",
      kid_name == "other_step_child2" ~ "2osc",
      kid_name == "other_step_child3" ~ "3osc",
      kid_name == "other_step_child4" ~ "4osc",
      kid_name == "step_mother_step_child1" ~ "1smsc",
      kid_name == "step_mother_step_child2" ~ "2smsc",
      kid_name == "step_mother_step_child3" ~ "3smsc"
    ),
    full_id = paste0(id, "_", kid_code)
  ) %>%
  relocate(
    full_id,
    .after = id
  )

birth_data <-
  final %>%
  filter(
    str_detect(
      kid_name,
      'new_birth'
    )
  ) %>%
  arrange(id)

# if you only want the pregnancy results that were births
birth_data %>%
  filter(preg_result == 'birth')

already_born_data <-
  final %>%
  filter(
    !str_detect(
      kid_name,
      'new_birth'
    )
  ) %>%
  arrange(id) %>%
  select(
    -curr_preg,
    -preg_result
  )


# write.csv(
#   final,
#   'calc_interview_data_allkids.csv'
# )
#
# write.csv(
#   birth_data,
#   'calc_interview_data_newbirths.csv'
# )
#
# write.csv(
#   already_born_data,
#   'calc_interview_data_alreadyborn.csv'
# )


final_ex <-
  final %>%
  distinct()
