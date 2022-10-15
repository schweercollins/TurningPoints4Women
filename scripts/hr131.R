hr131 <- remove_df %>%
  filter(id == 'HR131')

hr131 %>%
  select(
    id,
    matches('preg_result$'),
    curr_preg
  ) %>%
  pivot_longer(
    cols = matches('preg_result$'),
    names_to = 'preg_result_name',
    values_to = 'preg_result'
  ) %>%
  mutate(
    preg_result = case_when(
      preg_result == 1 ~ 'still_preg',
      preg_result == 2 ~ 'miscarriage',
      preg_result == 3 ~ 'abortion',
      preg_result == 4 ~ 'birth',
      preg_result == 5 ~ 'multiple_birth',
      preg_result == 6 ~ 'stillbirth',
      preg_result == 7 ~ 'false_pos',
      preg_result == 8 ~ 'decline',
      TRUE ~ NA_character_
    ),
    curr_preg = case_when(
      curr_preg == 1 ~ 'yes',
      curr_preg == 2 ~ 'no',
      curr_preg == 3 ~ 'decline',
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(id) %>%
  drop_na(
    preg_result,
    curr_preg
  ) %>% # View() # Use this if you want to see the pregnancy results
  group_by(id) %>%
  count(preg_result_name, preg_result, curr_preg) %>%
  ungroup() %>%
  arrange(id) %>%
  mutate(
    new_birth = case_when( #this variable is because I'm not sure if the multiple birth countes as 2 for your codebook
      preg_result == 'birth' ~ 1,
      preg_result == 'multiple_birth' ~ 2,
      !preg_result %in% c('birth', 'multiple_birth') ~ 0,
      TRUE ~ 0
    ),
    new_birth_bi = case_when(
      preg_result %in% c('birth', 'multiple_birth') ~ 1,
      !preg_result %in% c('birth', 'multiple_birth') ~ 0,
      TRUE ~ 0
    )
  ) %>%
  group_by(id) %>%
  summarize(
    new_birth_counts = sum(new_birth),
    new_birth_counts_bi = sum(new_birth_bi)
  ) %>%
  ungroup()



hr131 %>%
  select(
    id,
    matches('name$'),
    matches('^pc.*birth_04'), # kiddos that are already born don't have names, used birth_04 variables as proxies
    matches('^pc.*custody'),
    matches('^pc.*lost_custody'),
    matches('sm_.*welfare_contact')
  ) %>% # View() # There are no names for the multiple births. I'm not sure if this is because TC is still pregnant with multiple children
  mutate(
    across(
      .cols = c(
        matches('^bc\\d'),
        matches('^other_sc\\d'),
        matches('^other_kid\\d')
      ),
      .fns = ~case_when(
        is.na(.x) ~ 0,
        TRUE ~ 1
      ),
      .names = '{.col}_num'
    ),
    across(
      .cols = c(matches('sm_sc\\d_welfare_contact')),
      .fns = ~case_when(
        .x == 4 ~ 0,
        TRUE ~ 1
      ),
      .names = '{.col}_num'
    ),
    across(
      .cols = c(matches('pc_bc\\d_birth_04')),
      .fns = ~case_when(
        .x == 3 ~ 0,
        TRUE ~ 1
      ),
      .names = '{.col}_num'
    )
  ) %>%
  select(
    id,
    matches('num$')
  ) %>% # View() # This will show how kiddos are counted
  pivot_longer(
    cols = -id,
    names_to = 'type_of_kiddo',
    values_to = 'yes_kiddo'
  ) %>%
  filter(yes_kiddo == 1) %>%
  group_by(id, type_of_kiddo) %>%
  count(yes_kiddo) %>%
  ungroup() %>%
  pivot_wider(
    names_from = type_of_kiddo,
    values_from = yes_kiddo
  ) %>%
  select(-n) %>%
  janitor::adorn_totals(where = 'col') %>%
  relocate(Total, .after = id) %>%
  gt::gt() #this doesn't count the multiple births because they don't have names.

hr131 %>%
  select(
    id,
    matches('\\d_custody$')
  ) %>% View()
  pivot_longer(
    cols = matches('\\d_custody$'),
    names_to = 'custody_name',
    values_to = 'custody'
  ) %>%
  mutate(
    custody = case_when(
      custody == 1 ~ 'tc',
      custody == 2 ~ 'bio dad',
      custody == 3 ~ 'tc bio dad - living',
      custody == 4 ~ 'tc and bio dad - shared',
      custody == 5 ~ 'tc and partner',
      custody == 6 ~ 'grandparent(s)',
      custody == 7 ~ 'other relative',
      custody == 8 ~ 'adopted',
      custody == 9 ~ 'state',
      custody == 10 ~ 'don\'t know',
      custody == 11 ~ 'decline',
      custody == 12 ~ NA_character_,
      TRUE ~ 'no_info_input'
    )
  ) %>%
  arrange(id)
