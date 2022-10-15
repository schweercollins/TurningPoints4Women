library(tidyverse)

interview <-
  read_csv('C:/Users/cpppe/Desktop/github_projects/tpwdata_fork/jp_files/interview.csv') %>%
  # read_csv('D:/delete_when_done/interview.csv') %>%
  mutate(id = assessment_screen_2) %>%
  select(-matches('^pct')) %>%
  mutate(
    id = case_when(
      id == 'av' ~ 'P901',
      TRUE ~ id
      )
    ) %>%
  filter(id != 'AG400',
         id != 'K000',
         id != 'F000',
         id != 'P100',
         id != '65973269')

p831_inter <- interview %>%
  filter(id == 'P831') %>%
  filter(assessment_screen == '11/30/2021')

inter_data <-
  interview %>%
  filter(id != 'P831')

inter_data <-
  full_join(inter_data, p831_inter)

maria_file <-
  rio::import('C:/Users/cpppe/Desktop/github_projects/TurningPoints4Women/do_not_push/GLOINFO_DOB.SAV') %>%
  # rio::import('D:/delete_when_done/GLOINFO_DOB.SAV') %>%
  janitor::clean_names()

child_history <-
readxl::read_excel('C:/Users/cpppe/Desktop/github_projects/TurningPoints4Women/do_not_push/full_pregnancy_child_history.xlsx') %>%
  # readxl::read_excel('D:/delete_when_done/full_pregnancy_child_history.xlsx') %>%
  janitor::clean_names()

ex <- inter_data[, colSums(is.na(inter_data)) != nrow(inter_data)]

ex <- ex[ - as.numeric(which(apply(ex, 2, var) == 0))]

ex <-
  ex %>%
  rename(
    #bc = biological child
    times_preg = p00a, #how many times pregnant since last talk
    curr_preg = p00aa, #currently pregnant
    bc1_tc_age_preg = p01a, #tc age
    bc1_preg_attempt = p01c, #trying to get pregnant
    bc1_preg_result = p01d, #what happened to the pregnancy
    # tri_still_preg = p01g, #trimester_current
    bc1_tri_mis_abort = p01g2, #trimester_loss
    bc1_name = p01b, #child name
    bc1_dob = p01b2, #child DOB
    bc1_birth_04 = p01db1, #born before today's date 2004
    bc1_gender_id = p01b3, #child gender identity
    # gender_self_id = p01b3_8_text, # gender identity (self identity)
    bc1_white = p01b4_1, # race - white
    bc1_black = p01b4_2, #race - black
    bc1_amer_ind = p01b4_3, #race - american indian
    # asian_indian = p01b4_4, #race - asian indian
    # other_asian_all = p01b4_5, #race - other asian
    # chinese = p01b4_6, #race - chinese
    # filipino = p01b4_7, #race - filipino
    # japanese = p01b4_8, #race -  japanese
    # korean = p01b4_9, #race -  korean
    # vietnamese = p01b4_10, #race - vietnamese
    # other_asian = p01b4_11, #race - other
    # native_hawaii_all = p01b4_12, #race - native hawaiian/other pacific islander
    # native_hawaii = p01b4_13, #race - native hawaiian
    # guamanian = p01b4_14, #race - guamanian
    bc1_samoan = p01b4_15, #race - samoan
    # other_pacific = p01b4_16, #race - other - entry
    # race_dontknow = p01b4_17, #race - don't know
    bc1_race_other = p01b4_18, #race - other race - entry
    # race_decline = p01b4_77, #race - decline to answer
    bc1_race_tribe_text = p01b4_3_text, #race - tribe text
    # race_pacific_text = p01b4_16_text, #race - other race
    bc1_race_other_text = p01b4_18_text, #race - other race text,
    bc1_latino = p01b5, #latino
    bc1_mexican = p01b55_1, #mexican
    # puerto_rican = p01b55_2, #puerto rican
    # cuban = p01b55_3,  #cuban
    bc1_latino_other = p01b55_4, #other
    # latino_dontknow= p01b55_5, #don't know
    # latino_decline = p01b55_77, #declined
    bc1_latino_other_text = p01b55_4_text, #other text
    bc1_custody = p01b6, #who currently has custody
    # custody_other_rel = p01b6_7_text, #other relative specificed text
    # num_kid_twin = p01mb0, # how many children
    # name_twin = p01mb1, #child name
    # dob_twin = p01mb2, #child DOB
    # bc1_birth_04_twin = p01db2, # child born before 2004
    # bc1_gender_id_twin = p01mb3, #child gender identity
    # gender_self_id_twin = p01mb3_8_text, #self id gender
    # white_twin = p01mb4_1, #race - white
    # black_twin = p01mb4_2, #race - black
    # amer_ind_twin = p01mb4_3, #race - american indian
    # asian_indian_twin = p01mb4_4, #race - asian indian
    # other_asian_all_twin = p01mb4_5, #race - other asian
    # chinese_twin = p01mb4_6, #race - chinese
    # filipino_twin = p01mb4_7, #race - filipino
    # japanese_twin = p01mb4_8, #race -  japanese
    # korean_twin = p01mb4_9, #race -  korean
    # vietnamese_twin = p01mb4_10, #race - vietnamese
    # other_asian_twin = p01mb4_11, #race - other
    # native_hawaii_all_twin = p01mb4_12, #race - native hawaiian/other pacific islander
    # native_hawaii_twin = p01mb4_13, #race - native hawaiian
    # samoan_twin = p01mb4_14, #race - guamanian
    # guamanian_twin = p01mb4_15, #race - samoan
    # other_pacific_twin = p01mb4_16, #race - other - entry
    # race_dontknow_twin = p01mb4_17, #race - don't know
    # race_other_twin = p01mb4_18, #race - other race - entry
    # race_decline_twin = p01mb4_77, #race - decline to answer
    # race_tribe_text_twin = p01mb4_3_text, #race - tribe text
    # race_pacific_text_twin = p01mb4_16_text, #race - other race
    # race_other_text_twin = p01mb4_18_text, #race - other race text
    # bc1_latino_twin = p01mb5, #child latino
    # mexican_twin = p01mb55_1, #mexican
    # puerto_rican_twin = p01mb55_2, #puerto rican
    # cuban_twin = p01mb55_3, #cuban
    # latino_other_twin = p01mb55_4, #other
    # latino_dontknow_twin = p01mb55_5, #don't know
    # latino_decline_twin = p01mb55_77, #declined
    # latino_other_text_twin = p01mb55_4_text, #other text
    # bc1_custody_twin = p01mb6, #who currently has custody
    # custody_other_rel_twin = p01mb6_7_text, #other relative specificed text
    # num_kid_add_birth1 = p01mb0, # how many children
    # name_add_birth1 = p01mb1, #child name
    # dob_add_birth1 = p01mb2, #child DOB
    # birth_04_add_birth1 = p01db2, # child born before 2004
    # gender_id_add_birth1 = p01mb3, #child gender identity
    # gender_self_id_add_birth1 = p01mb3_8_text, #self id gender
    # white_add_birth1 = p01mb4_1,#race - white
    # black_add_birth1 = p01mb4_2,#race - black
    # amer_ind_add_birth1 = p01mb4_3,#race - american indian
    # asian_indian_add_birth1 = p01mb4_4,#race - asian indian
    # other_asian_all_add_birth1 = p01mb4_5,#race - other asian
    # chinese_add_birth1 = p01mb4_6,#race - chinese
    # filipino_add_birth1 = p01mb4_7,#race - filipino
    # japanese_add_birth1 = p01mb4_8,#race - japanese
    # korean_add_birth1 = p01mb4_9,#race - korean
    # vietnamese_add_birth1 = p01mb4_10,#race - vietnamese
    # other_asian_add_birth1 = p01mb4_11,#race - other
    # native_hawaii_all_add_birth1 = p01mb4_12,#race - native hawaiian/other pacific islander
    # native_hawaii_add_birth1 = p01mb4_13,#race - native hawaiian
    # samoan_add_birth1 = p01mb4_14,#race - guamanian
    # guamanian_add_birth1 = p01mb4_15,#race - samoan
    # other_pacific_add_birth1 = p01mb4_16,#race - other - entry
    # race_dontknow_add_birth1 = p01mb4_17,#race - don't know
    # race_other_add_birth1 = p01mb4_18,#race - other race - entry
    # race_decline_add_birth1 = p01mb4_77,#race - decline to answer
    # race_tribe_text_add_birth1 = p01mb4_3_text, #race - tribe text
    # race_pacific_text_add_birth1 = p01mb4_16_text, #race - other race
    # race_other_text_add_birth1 = p01mb4_18_text,#race - other race text
    # latino_add_birth1 = p01mb5, #child latino
    # mexican_add_birth1 = p01mb55_1,#mexican
    # puerto_rican_add_birth1 = p01mb55_2,#puerto rican
    # cuban_add_birth1 = p01mb55_3,#cuban
    # latino_other_add_birth1 = p01mb55_4,#other
    # latino_dontknow_add_birth1 = p01mb55_5,#don't know
    # latino_decline_add_birth1 = p01mb55_77, #declined
    # latino_other_text_add_birth1 = p01mb55_4_text, #other text
    # custody_add_birth1 = p01mb6, #who currently has custody
    # custody_other_rel_add_birth1 = p01mb6_7_text, #other relative specificed text
    # name_add_birth2 = p01mb1b,# child's name
    # dob_add_birth2 = p01mb2b, #child DOB
    # bc1_birth_04_add_birth2 = p01db3, # child born before 2004
    # bc1_gender_id_add_birth2 = p01mb3b, #child gender identity
    # gender_self_id_add_birth2 = p01mb3b_8_text, #self id gender
    # white_add_birth2 = p01mb4b_1, #race - white
    # black_add_birth2 = p01mb4b_2, #race - black
    # amer_ind_add_birth2 = p01mb4b_3, #race - american indian
    # asian_indian_add_birth2 = p01mb4b_4, #race - asian indian
    # other_asian_all_add_birth2 = p01mb4b_5, #race - other asian
    # chinese_add_birth2 = p01mb4b_6, #race - chinese
    # filipino_add_birth2 = p01mb4b_7, #race - filipino
    # japanese_add_birth2 = p01mb4b_8, #race - japanese
    # korean_add_birth2 = p01mb4b_9, #race -  korean
    # vietnamese_add_birth2 = p01mb4b_10, #race - vietnamese
    # other_asian_add_birth2 = p01mb4b_11, #race - other
    # native_hawaii_all_add_birth2 = p01mb4b_12, #race - native hawaiian/other pacific islander
    # native_hawaii_add_birth2 = p01mb4b_13, #race - native hawaiian
    # samoan_add_birth2 = p01mb4b_14, #race - guamanian
    # guamanian_add_birth2 = p01mb4b_15, #race - somoan
    # other_pacific_add_birth2 = p01mb4b_16, #race - other - entry
    # race_dontknow_add_birth2 = p01mb4b_17, #race - don't know
    # race_other_add_birth2 = p01mb4b_18, #race - other race - entry
    # race_decline_add_birth2 = p01mb4b_77, #race - decline to answer
    # race_tribe_text_add_birth2 = p01mb4b_3_text, #race - tribe text
    # race_pacific_text_add_birth2 = p01mb4b_16_text,#race - other race
    # race_other_text_add_birth2 = p01mb4b_18_text, #race - other race text
    # bc1_latino_add_birth2 = p01mb5b, #child latino
    # mexican_add_birth2 = p01mb5bb_1, # mexican
    # puerto_rican_add_birth2 = p01mb5bb_2, # puerto rican
    # cuban_add_birth2 = p01mb5bb_3, # cuban
    # latino_other_add_birth2 = p01mb5bb_4, # other
    # latino_dontknow_add_birth2 = p01mb5bb_5, # don't know
    # latino_decline_add_birth2 = p01mb5bb_77,#declined
    # latino_other_text_add_birth2 = p01mb5bb_4_text, #other text
    # bc1_custody_add_birth2 = p01mb6b, #who currently has custody
    # custody_other_rel_add_birth2 = p01mb6b_7_text, #other relative specificed text
    # name_add_birth3 = p01mb1c, #child name
    # dob_add_birth3 = p01mb2c, #child DOB
    # bc1_birth_04_add_birth3 = p01db4, # child born before 2004
    # bc1_gender_id_add_birth3 = p01mb3c, #child gender identity
    # gender_self_id_add_birth3 = p01mb3c_8_text, #self id gender
    # white_add_birth3 = p01mb4c_1, #race - white
    # black_add_birth3 = p01mb4c_2, #race - black
    # amer_ind_add_birth3 = p01mb4c_3, #race - american indian
    # asian_indian_add_birth3 = p01mb4c_4, #race - asian indian
    # other_asian_all_add_birth3 = p01mb4c_5, #race - other asian
    # chinese_add_birth3 = p01mb4c_6, #race - chinese
    # filipino_add_birth3 = p01mb4c_7, #race - filipino
    # japanese_add_birth3 = p01mb4c_8, #race - japanese
    # korean_add_birth3 = p01mb4c_9, #race - korean
    # vietnamese_add_birth3 = p01mb4c_10, #race - vietnamese
    # other_asian_add_birth3 = p01mb4c_11, #race - other
    # native_hawaii_all_add_birth3 = p01mb4c_12, #race - native hawaiian/other pacific islander
    # native_hawaii_add_birth3 = p01mb4c_13, #race - native hawaiian
    # samoan_add_birth3 = p01mb4c_14, #race - guamanian
    # guamanian_add_birth3 = p01mb4c_15, #race - somoan
    # other_pacific_add_birth3 = p01mb4c_16, #race - other - entry
    # race_dontknow_add_birth3 = p01mb4c_17, #race - don't know
    # race_other_add_birth3 = p01mb4c_18, #race - other race - entry
    # race_decline_add_birth3 = p01mb4c_77, #race - decline to answer
    # race_tribe_text_add_birth3 = p01mb4c_3_text,# race - tribe text
    # race_pacific_text_add_birth3 = p01mb4c_16_text, #race - other race
    # race_other_text_add_birth3 = p01mb4c_18_text,#race - other race text
    # bc1_latino_add_birth3 = p01mb5c, #child latino
    # mexican_add_birth3 = p01mb5cc_1,# mexican
    # puerto_rican_add_birth3 = p01mb5cc_2,# puerto ricna
    # cuban_add_birth3 = p01mb5cc_3,# cuban
    # latino_other_add_birth3 = p01mb5cc_4,# other
    # latino_dontknow_add_birth3 = p01mb5cc_5,# don't know
    # latino_decline_add_birth3 = p01mb5cc_77, #declined
    # latino_other_text_add_birth3 = p01mb5cc_4_text, #other text
    # bc1_custody_add_birth3 = p01mb6c, #who currently has custody
    # custody_other_rel_add_birth3 = p01mb6c_7_text,# other relative specified text
    bc1_drug_preg = p01l, #drugs during pregnancy
    bc1_drug_tri1 = p01n_1, #first trimester drugs
    bc1_drug_tri2= p01n_2, #second trimester drugs
    bc1_drug_tri3 = p01n_3,#third trimester drugs
    # drug_tri_decline = p01n_77, #declined trimester drugs
    bc1_days_drug_preg = p01m, #days using drugs during pregnancy
    bc1_alc_preg = p01q, #alcohol during pregnancy
    bc1_alc_tri1 = p01s_1, #first trimester alcohol
    # bc1_alc_tri2 = p01s_2,#second trimester alcohol
    # bc1_alc_tri3 = p01s_3,#third trimester alcohol
    # alc_tri_decline = p01s_77, #declined trimester alcohol
    bc1_days_alc_preg = p01r, #days alcohol during pregnancy
    bc1_tob_preg = p01v, #did you use use nicotine during pregnancy
    bc1_tob_tri1 = p01x_1, #first trimester nicotine
    bc1_tob_tri2 = p01x_2,#second trimester nicotine
    bc1_tob_tri3 = p01x_3, #third trimester nicotine
    # tob_tri_decline = p01x_77, #declined trimester nicotine
    bc1_days_tob_preg = p01w,

    bc2_tc_age_preg = p02a,
    bc2_preg_attempt = p02c,
    bc2_preg_result = p02d,
    bc2_tri_still_preg = p02g,
    bc2_tri_mis_abort = p02g2,
    bc2_name = p02b1,
    bc2_dob = p02b2,
    bc2_birth_04 = p02db1,
    bc2_gender_id = p02b3,
    bc2_white= p02b4_1,
    bc2_black = p02b4_2,
    bc2_amer_ind= p02b4_3,
    bc2_race_other = p02b4_18,
    bc2_race_tribe_text = p02b4_3_text,
    bc2_race_other_text = p02b4_18_text,
    bc2_latino = p02b5,
    bc2_mexican = p02b55_1,
    bc2_latino_other = p02b55_4,
    bc2_latino_other_text = p02b55_4_text,
    bc2_custody = p02b6,
    bc2_drug_preg = p02l,
    bc2_drug_tri1 = p02n_1,
    bc2_drug_tri2 = p02n_2,
    bc2_drug_tri3 = p02n_3,
    bc2_days_drug_preg = p02m,
    bc2_alc_preg = p02q,
    bc2_alc_tri1 = p02s_1,
    bc2_days_alc_preg = p02r,
    bc2_tob_preg = p02v,
    bc2_tob_tri1 = p02x_1,
    bc2_tob_tri2 = p02x_2,
    bc2_tob_tri3 = p02x_3,
    bc2_days_tob_preg = p02w,
    bc3_tc_age_preg = p03a,
    bc3_preg_attempt = p03c,
    bc3_preg_result = p03d,
    br3_still_preg = p03g,
    bc3_tri_mis_abort = p03g2,
    bc3_name = p03b1,
    bc3_dob = p03b2,
    bc3_birth_04= p03db1,
    bc3_gender_id = p03b3,
    bc3_white = p03b4_1,
    bc3_black = p03b4_2,
    bc3_amer_ind = p03b4_3,
    bc3_filipino = p03b4_7,
    bc3_race_other = p03b4_18,
    bc3_race_tribe_text = p03b4_3_text,
    bc3_race_other_text = p03b4_18_text,
    bc3_latino = p03b5,
    bc3_mexican = p03b55_1,
    bc3_latino_other = p03b55_4,
    bc3_latino_other_text = p03b55_4_text,
    bc3_custody = p03b6,
    bc3_num_kid_twin = p03mb0,
    bc3_name_twin = p03mb1,
    bc3_dob_twin = p03mb2,
    bc3_birth_04_twin = p03db2,
    bc3_gender_id_twin = p03mb3,
    bc3_white_twin = p03mb4a_1,
    bc3_latino_twin = p03mb5,
    bc3_custody_twin = p03mb6,
    bc3_name_add_birth2 = p03mb1b,
    bc3_dob_add_birth2 = p03mb2b,
    bc3_birth_04_add_birth2 = p03db3,
    bc3_gender_id_add_birth2 = p03mb3b,
    bc3_white_add_birth2 = p03mb4b_1,
    bc3_latino_add_birth2 = p03mb5b,
    bc3_custody_add_birth2 = p03mb6b,
    bc3_drug_preg= p03l,
    bc3_drug_tri1 = p03n_1,
    bc3_drug_tri2 = p03n_2,
    bc3_drug_tri3 = p03n_3,
    bc3_days_drug_preg = p03m,
    bc3_alc_preg = p03q,
    bc3_alc_tri1 = p03s_1,
    bc3_alc_tri2 = p03s_2,
    bc3_alc_tri3 = p03s_3,
    bc3_days_alc_preg = p03r,
    bc3_tob_preg = p03v,
    bc3_tob_tri1 = p03x_1,
    bc3_tob_tri2 = p03x_2,
    bc3_tob_tri3 = p03x_3,
    bc3_days_tob_preg = p03w,

    bc4_tc_age_preg = p04a,
    bc4_preg_attempt = p04c,
    bc4_preg_result = p04d,
    bc4_tri_still_preg = p04g,
    bc4_tri_mis_abort = p04g2,
    bc4_name = p04b1,
    bc4_dob = p04b2,
    bc4_birth_04 = p04db1,
    bc4_gender_id = p04b3,
    bc4_white = p04b4_1,
    bc4_amer_ind = p04b4_3,
    bc4_race_other = p04b4_18,
    bc4_race_tribe_text = p04b4_3_text,
    bc4_race_other_text = p04b4_18_text,
    bc4_latino = p04b5,
    bc4_latino_other = p04b55_4,
    bc4_latino_other_text = p04b55_4_text,
    bc4_custody = p04b6,
    bc4_drug_preg = p04l,
    bc4_drug_tri1 = p04n_1,
    bc4_drug_tri2 = p04n_2,
    bc4_days_drug_preg = p04m,
    bc4_alc_preg = p04q,
    bc4_tob_preg = p04v,
    bc4_tob_tri1 = p04x_1,
    bc4_tob_tri2 = p04x_2,
    bc4_tob_tri3 = p04x_3,
    bc4_days_tob_preg = p04w,

    bc5_tc_age_preg = p05a,
    bc5_preg_attempt = p05c,
    bc5_preg_result = p05d,
    bc5_tri_still_preg = p05g,
    bc5_tri_mis_abort = p05g2,
    bc5_name = p05b1,
    bc5_dob = p05b2,
    bc5_birth_04 = p05db1,
    bc5_gender_id = p05b3,
    bc5_white = p05b4_1,
    bc5_amer_ind = p05b4_3,
    bc5_race_other = p05b4_18,
    bc5_race_tribe_text = p05b4_3_text,
    bc5_race_other_text = p05b4_18_text,
    bc5_latino = p05b5,
    bc5_latino_other = p05b55_4,
    bc5_latino_other_text = p05b55_4_text,
    bc5_custody = p05b6,
    bc5_drug_preg = p05l,
    bc5_drug_tri1 = p05n_1,
    bc5_drug_tri2 = p05n_2,
    bc5_days_drug_preg = p05m,
    bc5_alc_preg = p05q,
    bc5_tob_preg = p05v,
    bc5_tob_tri1 = p05x_1,
    bc5_tob_tri2 = p05x_2,
    bc5_tob_tri3 = p05x_3,
    bc5_days_tob_preg = p05w,

    bc6_tc_age_preg = p06a,
    bc6_preg_attempt = p06c,
    bc6_preg_result = p06d,
    bc6_tri_still_preg = p06g,
    bc6_tri_mis_abort = p06g2,
    bc6_drug_preg = p06l,
    bc6_alc_preg = p06q,
    bc6_tob_preg = p06v,
    bc6_tob_tri1 = p06x_1,
    bc6_days_tob_preg = p06w,

    bc7_tc_age_preg = p07a,
    bc7_preg_attempt = p07c,
    bc7_preg_result = p07d,
    bc7_still_preg = p07g,

    bc_parent_num = r0start,

    pc_bc1_white = r01a_c_1_1799, #pc = previous custody
    pc_bc1_black = r01a_c_2_1800,
    pc_bc1_amer_ind = r01a_c_3_1801,
    pc_bc1_other_asian_all = r01a_c_5_1803,
    pc_bc1_filipino = r01a_c_7,
    # pc_bc1_vietnamese= r01a_c_10,
    pc_bc1_samoan = r01a_c_15,
    pc_bc1_other_pacific = r01a_c_16,
    pc_bc1_race_other = r01a_c_18,
    pc_bc1_race_tribe_text= r01a_c_3_text,
    pc_bc1_race_pacific_text = r01a_c_16_text,
    pc_bc1_race_other_text = r01a_c_18_text,
    pc_bc1_birth_04 = r01_screen,
    pc_bc1_latino = r01a_c,
    pc_bc1_mexican = r01a_c_1_1823,
    pc_bc1_puerto_rican = r01a_c_2_1824,
    pc_bc1_cuban = r01a_c_3_1825,
    pc_bc1_latino_other = r01a_c_4_1826,
    pc_bc1_latino_dontknow = r01a_c_5_1827,
    pc_bc1_latino_other_text = r01a_c_4_text,
    pc_bc1_custody = r01g,
    pc_bc1_custody_other_rel= r01g_7_text,
    pc_bc1_lost_custody = r01j,

    pc_bc2_white = r02a_c_1_1833,
    pc_bc2_black = r02a_c_2_1834,
    pc_bc2_amer_ind = r02a_c_3_1835,
    # pc_bc2_asian_indian = r02a_c_4_1836,
    pc_bc2_filipino = r02a_c_7,
    # pc_bc2_vietnamese = r02a_c_10,
    # pc_bc2_native_hawaii_all = r02a_c_12,
    pc_bc2_native_hawaii = r02a_c_13,
    pc_bc2_other_pacific = r02a_c_16,
    pc_bc2_race_other = r02a_c_18,
    pc_bc2_race_tribe_text = r02a_c_3_text,
    pc_bc2_race_pacific_text = r02a_c_16_text,
    pc_bc2_race_other_text = r02a_c_18_text,
    pc_bc2_birth_04 = r02_screen,
    pc_bc2_latino = r02a_c,
    pc_bc2_mexican = r02a_c_1_1857,
    pc_bc2_puerto_rican = r02a_c_2_1858,
    pc_bc2_latino_other = r02a_c_4_1860,
    pc_bc2_latino_other_text = r02a_c_4_text,
    pc_bc2_custody = r02g,
    pc_bc2_custody_other_rel = r02g_7_text,
    pc_bc2_lost_custody = r02j,

    pc_bc3_white = r03a_c_1_1867,
    pc_bc3_black = r03a_c_2_1868,
    pc_bc3_amer_ind = r03a_c_3_1869,
    pc_bc3_filipino = r03a_c_7,
    pc_bc3_other_asian = r03a_c_11,
    pc_bc3_native_hawaii_all = r03a_c_12,
    pc_bc3_native_hawaii = r03a_c_13,
    pc_bc3_race_other = r03a_c_18,
    pc_bc3_decline = r03a_c_77_1885,
    pc_bc3_race_tribe_text = r03a_c_3_text,
    pc_bc3_race_other_text = r03a_c_18_text,
    pc_bc3_birth_04 = r03_screen,
    pc_bc3_latino = r03a_c2,
    pc_bc3_mexican = r03a_c_1_1891,
    pc_bc3_puerto_rican = r03a_c_2_1892,
    pc_bc3_latino_other = r03a_c_4_1894,
    pc_bc3_latino_other_text = r03a_c_4_text,
    pc_bc3_custody = r03g,
    pc_bc3_custody_other_rel = r03g_7_text,
    pc_bc3_lost_custody = r03j,

    pc_bc4_white = r04a_c_1_1901,
    pc_bc4_black = r04a_c_2_1902,
    pc_bc4_amer_ind = r04a_c_3_1903,
    pc_bc4_filipino = r04a_c_7,
    pc_bc4_native_hawaii_all = r04a_c_12,
    pc_bc4_native_hawaii = r04a_c_13,
    pc_bc4_race_other = r04a_c_18,
    pc_bc4_race_tribe_text = r04a_c_3_text,
    pc_bc4_race_other_text = r04a_c_18_text,
    pc_bc4_birth_04 = r04_screen,
    pc_bc4_latino = r04a_c2,
    pc_bc4_mexican = r04a_c_1_1925,
    pc_bc4_custody = r04g,
    pc_bc4_lost_custody = r04j,

    pc_bc5_white = r05a_c_1_1935,
    pc_bc5_amer_ind = r05a_c_3_1937,
    pc_bc5_race_other = r05a_c_18,
    pc_bc5_race_tribe_text = r05a_c_3_text,
    pc_bc5_race_other_text = r05a_c_18_text,
    pc_bc5_birth_04 = r05_screen,
    pc_bc5_latino = r05a_c2,
    pc_bc5_mexican = r05a_c_1_1959,
    pc_bc5_custody = r05g,
    pc_bc5_lost_custody = r05j,

    # pc_bc6_birth_04 = r06_screen,
    # pc_bc6_latino = r06a_c2,
    # pc_bc6_custody = r06g,
    # pc_bc6_lost_custody = r06j,
    #
    # pc_bc7_birth_04 = r07_screen,
    # pc_bc7_latino = r07a_c2,
    # pc_bc7_custody = r07g,
    # pc_bc7_lost_custody = r07j,
    #
    # pc_bc8_birth_04 = r08_screen,
    # pc_bc8_latino = r08a_c2,
    # pc_bc8_custody = r08g,
    # pc_bc8_lost_custody = r08j,
    #
    # pc_bc9_birth_04 = r09_screen,
    # pc_bc9_latino = r09a_c2,
    # pc_bc9_custody = r09g,
    # pc_bc9_lost_custody = r09j,
    #
    # pc_bc10_birth_04 = r10_screen,
    # pc_bc10_latino = r10a_c2,
    # pc_bc10_custody = r10g,
    # pc_bc10_lost_custody = r10j,

    sc_parent_num = spm00,

    sm_sc1_white = spm1b_1,
    sm_sc1_black = spm1b_2,
    sm_sc1_filipino = spm1b_7,
    sm_sc1_native_hawaii_all = spm1b_12,
    sm_sc1_native_hawaii = spm1b_13,
    sm_sc1_race_other = spm1b_18,
    sm_sc1_race_other_text = spm1b_18_text,
    sm_sc1_latino = spm1c_2162,
    sm_sc1_mexican = spm1c1_1,
    sm_sc1_welfare_contact = spm1b,
    sm_sc1_living_with = spm1c_2171,

    sm_sc2_white = spm2b_1,
    sm_sc2_black = spm2b_2,
    sm_sc2_other_pacific = spm2b_16,
    sm_sc2_race_other = spm2b_18,
    sm_sc2_race_pacific_text = spm2b_16_text,
    sm_sc2_race_other_text = spm2b_18_text,
    sm_sc2_latino = spm2c,
    sm_sc2_mexican = spm2c1_1,
    sm_sc2_welfare_contact = spm2d,
    sm_sc2_living_with = spm2e,

    sm_sc3_white = spm3b_1,
    sm_sc3_other_pacific = spm3b_16,
    sm_sc3_race_other = spm3b_18,
    sm_sc3_race_pacific_text = spm3b_16_text,
    sm_sc3_race_other_text = spm3b_18_text,
    sm_sc3_latino = spm3c,
    sm_sc3_mexican = spm3c1_1,
    sm_sc3_welfare_contact = spm3d,
    sm_sc3_living_with = spm3e,

    # sm_sc4_latino = spm4c,
    # sm_sc4_welfare_contact = spm4d,
    # sm_sc4_living_with = spm4e,

    # sm_sc5_latino = spm5c,
    # sm_sc5_welfare_contact = spm5d,
    # sm_sc5_living_with = spm5e,

    other_sc= sp00, #other stepchildren under 17 since last interview
    other_sc_num= sp000,

    other_sc1_name = sp1a,
    other_sc1_dob = sp1b,
    other_sc1_gender_id= sp1c,
    other_sc1_white = sp1d_1,
    other_sc1_black = sp1d_2,
    other_sc1_other_pacific = sp1d_16,
    other_sc1_race_other = sp1d_18,
    other_sc1_race_pacific_text= sp1d_16_text,
    other_sc1_race_other_text = sp1d_18_text,
    other_sc1_latino = sp1d1,
    other_sc1_mexican = sp1d1a_1,
    other_sc1_other_latino = sp1d1a_4,
    other_sc1_latino_other_text = sp1d1a_4_text,
    other_sc1_welfare_contact = sp1e,
    other_sc1_living_with = sp1f,

    other_sc2_name = sp2a,
    other_sc2_dob = sp2b,
    other_sc2_gender_id = sp2c,
    other_sc2_white = sp2d_1,
    other_sc2_black = sp2d_2,
    other_sc2_amer_ind = sp2d_3,
    other_sc2_race_other = sp2d_18,
    other_sc2_race_tribe_text = sp2d_3_text,
    other_sc2_race_other_text = sp2d_18_text,
    other_sc2_latino = sp2d1,
    other_sc2_mexican = sp2d1a_1,
    other_sc2_other_latino = sp2d1a_4,
    other_sc2_dontknow = sp2d1a_5,
    other_sc2_decline = sp2d1a_77,
    other_sc2_latino_other_text= sp2d1a_4_text,
    other_sc2_welfare_contact = sp2e,
    other_sc2_living_with = sp2f,

    other_sc3_name = sp3a,
    other_sc3_dob = sp3b,
    other_sc3_gender_id = sp3c,
    other_sc3_white = sp3d_1,
    other_sc3_amer_ind = sp3d_3,
    other_sc3_race_other = sp3d_18,
    other_sc3_race_tribe_text = sp3d_3_text,
    other_sc3_race_other_text = sp3d_18_text,
    other_sc3_latino = sp3d1,
    other_sc3_mexican = sp3d1a_1,
    other_sc3_welfare_contact = sp3e,
    other_sc3_living_with = sp3f,

    other_sc4_name = sp4a,
    other_sc4_dob = sp4b,
    other_sc4_gender_id = sp4c,
    other_sc4_white = sp4d_1,
    other_sc4_amer_ind = sp4d_3,
    other_sc4_race_tribe_text = sp4d_3_text,
    other_sc4_latino = sp4d1,
    other_sc4_welfare_contact = sp4e,
    other_sc4_living_with = sp4f,

    other_kid = ct0,
    other_kid_num = ct000,

    other_kid1_name = nc1,
    other_kid1_dob = nc1a,
    other_kid1_gender_id = nc1b,
    other_kid1_white = nc1c_1,
    other_kid1_black = nc1c_2,
    other_kid1_amer_ind = nc1c_3,
    other_kid1_other_asian_all = nc1c_5,
    other_kid1_race_tribe_text = nc1c_3_text,
    other_kid1_mexican = nc1c1a_1, #no asking about being latino
    # other_kid1_dontknow = nc1c1a_5,
    other_kid1_decline = nc1c1a_77,
    other_kid1_relation = nc1d,
    other_kid1_welfare_contact = nc1e,
    other_kid1_living_with = nc1f,

    other_kid2_name = nc2,
    other_kid2_dob = nc2a,
    other_kid2_gender_id = nc2b,
    other_kid2_white = nc2c_1,
    other_kid2_other_asian_all = nc2c_5,
    # other_kid2_race_other = nc2c_18,
    # other_kid2_race_other_text = nc2c_18_text,
    other_kid2_latino = nc2c1,
    # other_kid2_mexican = nc2c1a_1,
    other_kid2_relation = nc2d,
    other_kid2_welfare_contact = nc2e,
    other_kid2_living_with = nc2f,

    # other_kid3_name = nc3,
    # other_kid3_dob = nc3a,
    # other_kid3_gender_id = nc3b,
    # other_kid3_race_other = nc3c_18,
    # other_kid3_race_other_text = nc3c_18_text,
    # other_kid3_latino = nc3c1,
    # other_kid3_mexican = nc3c1a_1,
    # other_kid3_relation = nc3d,
    # other_kid3_welfare_contact = nc3e,
    # other_kid3_living_with = nc3f,

    # other_kid4_name = nc4,
    # other_kid4_dob = nc4a,
    # other_kid4_gender_id = nc4b,
    # other_kid4_race_other = nc4c_18,
    # other_kid4_race_other_text = nc4c_18_text,
    # other_kid4_latino = nc4c1,
    # other_kid4_mexican = nc4c1a_1,
    # other_kid4_relation = nc4d,
    # other_kid4_welfare_contact = nc4e,
    # other_kid4_living_with = nc4f,

    # other_kid5_name = nc5,
    # other_kid5_dob = nc5a,
    # other_kid5_gender_id = nc5b,
    # other_kid5_race_other = nc5c_18,
    # other_kid5_race_other_text = nc5c_18_text,
    # other_kid5_latino = nc5c1,
    # other_kid5_mexican = nc5c1a_1,
    # other_kid5_relation = nc5d,
    # other_kid5_welfare_contact = nc5e,
    # other_kid5_living_with = nc5f
  )

# write.csv(
#   ex,
#   'interview_no_na.csv'
#   )

# ex %>%
#   filter(id != 'P813') %>%
#   select(id,
#          p00aa:p10v) %>%
#   pivot_longer(
#     cols = -id,
#     names_to = 'pregnancy',
#     values_to = 'preg_values'
#   ) %>%
#   View()

ex <- ex %>%
  select(-...1:-user_language) %>%
  relocate(id,
           .before = 1)

names(maria_file)
glimpse(maria_file)


comb_long_preg <-
  ex %>%
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
  )

comb_long_preg %>%
  drop_na(
    preg_result,
    curr_preg
    ) %>%
  group_by(id) %>%
  count(preg_result_name, preg_result, curr_preg) %>%
  ungroup() %>%
  arrange(id) %>%
  mutate(
    new_birth = case_when(
    preg_result %in% c('birth', 'multiple_birth') ~ 1,
    !preg_result %in% c('birth', 'multiple_birth') ~ 0,
    TRUE ~ 0
    )
  ) %>%
  group_by(id) %>%
  summarize(
    new_birth_counts = sum(new_birth)
    ) %>%
  ungroup()

comb_long_preg %>%
  drop_na(
    preg_result,
    curr_preg
  ) %>%
  group_by(id) %>%
  count(preg_result_name, preg_result, curr_preg) %>%
  ungroup() %>%
  arrange(id) %>%
  mutate(
    new_birth = case_when(
      preg_result %in% c('birth', 'multiple_birth') ~ 1,
      !preg_result %in% c('birth', 'multiple_birth') ~ 0,
      TRUE ~ 0
    )
  ) %>%
  summarize(
    total_birth_counts = sum(new_birth)
  )

comb_long_preg %>%
  count(preg_result) %>%
  drop_na()


# calculating total number of children
# comb_long_numkid <-
ex %>%
  select(
    id,
    matches('name$'),
    matches('^pc.*birth_04'),
    matches('^pc.*custody'),
    matches('^pc.*lost_custody'),
    matches('sm_.*welfare_contact')
  ) %>%
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
  ) %>%
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
    names_from = type_of_kid,
    values_from = yes_kiddo
  )
  # group_by(id) %>%
  # summarize(
  #   total_num_kids = sum(n)
  #   ) %>%
  # ungroup()

# number of stepchildren
comb_long_step <-
ex %>%
  pivot_longer(
    cols = c(matches('name$')),
    names_to = 'kid_name',
    values_to = 'actual_name'
  ) %>%
  drop_na(kid_name, actual_name) %>%
  group_by(id, kid_name) %>%
  mutate(
    kid_count = n()
         ) %>%
  ungroup() %>%
  select(
    id,
    kid_name,
    kid_count,
    sc_parent_num
    ) %>%
  filter(
    str_detect(
      kid_name,
      '^other_sc'
    )
  ) %>%
  group_by(id) %>%
  summarize(
    total_stepkid = kid_count + sc_parent_num
    ) %>%
  ungroup()

# number of other children
comb_long_other <-
ex %>%
  pivot_longer(
    cols = c(matches('name$')),
    names_to = 'kid_name',
    values_to = 'actual_name'
  ) %>%
  drop_na(kid_name, actual_name) %>%
  group_by(id, kid_name) %>%
  count(actual_name) %>%
  ungroup() %>%
  filter(
    str_detect(
      kid_name,
      '^other'
    ),
    !str_detect(
      kid_name,
      '_sc\\d_'
    )
  ) %>%
  group_by(id) %>%
  summarize(
    total_other_kid = sum(n,
    na.rm = TRUE)
            )

# age at each pregnancy and the pregnancy numbered list

comb_long_age <-
ex %>%
  pivot_longer(
    cols = c(matches('age_preg$')),
    names_to = 'kid_number',
    values_to = 'age_preg'
  ) %>%
  drop_na(kid_number, age_preg) %>%
  group_by(id, kid_number) %>%
  count(age_preg) %>%
  ungroup() %>%
  separate(
    col = kid_number,
    into = c('birth_number',
             'tc_remove',
             'age_remove',
             'preg_remove')
  ) %>%
  relocate(
    birth_number,
    .after = id
  ) %>%
  select(
    -matches(
      'remove$'
    ),
    -n
  )


# child race
comb_long_race <-
ex %>%
  pivot_longer(
    cols = c(matches('white$'),
             matches('black$'),
             matches('amer_ind$'),
             matches('samoan$'),
             matches('race_other$'),
             matches('filipino$'),
             matches('vietnames$'),
             matches('other_pacific$'),
             matches('native_hawaii$'),
             matches('native_hawaii_all$'),
             matches('other_asian_all$'),
             matches('asian_indian$'),
             matches('text$')),
    names_to = 'race_name',
    values_to = 'race'
  ) %>%
  drop_na(race_name, race)

library(lubridate)

# months from baseline
assess_date <-
  ex %>%
  drop_na(assessment_screen) %>%
  separate(
    col = assessment_screen,
    into = c('month',
             'day',
             'year'),
    sep = '/'
  ) %>%
  unite(
    col = 'assessment_screen',
    c(month, day, year),
    sep = '-'
  ) %>%
  mutate(
    tp_assess_date = mdy(assessment_screen)
  )

comb_long_dob <-
  ex %>%
pivot_longer(
  cols = matches('dob$'),
  names_to = 'dob_name',
  values_to = 'dob'
) %>%
  drop_na(dob_name, dob)


comb_long_eth <-
ex %>%
pivot_longer(
  cols = c(matches('latino_other$'),
           matches('mexican$'),
           matches('cuban$'),
           matches('puerto_rican$')),
  names_to = 'latino_group_name',
  values_to = 'latino_group'
) %>%
  drop_na(latino_group_name, latino_group)

comb_long_gender <-
ex %>%
  pivot_longer(
    cols = matches('gender_id$'),
    names_to = 'gender_name',
    values_to = 'gender_id'
  ) %>%
  drop_na(gender_name,
          gender_id)

comb_long_custody <-
ex %>%
  pivot_longer(
    cols = matches('\\d_custody$'),
    names_to = 'custody_name',
    values_to = 'custody'
  ) %>%
  drop_na(custody_name, custody)



assess_date_sub <-
  assess_date %>%
  select(
    id,
    tp_assess_date
    )

child_sub <-
  child_history %>%
  mutate(
    family = fam_id
    ) %>%
  select(
    dob,
    total_number_of_children:number_of_other_children,
    preg_num:child_num,
    child_dob:child_n,
    date0,
    family
  )


combined <-
  maria_file %>%
  left_join(
    child_sub
  )

combined <-
  combined %>%
  select(
    family:dob,
    date0
  ) %>%
  rename(
    first_assessment = date0
  )

comb_long_age <-
  comb_long_age %>%
  rename(
    family = id
    )

jp <-
  left_join(combined, comb_long_age)

jp <-
  comb_long_numkid %>%
  rename(family = id) %>%
  right_join(jp)

jp <-
  comb_long_other %>%
  rename(family = id) %>%
  right_join(jp)

jp <-
  comb_long_preg %>%
  # comb_long_preg_find %>%
  rename(family = id) %>%
  right_join(jp)

jp <-
  comb_long_step %>%
  rename(family = id) %>%
  right_join(jp)

jp <-
  assess_date_sub %>%
  rename(family = id) %>%
  right_join(jp)

jp <-
  comb_long_race %>%
    rename(family = id) %>%
    right_join(jp)

# issue
# jp <-
  comb_long_dob %>%
  select(
    dob_name,
    dob
  ) %>%
    rename(family = id) %>%
    right_join(jp)

# need to include custody
jp <-
  comb_long_custody %>%
  select(
    id,
    custody_name,
    custody
    ) %>%
    rename(family = id) %>%
    right_join(jp)

# need to include ethnicity
jp <-
  comb_long_eth %>%
    select(
      id,
      latino_group_name,
      latino_group
    ) %>%
    rename(family = id) %>%
    right_join(jp)

jp <-
  jp %>%
  distinct(
    across(
      .cols = everything()
    )
  )

jp <- jp %>%
  drop_na(first_assessment)

jp <-
  jp %>%
  relocate(
    first_assessment,
    .after = family
  ) %>%
    relocate(
      curr_preg,
      .before = preg_result_name
    ) %>%
    relocate(
      total_stepkid,
      .before = total_other_kid
    ) %>%
    relocate(
      n,
      .after = age_preg
    ) %>%
    relocate(
      nfam,
      .before = first_assessment
    ) %>%
    relocate(
      cohort,
      .before = first_assessment
    ) %>%
    relocate(
      dob,
      .before = first_assessment
    ) %>%
    relocate(
      glptx,
      .before = first_assessment
    ) %>%
  relocate(
    age_preg,
    .before = total_stepkid
  ) %>%
  relocate(
    birth_number,
    .after = age_preg
  ) %>%
    select(-n) %>%
  mutate(
    birth_number = case_when(
      ((preg_result %in% c('stillbirth', 'abortion','miscarriage')) &
         !is.na(birth_number)) ~ NA_character_,
      TRUE ~ birth_number
    )
  )


jp <-
  jp %>%
  mutate(
    month_compare = lubridate::interval(
      first_assessment,
      tp_assess_date
      )
  )

jp$month_diff <- jp$month_compare %/% months(1)

jp <-
  jp %>%
  arrange(family)

View(jp)

# write.csv(preg_result_count,
#           'preg_result_count.csv')




# number of new pregnancies - DONE
# number of cumulative pregnancies at this wave - DONE
# number of new live births
# number of cumulative live births at this wave
# summary variable y/n in custody
# number of children in tc custody
# each pregnancy, create a variable that codes the outcome of the pregnancy
# total number of pregnancies
# total number of childbirth
# age of first pregnancy
# age of first childbirth

# new file with
# tc id
# each pregnancy labeled
# each birth numbered
# child age
# gender
# race/ethnicity
# custody status




































  # select(
  #   id,
  #   wave,
  #   times_preg, #preg_num
  #   curr_preg, # preg_age
  #   preg_result_name, #pregnancy result for each biological child ()
  #   preg_result #pregnancy result
    # number of children
    # number of pregnancy losses
    # number of new pregnancies
    # number stepchildren
    # number other children
    # child order/numbered order
    # dob_name,
    # dob, # child dob
    # kid_name,
    # actual_name, # child name
    # custody_name,
    # custody # custody
    # race
  # ) %>%


























ex1 <-
  ex1 %>%
  pivot_longer(
    cols = matches('dob$'),
    names_to = 'dob_name',
    values_to = 'dob'
  ) %>%
  relocate(
    c(dob_name,
      dob),
    .after = actual_name
  ) %>%
  drop_na(dob_name, dob)

# race
ex1 <-
  ex1 %>%
  pivot_longer(
    cols = c(matches('white$'),
             matches('black$'),
             matches('amer_ind$'),
             matches('samoan$'),
             matches('race_other$'),
             matches('filipino$'),
             matches('vietnames$'),
             matches('other_pacific$'),
             matches('native_hawaii$'),
             matches('native_hawaii_all$'),
             matches('other_asian_all$'),
             matches('asian_indian$'),
             matches('text$')),
    names_to = 'race_name',
    values_to = 'race'
  ) %>%
  relocate(
    c(race_name,
      race),
    .after = dob
  ) %>%
  drop_na(race_name, race)

# latino groups
ex1 <-
  ex1 %>%
  pivot_longer(
    cols = c(matches('latino_other$'),
             matches('mexican$'),
             matches('cuban$'),
             matches('puerto_rican$')),
    names_to = 'latino_group_name',
    values_to = 'latino_group'
  ) %>%
  relocate(
    c(latino_group_name,
      latino_group),
    .after = race
  ) %>%
  drop_na(latino_group_name, latino_group)

# gender_id

ex2 <-
  ex1 %>%
  pivot_longer(
    cols = matches('gender_id$'),
    names_to = 'gender_name',
    values_to = 'gender_id'
  ) %>%
  relocate(
    c(gender_name,
      gender_id),
    .after = latino_group
  ) %>%
  drop_na(gender_name,
          gender_id)


# custody
ex2 <-
  ex2 %>%
  pivot_longer(
    cols = matches('\\d_custody$'),
    names_to = 'custody_name',
    values_to = 'custody'
  ) %>%
    relocate(
      c(custody,
        custody_name),
      .after = gender_id
    ) %>%
  drop_na(custody_name, custody)

# building df for maria here

ex2 %>%
  # filter(
  #   id == 'HR135' |
  #   id == 'HR147' |
  #   id == 'P828'
  #        ) %>%
  mutate(
    wave = 'tp'
  ) %>%
  pivot_longer(
    cols = matches('preg_result$'),
    names_to = 'preg_result_name',
    values_to = 'preg_result'
  ) %>%
  select(
    id,
    wave,
    times_preg, #preg_num
    curr_preg, # preg_age
    preg_result_name, #pregnancy result for each biological child ()
    preg_result, #pregnancy result
     # number of children
     # number of pregnancy losses
     # number of new pregnancies
     # number stepchildren
     # number other children
     # child order/numbered order
    dob_name,
    dob, # child dob
    kid_name,
    actual_name, # child name
    custody_name,
    custody # custody
     # race
    ) %>%
  filter(
    str_detect(kid_name, 'bc\\d')
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
    )
  ) %>%
  # drop_na() %>%
  distinct(
    id,
    preg_result_name,
    preg_result,
    kid_name,
    actual_name
    ) %>%
  group_by(id) %>%
  count(
    preg_result,
    preg_result_name,
    kid_name,
    actual_name
        ) %>%
  ungroup() %>%
  separate(
    preg_result_name,
    into = c('bc_preg_num', 'remove', 'result'),
    sep = '_'
  ) %>%
  separate(
    kid_name,
    into = c('bc_name_num', 'remove1'),
    sep = '_'
    ) %>%
  mutate(
    same_bc = if_else(
      bc_preg_num == bc_name_num, 'same', 'different'
    )
  ) %>%
  filter(same_bc == 'same')




# distinct(
  # across(
    # .cols = everything()
  # )
# )



  # mutate(
  #   wave = 'tp',
  #   curr_preg = case_when(
  #     p00aa == 1 ~ 'yes',
  #     p00aa == 2 ~ 'no',
  #     p00aa == 3 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   try_preg = case_when(
  #     p01c == 1 ~ 'yes',
  #     p01c == 2 ~ 'no',
  #     p01c == 3 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   preg_result = case_when(
  #     p01d == 1 ~ 'still_preg',
  #     p01d == 2 ~ 'miscarriage',
  #     p01d == 3 ~ 'abortion',
  #     p01d == 4 ~ 'birth',
  #     p01d == 5 ~ 'multiple_birth',
  #     p01d == 6 ~ 'stillbirth',
  #     p01d == 7 ~ 'false_pos',
  #     p01d == 8 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   trimester_curr = case_when(
  #     p01g == 1 ~ 'first_tri',
  #     p01g == 2 ~ 'second_tri',
  #     p01g == 3 ~ 'third_tri',
  #     p01g == 4 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   trimester_loss = case_when(
  #     p01g2 == 1 ~ 'first_tri',
  #     p01g2 == 2 ~ 'second_tri',
  #     p01g2 == 3 ~ 'third_tri',
  #     p01g2 == 4 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   born_before_04 = case_when(
  #     p01db1 == 1 ~ 'yes',
  #     p01db1 == 2 ~ 'no',
  #     TRUE ~ NA_character_
  #   ),
  #   child_gender_identity = case_when(
  #     p01b3 == 1 ~ 'cis_fem',
  #     p01b3 == 2 ~ 'female',
  #     p01b3 == 3 ~ 'cis_male',
  #     p01b3 == 4 ~ 'male',
  #     p01b3 == 5 ~ 'trans_fem',
  #     p01b3 == 6 ~ 'trans_male',
  #     p01b3 == 7 ~ 'non_binary',
  #     p01b3 == 8 ~ 'self_id',
  #     p01b3 == 9 ~ 'decline',
  #     TRUE ~ NA_character_
  #   ),
  #   child_latino = case_when(
  #     p01b5 == 1 ~ 'yes',
  #     p01b5 == 2 ~ 'no',
  #     p01b5 == 3 ~ 'dont_know',
  #     p01b5 == 4 ~ 'decline',
  #     p01b5 == 5 ~ NA_character_
  #   )
  # )












# first_preg <-
#   interview %>%
#   select(
#     id,
#     p00a, #times pregnant since last talk
#     p00aa, #currently pregnant
#     p01a, #tc age
#     p01c, #trying to get pregnant
#     p01d, #what happened to the pregnancy
#     p01g, #trimester_current
#     p01g2, #trimester_loss
#     p01b, #child name
#     p01b2, #child DOB
#     p01db1, #born before today's date 2004
#     p01b3, #child gender identity
#     p01b3_8_text, # gender identity (self identity)
#     p01b4_1, # race - white
#     p01b4_2, #race - black
#     p01b4_3, #race - american indian
#     p01b4_4, #race - asian indian
#     p01b4_5, #race - other asian
#     p01b4_6, #race - chinese
#     p01b4_7, #race - filipino
#     p01b4_8, #race -  japanese
#     p01b4_9, #race -  korean
#     p01b4_10, #race - vietnamese
#     p01b4_11, #race - other
#     p01b4_12, #race - native hawaiian/other pacific islander
#     p01b4_13, #race - native hawaiian
#     p01b4_14, #race - guamanian
#     p01b4_15, #race - samoan
#     p01b4_16, #race - other - entry
#     p01b4_17, #race - don't know
#     p01b4_18, #race - other race - entry
#     p01b4_77, #race - decline to answer
#     p01b4_3_text, #race - tribe text
#     p01b4_16_text, #race - other race
#     p01b4_18_text, #race - other race text,
#     p01b5, #latino
#     p01b55_1, #mexican
#     p01b55_2, #puerto rican
#     p01b55_3,  #cuban
#     p01b55_4, #other
#     p01b55_5, #don't know
#     p01b55_77, #declined
#     p01b55_4_text, #other text
#     p01b6, #who currently has custody
#     p01b6_7_text, #other relative specificed text
#     p01mb0, # how many children
#     p01mb1, #child name
#     p01mb2, #child DOB
#     p01db2, # child born before 2004
#     p01db3, #child gender identity
#     p01mb3_8_text, #self id gender
#     p01mb4_1, #race - white
#     p01mb4_2, #race - black
#     p01mb4_3, #race - american indian
#     p01mb4_4, #race - asian indian
#     p01mb4_5, #race - other asian
#     p01mb4_6, #race - chinese
#     p01mb4_7, #race - filipino
#     p01mb4_8, #race -  japanese
#     p01mb4_9, #race -  korean
#     p01mb4_10, #race - vietnamese
#     p01mb4_11, #race - other
#     p01mb4_12, #race - native hawaiian/other pacific islander
#     p01mb4_13, #race - native hawaiian
#     p01mb4_14, #race - guamanian
#     p01mb4_15, #race - samoan
#     p01mb4_16, #race - other - entry
#     p01mb4_17, #race - don't know
#     p01mb4_18, #race - other race - entry
#     p01mb4_77, #race - decline to answer
#     p01mb4_3_text, #race - tribe text
#     p01mb4_16_text, #race - other race
#     p01mb4_18_text, #race - other race text
#     p01mb5, #child latino
#     p01mb55_1, #mexican
#     p01mb55_2, #puerto rican
#     p01mb55_3, #cuban
#     p01mb55_4, #other
#     p01mb55_5, #don't know
#     p01mb55_77, #declined
#     p01mb55_4_text, #other text
#     p01b6, #who currently has custody
#     p01b6_7_text, #other relative specificed text
#     p01mb0, # how many children
#     p01mb1, #child name
#     p01mb2, #child DOB
#     p01db2, # child born before 2004
#     p01mb3, #child gender identity
#     p01mb3_8_text, #self id gender
#     p01mb4_1,#race - white
#     p01mb4_2,#race - black
#     p01mb4_3,#race - american indian
#     p01mb4_4,#race - asian indian
#     p01mb4_5,#race - other asian
#     p01mb4_6,#race - chinese
#     p01mb4_7,#race - filipino
#     p01mb4_8,#race - japanese
#     p01mb4_9,#race - korean
#     p01mb4_10,#race - vietnamese
#     p01mb4_11,#race - other
#     p01mb4_12,#race - native hawaiian/other pacific islander
#     p01mb4_13,#race - native hawaiian
#     p01mb4_14,#race - guamanian
#     p01mb4_15,#race - samoan
#     p01mb4_16,#race - other - entry
#     p01mb4_17,#race - don't know
#     p01mb4_18,#race - other race - entry
#     p01mb4_77,#race - decline to answer
#     p01mb4_3_text, #race - tribe text
#     p01mb4_16_text, #race - other race
#     p01mb4_18_text,#race - other race text
#     p01mb5, #child latino
#     p01mb55_1,#mexican
#     p01mb55_2,#puerto rican
#     p01mb55_3,#cuban
#     p01mb55_4,#other
#     p01mb55_5,#don't know
#     p01mb55_77, #declined
#     p01mb55_4_text, #other text
#     p01mb6, #who currently has custody
#     p01mb6_7_text, #other relative specificed text
#     p01mb1b,# child's name
#     p01mb2b, #child DOB
#     p01db3, # child born before 2004
#     p01mb3b, #child gender identity
#     p01mb3b_8_text, #self id gender
#     p01mb4b_1, #race - white
#     p01mb4b_2, #race - black
#     p01mb4b_3, #race - american indian
#     p01mb4b_4, #race - asian indian
#     p01mb4b_5, #race - other asian
#     p01mb4b_6, #race - chinese
#     p01mb4b_7, #race - filipino
#     p01mb4b_8, #race - japanese
#     p01mb4b_9, #race -  korean
#     p01mb4b_10, #race - vietnamese
#     p01mb4b_11, #race - other
#     p01mb4b_12, #race - native hawaiian/other pacific islander
#     p01mb4b_13, #race - native hawaiian
#     p01mb4b_14, #race - guamanian
#     p01mb4b_15, #race - somoan
#     p01mb4b_16, #race - other - entry
#     p01mb4b_17, #race - don't know
#     p01mb4b_18, #race - other race - entry
#     p01mb4b_77, #race - decline to answer
#     p01mb4b_3_text, #race - tribe text
#     p01mb4b_16_text,#race - other race
#     p01mb4b_18_text, #race - other race text
#     p01mb5b, #child latino
#     p01mb5bb_1, # mexican
#     p01mb5bb_2, # puerto rican
#     p01mb5bb_3, # cuban
#     p01mb5bb_4, # other
#     p01mb5bb_5, # don't know
#     p01mb5bb_77,#declined
#     p01mb5bb_4_text, #other text
#     p01mb6b, #who currently has custody
#     p01mb6b_7_text, #other relative specificed text
#     p01mb1c, #child name
#     p01mb2c, #child DOB
#     p01db4, # child born before 2004
#     p01mb3c, #child gender identity
#     p01mb3c_8_text, #self id gender
#     p01mb4c_1, #race - white
#     p01mb4c_2, #race - black
#     p01mb4c_3, #race - american indian
#     p01mb4c_4, #race - asian indian
#     p01mb4c_5, #race - other asian
#     p01mb4c_6, #race - chinese
#     p01mb4c_7, #race - filipino
#     p01mb4c_8, #race - japanese
#     p01mb4c_9, #race - korean
#     p01mb4c_10, #race - vietnamese
#     p01mb4c_11, #race - other
#     p01mb4c_12, #race - native hawaiian/other pacific islander
#     p01mb4c_13, #race - native hawaiian
#     p01mb4c_14, #race - guamanian
#     p01mb4c_15, #race - somoan
#     p01mb4c_16, #race - other - entry
#     p01mb4c_17, #race - don't know
#     p01mb4c_18, #race - other race - entry
#     p01mb4c_77, #race - decline to answer
#     p01mb4c_3_text,# race - tribe text
#     p01mb4c_16_text, #race - other race
#     p01mb4c_18_text,#race - other race text
#     p01mb5c, #child latino
#     p01mb5cc_1,# mexican
#     p01mb5cc_2,# puerto ricna
#     p01mb5cc_3,# cuban
#     p01mb5cc_4,# other
#     p01mb5cc_5,# don't know
#     p01mb5cc_77, #declined
#     p01mb5cc_4_text, #other text
#     p01mb6c, #who currently has custody
#     p01mb6c_7_text,# other relative specified text
#     p01l, #drugs during pregnancy
#     p01n_1, #first trimester drugs
#     p01n_2, #second trimester drugs
#     p01n_3,#third trimester drugs
#     p01n_77, #declined trimester drugs
#     p01m, #days using drugs during pregnancy
#     p01q, #alcohol during pregnancy
#     p01s_1, #first trimester alcohol
#     p01s_2,#second trimester alcohol
#     p01s_3,#third trimester alcohol
#     p01s_77, #declined trimester alcohol
#     p01r, #days alcohol during pregnancy
#     p01v, #did you use use nicotine during pregnancy
#     p01x_1, #first trimester nicotine
#     p01x_2,#second trimester nicotine
#     p01x_3, #third trimester nicotine
#     p01x_77, #declined trimester nicotine
#     p01w #how many days nicotine during pregnancy
#   )


# first_preg %>%
#   mutate(
#     family = id
#   ) %>%
#   right_join(maria_file,
#             by = 'family'
#   )

# first_preg <-
#   first_preg %>%
#   mutate(
#     wave = 'tp',
#     bio_child_num = 'biological child 1'
#   ) %>%
#   relocate(wave, .after = p00aa) %>%
#   relocate(bio_child_num, .after = wave)

# first_preg %>%
#   mutate(
#     preg_result = case_when(
#       p01d == 1 ~ 'still_preg', #how far along are you (p01g)
#       p01d == 2 ~ 'miscarriage', #how far along were you (p01g2)
#       p01d == 3 ~ 'abortion', #how far along were you (p01g2)
#       p01d == 4 ~ 'birth', #what is your child's name (p01b)
#       p01d == 5 ~ 'multiple_birth', #specify how many children (p01mb)
#       p01d == 6 ~ 'stillbirth', #now i have a very personal question (p01i)
#       p01d == 7 ~ 'false_pos', #pregnancy 2 (p02a)
#       p01d == 8 ~ 'decline', #pregnancy 2 (p02a)
#       TRUE ~ NA_character_
#     ),
#     preg_result = as.factor(preg_result)
#   ) %>%
#   count(preg_result)


# pregnancies <- c('^p01', '^p02', '^p03', '^p04', '^p05',
#                  '^p06', '^p07', '^p08', '^p09', '^p10')

# iterate through each pregnancy
# preg_lists <-
#   map(
#   pregnancies,
#   ~select(
#     interview,
#     matches(.x)
#           )
# )

# ex_df <- preg_lists[[1]] %>%
#   rename(
#     tc_age_preg = p01a, #tc age
#     preg_attempt = p01c, #trying to get pregnant
#     preg_result = p01d, #what happened to the pregnancy
#     tri_still_preg = p01g, #trimester_current
#     tri_mis_abort = p01g2, #trimester_loss
#     name = p01b, #child name
#     dob = p01b2, #child DOB
#     birth_04 = p01db1, #born before today's date 2004
#     gender_id = p01b3, #child gender identity
#     gender_self_id = p01b3_8_text, # gender identity (self identity)
#     white = p01b4_1, # race - white
#     black = p01b4_2, #race - black
#     amer_ind = p01b4_3, #race - american indian
#     asian_indian = p01b4_4, #race - asian indian
#     other_asian_all = p01b4_5, #race - other asian
#     chinese = p01b4_6, #race - chinese
#     filipino = p01b4_7, #race - filipino
#     japanese = p01b4_8, #race -  japanese
#     korean = p01b4_9, #race -  korean
#     vietnamese = p01b4_10, #race - vietnamese
#     other_asian = p01b4_11, #race - other
#     native_hawaii_all = p01b4_12, #race - native hawaiian/other pacific islander
#     native_hawaii = p01b4_13, #race - native hawaiian
#     guamanian = p01b4_14, #race - guamanian
#     samoan = p01b4_15, #race - samoan
#     other_pacific = p01b4_16, #race - other - entry
#     race_dontknow = p01b4_17, #race - don't know
#     race_other = p01b4_18, #race - other race - entry
#     race_decline = p01b4_77, #race - decline to answer
#     race_tribe_text = p01b4_3_text, #race - tribe text
#     race_pacific_text = p01b4_16_text, #race - other race
#     race_other_text = p01b4_18_text, #race - other race text,
#     latino = p01b5, #latino
#     mexican = p01b55_1, #mexican
#     puerto_rican = p01b55_2, #puerto rican
#     cuban = p01b55_3,  #cuban
#     latino_other = p01b55_4, #other
#     latino_dontknow= p01b55_5, #don't know
#     latino_decline = p01b55_77, #declined
#     latino_other_text = p01b55_4_text, #other text
#     custody = p01b6, #who currently has custody
#     custody_other_rel = p01b6_7_text, #other relative specificed text
#     num_kid_twin = p01mb0, # how many children
#     name_twin = p01mb1, #child name
#     dob_twin = p01mb2, #child DOB
#     birth_04_twin = p01db2, # child born before 2004
#     gender_id_twin = p01db3, #child gender identity
#     gender_self_id_twin = p01mb3_8_text, #self id gender
#     white_twin = p01mb4_1, #race - white
#     black_twin = p01mb4_2, #race - black
#     amer_ind_twin = p01mb4_3, #race - american indian
#     asian_indian_twin = p01mb4_4, #race - asian indian
#     other_asian_all_twin = p01mb4_5, #race - other asian
#     chinese_twin = p01mb4_6, #race - chinese
#     filipino_twin = p01mb4_7, #race - filipino
#     japanese_twin = p01mb4_8, #race -  japanese
#     korean_twin = p01mb4_9, #race -  korean
#     vietnamese_twin = p01mb4_10, #race - vietnamese
#     other_asian_twin = p01mb4_11, #race - other
#     native_hawaii_all_twin = p01mb4_12, #race - native hawaiian/other pacific islander
#     native_hawaii_twin = p01mb4_13, #race - native hawaiian
#     samoan_twin = p01mb4_14, #race - guamanian
#     guamanian_twin = p01mb4_15, #race - samoan
#     other_pacific_twin = p01mb4_16, #race - other - entry
#     race_dontknow_twin = p01mb4_17, #race - don't know
#     race_other_twin = p01mb4_18, #race - other race - entry
#     race_decline_twin = p01mb4_77, #race - decline to answer
#     race_tribe_text_twin = p01mb4_3_text, #race - tribe text
#     race_pacific_text_twin = p01mb4_16_text, #race - other race
#     race_other_text_twin = p01mb4_18_text, #race - other race text
#     latino_twin = p01mb5, #child latino
#     mexican_twin = p01mb55_1, #mexican
#     puerto_rican_twin = p01mb55_2, #puerto rican
#     cuban_twin = p01mb55_3, #cuban
#     latino_other_twin = p01mb55_4, #other
#     latino_dontknow_twin = p01mb55_5, #don't know
#     latino_decline_twin = p01mb55_77, #declined
#     latino_other_text_twin = p01mb55_4_text, #other text
#     custody_twin = p01b6, #who currently has custody
#     custody_other_rel_twin = p01b6_7_text, #other relative specificed text
#     num_kid_add_birth1 = p01mb0, # how many children
#     name_add_birth1 = p01mb1, #child name
#     dob_add_birth1 = p01mb2, #child DOB
#     birth_04_add_birth1 = p01db2, # child born before 2004
#     gender_id_add_birth1 = p01mb3, #child gender identity
#     gender_self_id_add_birth1 = p01mb3_8_text, #self id gender
#     white_add_birth1 = p01mb4_1,#race - white
#     black_add_birth1 = p01mb4_2,#race - black
#     amer_ind_add_birth1 = p01mb4_3,#race - american indian
#     asian_indian_add_birth1 = p01mb4_4,#race - asian indian
#     other_asian_all_add_birth1 = p01mb4_5,#race - other asian
#     chinese_add_birth1 = p01mb4_6,#race - chinese
#     filipino_add_birth1 = p01mb4_7,#race - filipino
#     japanese_add_birth1 = p01mb4_8,#race - japanese
#     korean_add_birth1 = p01mb4_9,#race - korean
#     vietnamese_add_birth1 = p01mb4_10,#race - vietnamese
#     other_asian_add_birth1 = p01mb4_11,#race - other
#     native_hawaii_all_add_birth1 = p01mb4_12,#race - native hawaiian/other pacific islander
#     native_hawaii_add_birth1 = p01mb4_13,#race - native hawaiian
#     samoan_add_birth1 = p01mb4_14,#race - guamanian
#     guamanian_add_birth1 = p01mb4_15,#race - samoan
#     other_pacific_add_birth1 = p01mb4_16,#race - other - entry
#     race_dontknow_add_birth1 = p01mb4_17,#race - don't know
#     race_other_add_birth1 = p01mb4_18,#race - other race - entry
#     race_decline_add_birth1 = p01mb4_77,#race - decline to answer
#     race_tribe_text_add_birth1 = p01mb4_3_text, #race - tribe text
#     race_pacific_text_add_birth1 = p01mb4_16_text, #race - other race
#     race_other_text_add_birth1 = p01mb4_18_text,#race - other race text
#     latino_add_birth1 = p01mb5, #child latino
#     mexican_add_birth1 = p01mb55_1,#mexican
#     puerto_rican_add_birth1 = p01mb55_2,#puerto rican
#     cuban_add_birth1 = p01mb55_3,#cuban
#     latino_other_add_birth1 = p01mb55_4,#other
#     latino_dontknow_add_birth1 = p01mb55_5,#don't know
#     latino_decline_add_birth1 = p01mb55_77, #declined
#     latino_other_text_add_birth1 = p01mb55_4_text, #other text
#     custody_add_birth1 = p01mb6, #who currently has custody
#     custody_other_rel_add_birth1 = p01mb6_7_text, #other relative specificed text
#     name_add_birth2 = p01mb1b,# child's name
#     dob_add_birth2 = p01mb2b, #child DOB
#     birth_04_add_birth2 = p01db3, # child born before 2004
#     gender_id_add_birth2 = p01mb3b, #child gender identity
#     gender_self_id_add_birth2 = p01mb3b_8_text, #self id gender
#     white_add_birth2 = p01mb4b_1, #race - white
#     black_add_birth2 = p01mb4b_2, #race - black
#     amer_ind_add_birth2 = p01mb4b_3, #race - american indian
#     asian_indian_add_birth2 = p01mb4b_4, #race - asian indian
#     other_asian_all_add_birth2 = p01mb4b_5, #race - other asian
#     chinese_add_birth2 = p01mb4b_6, #race - chinese
#     filipino_add_birth2 = p01mb4b_7, #race - filipino
#     japanese_add_birth2 = p01mb4b_8, #race - japanese
#     korean_add_birth2 = p01mb4b_9, #race -  korean
#     vietnamese_add_birth2 = p01mb4b_10, #race - vietnamese
#     other_asian_add_birth2 = p01mb4b_11, #race - other
#     native_hawaii_all_add_birth2 = p01mb4b_12, #race - native hawaiian/other pacific islander
#     native_hawaii_add_birth2 = p01mb4b_13, #race - native hawaiian
#     samoan_add_birth2 = p01mb4b_14, #race - guamanian
#     guamanian_add_birth2 = p01mb4b_15, #race - somoan
#     other_pacific_add_birth2 = p01mb4b_16, #race - other - entry
#     race_dontknow_add_birth2 = p01mb4b_17, #race - don't know
#     race_other_add_birth2 = p01mb4b_18, #race - other race - entry
#     race_decline_add_birth2 = p01mb4b_77, #race - decline to answer
#     race_tribe_text_add_birth2 = p01mb4b_3_text, #race - tribe text
#     race_pacific_text_add_birth2 = p01mb4b_16_text,#race - other race
#     race_other_text_add_birth2 = p01mb4b_18_text, #race - other race text
#     latino_add_birth2 = p01mb5b, #child latino
#     mexican_add_birth2 = p01mb5bb_1, # mexican
#     puerto_rican_add_birth2 = p01mb5bb_2, # puerto rican
#     cuban_add_birth2 = p01mb5bb_3, # cuban
#     latino_other_add_birth2 = p01mb5bb_4, # other
#     latino_dontknow_add_birth2 = p01mb5bb_5, # don't know
#     latino_decline_add_birth2 = p01mb5bb_77,#declined
#     latino_other_text_add_birth2 = p01mb5bb_4_text, #other text
#     custody_add_birth2 = p01mb6b, #who currently has custody
#     custody_other_rel_add_birth2 = p01mb6b_7_text, #other relative specificed text
#     name_add_birth3 = p01mb1c, #child name
#     dob_add_birth3 = p01mb2c, #child DOB
#     birth_04_add_birth3 = p01db4, # child born before 2004
#     gender_id_add_birth3 = p01mb3c, #child gender identity
#     gender_self_id_add_birth3 = p01mb3c_8_text, #self id gender
#     white_add_birth3 = p01mb4c_1, #race - white
#     black_add_birth3 = p01mb4c_2, #race - black
#     amer_ind_add_birth3 = p01mb4c_3, #race - american indian
#     asian_indian_add_birth3 = p01mb4c_4, #race - asian indian
#     other_asian_all_add_birth3 = p01mb4c_5, #race - other asian
#     chinese_add_birth3 = p01mb4c_6, #race - chinese
#     filipino_add_birth3 = p01mb4c_7, #race - filipino
#     japanese_add_birth3 = p01mb4c_8, #race - japanese
#     korean_add_birth3 = p01mb4c_9, #race - korean
#     vietnamese_add_birth3 = p01mb4c_10, #race - vietnamese
#     other_asian_add_birth3 = p01mb4c_11, #race - other
#     native_hawaii_all_add_birth3 = p01mb4c_12, #race - native hawaiian/other pacific islander
#     native_hawaii_add_birth3 = p01mb4c_13, #race - native hawaiian
#     samoan_add_birth3 = p01mb4c_14, #race - guamanian
#     guamanian_add_birth3 = p01mb4c_15, #race - somoan
#     other_pacific_add_birth3 = p01mb4c_16, #race - other - entry
#     race_dontknow_add_birth3 = p01mb4c_17, #race - don't know
#     race_other_add_birth3 = p01mb4c_18, #race - other race - entry
#     race_decline_add_birth3 = p01mb4c_77, #race - decline to answer
#     race_tribe_text_add_birth3 = p01mb4c_3_text,# race - tribe text
#     race_pacific_text_add_birth3 = p01mb4c_16_text, #race - other race
#     race_other_text_add_birth3 = p01mb4c_18_text,#race - other race text
#     latino_add_birth3 = p01mb5c, #child latino
#     mexican_add_birth3 = p01mb5cc_1,# mexican
#     puerto_rican_add_birth3 = p01mb5cc_2,# puerto ricna
#     cuban_add_birth3 = p01mb5cc_3,# cuban
#     latino_other_add_birth3 = p01mb5cc_4,# other
#     latino_dontknow_add_birth3 = p01mb5cc_5,# don't know
#     latino_decline_add_birth3 = p01mb5cc_77, #declined
#     latino_other_text_add_birth3 = p01mb5cc_4_text, #other text
#     custody_add_birth3 = p01mb6c, #who currently has custody
#     custody_other_rel_add_birth3 = p01mb6c_7_text,# other relative specified text
#    drug_preg = p01l, #drugs during pregnancy
#     drug_tri1 = p01n_1, #first trimester drugs
#    drug_tri2= p01n_2, #second trimester drugs
#    drug_tri3 = p01n_3,#third trimester drugs
#     drug_tri_decline = p01n_77, #declined trimester drugs
#     days_drug_preg = p01m, #days using drugs during pregnancy
#     alc_preg = p01q, #alcohol during pregnancy
#     alc_tri1 = p01s_1, #first trimester alcohol
#     alc_tri2 = p01s_2,#second trimester alcohol
#     alc_tri3 = p01s_3,#third trimester alcohol
#     alc_tri_decline = p01s_77, #declined trimester alcohol
#     days_alc_preg = p01r, #days alcohol during pregnancy
#     tob_preg = p01v, #did you use use nicotine during pregnancy
#     tob_tri1 = p01x_1, #first trimester nicotine
#     tob_tri2 = p01x_2,#second trimester nicotine
#     tob_tri3 = p01x_3, #third trimester nicotine
#     tob_tri_decline = p01x_77, #declined trimester nicotine
#     days_tob_preg = p01w #how many days nicotine during pregnancy
#   ) %>%
#   rename_with(
#     .fn = ~str_replace(
#       .,
#       '^',
#       'bc1_'
#     ),
#     .cols = everything()
#   )


# baby_fun <- function(
#     dataset,
#     bio_child = 'bc1_preg_result'
# ){
#
#   library(magrittr)
#   library(dplyr)
#
#   if(dataset$bio_child == 1){
#     df1 <- {{dataset}} %>%
#       select(
#         tc_age_preg:preg_result
#       )
#   }
#
#   else if(dataset$bio_child == 2){
#     df2 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         tc_age_preg,
#         tri_mis_abort,
#         drugs_preg:days_tob_preg
#         )
#   }
#
#   else if(dataset$bio_child == 3){
#     df3 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         tc_age_preg,
#         tri_mis_abort,
#         drugs_preg:days_tob_preg
#         )
#   }
#
#   else if(dataset$bio_child == 4){
#     df4 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         tri_still_preg,
#         name:custody_other_rel,
#         drugs_preg:days_tob_preg
#       )
#   }
#
#   else if(dataset$bio_child == 5){
#     df5 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         num_kid_twin:days_tob_preg
#       )
#   }
#
#   else if(dataset$bio_child == 6){
#     df6 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         drugs_preg:days_tob_preg
#       )
#   }
#
#   else if(dataset$bio_child == 7){
#     df7 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         drugs_preg:days_tob_preg
#       )
#   }
#
#   else if(dataset$bio_child == 8){
#     df8 <- {{dataset}} %>%
#       select(
#         id:preg_result,
#         drugs_preg:days_tob_preg
#       )
#   }
#
#   return(
#     list(df1, df2, df3, df4,
#          df5, df6, df7, df8)
#   )
# }
