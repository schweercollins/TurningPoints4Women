# creating a new dataframe

bsi
cesd
dast
bcap
dyadc
ciq
cts

data <-
  left_join(
  bsi,
  cesd,
  by = 'id'
  )


data <-
left_join(
    data,
    dast,
    by = 'id'
  )

data <-
  left_join(
    data,
    bcap,
    by = 'id'
  )

data <-
  left_join(
    data,
    dyadc,
    by = 'id'
  )

data <-
  left_join(
    data,
    cts,
    by = 'id'
  )

data <-
  left_join(
    data,
    ciq,
    by = 'id'
  )

data <- data %>%
  distinct(id, .keep_all = TRUE)

write.csv(data,
          'data_77included.csv')

bsi_complete <- bsi_complete %>%
  relocate(id, .before = bsi1)

bsi_complete %>%
  distinct(id, .keep_all = TRUE)

cesd_complete %>%
  distinct(id, .keep_all = TRUE)

dast_complete %>%
  distinct(id, .keep_all = TRUE)

bcap_complete %>%
  distinct(id, .keep_all = TRUE)

dyadc_complete %>%
  distinct(id, .keep_all = TRUE)

cts_complete %>%
  distinct(id, .keep_all = TRUE)

ciq_complete %>%
  distinct(id, .keep_all = TRUE)

glimpse(bsi_complete)
glimpse(cesd_complete)
glimpse(ciq_complete)
glimpse(bcap_complete)
glimpse(cts_complete)
glimpse(dyadc_complete)
glimpse(dast_complete)


data_complete <-
  left_join(
    bsi_complete,
    cesd_complete,
    by = 'id'
    ) %>%
  relocate(id, .before = bsi1)

data_complete <-
  left_join(
    data_complete,
    ciq_complete,
    by = 'id'
  )

data_complete <-
  left_join(
    data_complete,
    bcap_complete,
    by = 'id'
    )

data_complete <-
  left_join(
    data_complete,
    cts_complete,
    by = 'id'
    )

data_complete <-
  left_join(
    data_complete,
    dyadc_complete,
    by = 'id'
  )

data_complete <-
  left_join(
    data_complete,
    dast_complete,
    by = 'id'
  )

data_complete <-
  data_complete %>%
  distinct(id, .keep_all = TRUE)

write.csv(data_complete,
          'data_calculated.csv')
