## code to prepare `DATASET` dataset goes here

local({
  source("data-raw/DATASET_ucd.R", local = TRUE)
  source("data-raw/DATASET_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_mcod_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_d_a.R", local = TRUE)
  source("data-raw/DATASET_ucd_forms.R", local = TRUE)
  source("data-raw/DATASET_mcd_forms.R", local = TRUE)
  source("data-raw/DATASET_ages.R", local = TRUE)
  source("data-raw/DATASET_113.R", local = TRUE)
  source("data-raw/DATASET_mcd.R", local = TRUE)
  source("data-raw/DATASET_FIPS.R", local = TRUE)
  # read FIPS as character because of leading zero's
  counties <- read.csv("data-raw/counties.txt",
    sep = "\t",
    colClasses = c("character")
  )
  counties_fips <- counties$County.Code[counties$County.Code != ""]
  states <- read.csv("data-raw/states.txt",
    sep = "\t",
    colClasses = c("character")
  )
  states_fips <- states$State.Code[states$State.Code != ""]

  usethis::use_data(states_fips,
    counties_fips,
    state_FIPS,
    FIPS_mcd,
    simple_mcd_form,
    simple_ucd_form,
    icd10_top_items_ucd,
    icd10_top_items_mcd,
    ucd_form_options,
    mcd_form_options,
    ucd_download_form,
    mcd_download_form,
    gender_key,
    group_by_1_key,
    group_by_2_key,
    hispanic_origin_key,
    injury_intent_key,
    injury_mechanism_key,
    race_key,
    urbanization_key,
    weekday_key,
    autopsy_key,
    place_of_death_key,
    drug_alcohol_key,
    ucd_key,
    icd_codes_key,
    mcod_icd_codes_key,
    group_by_1_opts,
    group_by_2_opts,
    urbanization_opts,
    weekday_opts,
    autopsy_opts,
    place_of_death_opts,
    gender_opts,
    hispanic_origin_opts,
    race_opts,
    injury_intent_opts,
    injury_mechanism_opts,
    drug_alcohol_opts,
    ucd_opts,
    all_age_groups_opts,
    ten_year_age_groups_opts,
    five_year_age_groups_opts,
    single_year_ages_opts,
    infant_age_groups_opts,
    ten_year_age_groups_key,
    five_year_age_groups_key,
    single_year_ages_key,
    infant_age_groups_key,
    icd10_113_list_opts,
    icd10_113_list_key,
    mcd_ucd_key,
    mcd_ucd_opts,
    mcd_group_by_1_key,
    mcd_group_by_2_key,
    mcd_group_by_1_opts,
    mcd_group_by_2_opts,
    single_race_6_key,
    single_race_6_opts,
    single_race_15_key,
    single_race_15_opts,
    single_multi_race_31_key,
    single_multi_race_31_opts,
    race_options_opts,
    race_18_opts,
    mcd_mcd_opts,
    mcd_mcd_key,
    period_key,
    period_opts,
    mcd_save_form,
    internal = TRUE,
    compress    = "xz",
    version = 3,
    overwrite = TRUE
  )
})
