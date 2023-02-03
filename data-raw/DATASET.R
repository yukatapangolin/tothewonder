## code to prepare `DATASET` dataset goes here

local({
  source("data-raw/DATASET_ucd.R", local = TRUE)
  source("data-raw/DATASET_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_mcod_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_d_a.R", local = TRUE)
  source("data-raw/DATASET_mcd_final18_forms.R", local = TRUE)
  source("data-raw/DATASET_mcd_final18.R", local = TRUE)
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
  COUNTIES_FIPS <- counties$County.Code[counties$County.Code != ""]
  states <- read.csv("data-raw/states.txt",
    sep = "\t",
    colClasses = c("character")
  )
  STATES_FIPS <- states$State.Code[states$State.Code != ""]

  usethis::use_data(
    STATES_FIPS,
    COUNTIES_FIPS,
    STATE_FIPS,
    FIPS_MCD,
    SIMPLE_MCD_FORM,
    SIMPLE_UCD_FORM,
    ICD10_TOP_ITEMS_UCD,
    ICD10_TOP_ITEMS_MCD,
    UCD_FORM_OPTIONS,
    MCD_FORM_OPTIONS,
    UCD_DOWNLOAD_FORM,
    MCD_DOWNLOAD_FORM,
    GENDER_KEY,
    GROUP_BY_1_KEY,
    GROUP_BY_2_KEY,
    HISPANIC_ORIGIN_KEY,
    INJURY_INTENT_KEY,
    INJURY_MECHANISM_KEY,
    RACE_KEY,
    URBANIZATION_KEY,
    WEEKDAY_KEY,
    AUTOPSY_KEY,
    PLACE_OF_DEATH_KEY,
    DRUG_ALCOHOL_KEY,
    UCD_KEY,
    ICD_CODES_KEY,
    MCOD_ICD_CODES_KEY,
    GROUP_BY_1_OPTS,
    GROUP_BY_2_OPTS,
    URBANIZATION_OPTS,
    WEEKDAY_OPTS,
    AUTOPSY_OPTS,
    PLACE_OF_DEATH_OPTS,
    GENDER_OPTS,
    HISPANIC_ORIGIN_OPTS,
    RACE_OPTS,
    INJURY_INTENT_OPTS,
    INJURY_MECHANISM_OPTS,
    DRUG_ALCOHOL_OPTS,
    UCD_OPTS,
    ALL_AGE_GROUPS_OPTS,
    TEN_YEAR_AGE_GROUPS_OPTS,
    FIVE_YEAR_AGE_GROUPS_OPTS,
    SINGLE_YEAR_AGES_OPTS,
    INFANT_AGE_GROUPS_OPTS,
    TEN_YEAR_AGE_GROUPS_KEY,
    FIVE_YEAR_AGE_GROUPS_KEY,
    SINGLE_YEAR_AGES_KEY,
    INFANT_AGE_GROUPS_KEY,
    ICD10_113_LIST_OPTS,
    ICD10_113_LIST_KEY,
    MCD_UCD_KEY,
    MCD_UCD_OPTS,
    MCD_GROUP_BY_1_KEY,
    MCD_GROUP_BY_2_KEY,
    MCD_GROUP_BY_1_OPTS,
    MCD_GROUP_BY_2_OPTS,
    SINGLE_RACE_6_KEY,
    SINGLE_RACE_6_OPTS,
    SINGLE_RACE_15_KEY,
    SINGLE_RACE_15_OPTS,
    SINGLE_MULTI_RACE_31_KEY,
    SINGLE_MULTI_RACE_31_OPTS,
    RACE_OPTIONS_OPTS,
    RACE_18_OPTS,
    MCD_MCD_OPTS,
    MCD_MCD_KEY,
    PERIOD_KEY,
    PERIOD_OPTS,
    #MCD_SAVE_FORM,
    SIMPLE_FINAL18_MCD_FORM,
    MCD_FINAL18_UCD_KEY,
    MCD_FINAL18_UCD_OPTS,
    MCD_FINAL18_MCD_KEY,
    MCD_FINAL18_MCD_OPTS,
    MCD_FINAL18_GROUP_BY_1_KEY,
    MCD_FINAL18_GROUP_BY_1_OPTS,
    MCD_FINAL18_GROUP_BY_2_KEY,
    MCD_FINAL18_GROUP_BY_2_OPTS,
    MCD_FINAL18_DOWNLOAD_FORM,
    MCD_FINAL18_FORM_OPTIONS,
    internal = TRUE,
    compress    = "xz",
    version = 3,
    overwrite = TRUE
  )
})
