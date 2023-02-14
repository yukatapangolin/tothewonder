## code to prepare `DATASET` dataset goes here

local({
  source("data-raw/DATASET_ucd.R", local = TRUE)
  source("data-raw/DATASET_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_mcod_final18_icd_codes.R", local = TRUE)
  source("data-raw/DATASET_mcod_provisional_icd_codes.R", local = TRUE)
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
    #MCD_SAVE_FORM,
    ALL_AGE_GROUPS_OPTS,
    AUTOPSY_KEY,
    AUTOPSY_OPTS,
    COUNTIES_FIPS,
    DRUG_ALCOHOL_KEY,
    DRUG_ALCOHOL_OPTS,
    FIPS_MCD,
    FIVE_YEAR_AGE_GROUPS_KEY,
    FIVE_YEAR_AGE_GROUPS_OPTS,
    GENDER_KEY,
    GENDER_OPTS,
    GROUP_BY_1_KEY,
    GROUP_BY_1_OPTS,
    GROUP_BY_2_KEY,
    GROUP_BY_2_OPTS,
    HISPANIC_ORIGIN_KEY,
    HISPANIC_ORIGIN_OPTS,
    ICD10_113_LIST_KEY,
    ICD10_113_LIST_OPTS,
    ICD10_TOP_ITEMS_UCD,
    ICD_CODES_KEY,
    INFANT_AGE_GROUPS_KEY,
    INFANT_AGE_GROUPS_OPTS,
    INJURY_INTENT_KEY,
    INJURY_INTENT_OPTS,
    INJURY_MECHANISM_KEY,
    INJURY_MECHANISM_OPTS,
    MCD_DOWNLOAD_FORM,
    MCD_FINAL18_DOWNLOAD_FORM,
    MCD_FINAL18_FORM_OPTIONS,
    MCD_FINAL18_GROUP_BY_1_KEY,
    MCD_FINAL18_GROUP_BY_1_OPTS,
    MCD_FINAL18_GROUP_BY_2_KEY,
    MCD_FINAL18_GROUP_BY_2_OPTS,
    MCD_FINAL18_MCD_KEY,
    MCD_FINAL18_MCD_OPTS,
    MCD_FINAL18_UCD_KEY,
    MCD_FINAL18_UCD_OPTS,
    MCD_FORM_OPTIONS,
    MCD_GROUP_BY_1_KEY,
    MCD_GROUP_BY_1_OPTS,
    MCD_GROUP_BY_2_KEY,
    MCD_GROUP_BY_2_OPTS,
    MCD_MCD_KEY,
    MCD_MCD_OPTS,
    MCD_UCD_KEY,
    MCD_UCD_OPTS,
    PERIOD_KEY,
    PERIOD_OPTS,
    PLACE_OF_DEATH_KEY,
    PLACE_OF_DEATH_OPTS,
    RACE_18_OPTS,
    RACE_KEY,
    RACE_OPTIONS_OPTS,
    RACE_OPTS,
    SIMPLE_FINAL18_MCD_FORM,
    SIMPLE_MCD_FORM,
    SIMPLE_UCD_FORM,
    SINGLE_MULTI_RACE_31_KEY,
    SINGLE_MULTI_RACE_31_OPTS,
    SINGLE_RACE_15_KEY,
    SINGLE_RACE_15_OPTS,
    SINGLE_RACE_6_KEY,
    SINGLE_RACE_6_OPTS,
    SINGLE_YEAR_AGES_KEY,
    SINGLE_YEAR_AGES_OPTS,
    STATE_FIPS,
    TEN_YEAR_AGE_GROUPS_KEY,
    TEN_YEAR_AGE_GROUPS_OPTS,
    UCD_DOWNLOAD_FORM,
    UCD_FORM_OPTIONS,
    UCD_KEY,
    UCD_OPTS,
    URBANIZATION_KEY,
    URBANIZATION_OPTS,
    WEEKDAY_KEY,
    WEEKDAY_OPTS,
    STATES_FIPS,
    UCD_ICD10_TOP_ITEMS_MCD_FINAL18,
    MCD_ICD10_TOP_ITEMS_MCD_FINAL18,
    MCD_ICD10_TOP_ITEMS_MCD_PROVISIONAL,
    UCD_ICD10_TOP_ITEMS_MCD_PROVISIONAL,
    EXTRA_MCD_PROVISIONAL_UCD_ICD_CODES_KEY,
    EXTRA_MCD_PROVISIONAL_MCD_ICD_CODES_KEY,
    MCD_ICD_CODES_KEY,
    MCD_PROVISIONAL_ICD10_113_LIST_OPTS,
    MCD_PROVISIONAL_ICD10_113_LIST_KEY,
    MCD_PROVISIONAL_DRUG_ALCOHOL_KEY,
    MCD_PROVISIONAL_DRUG_ALCOHOL_OPTS,
    internal = TRUE,
    compress    = "xz",
    version = 3,
    overwrite = TRUE
  )
})

local({
  source("data-raw/DATASET_HHS_CR.R", local = TRUE)
  source("data-raw/DATASET_FIPS.R", local = TRUE)
  usethis::use_data(
    US_STATES_FIPS,
    HHS1,
    HHS2,
    HHS3,
    HHS4,
    HHS5,
    HHS6,
    HHS7,
    HHS8,
    HHS9,
    HHS10,
    CENS_D1,
    CENS_D2,
    CENS_D3,
    CENS_D4,
    CENS_D5,
    CENS_D6,
    CENS_D7,
    CENS_D8,
    CENS_D9,
    CENS_R1,
    CENS_R2,
    CENS_R3,
    CENS_R4,
    compress    = "xz",
    version = 3,
    overwrite = TRUE)
})
