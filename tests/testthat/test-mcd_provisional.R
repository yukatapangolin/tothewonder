test_that("provisional multiple cause of death function test", {
  wonder_url_mcod18 <- session_mcd_provisional(I_Agree = TRUE)
  df <- mcd_provisional(
    wonder_url = wonder_url_mcod18,
    group_by_1 = "MMWR Week",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = 15:25,
    period_option = "MMWR",
    period = 2019:2022,
    residence_fips = "All",
    residence_urbanization_year = c("2013"),
    residence_urbanization = "All Categories",
    occurrence_fips = "All",
    occurrence_urbanization_year = c("2013"),
    occurrence_urbanization = "All Categories",
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All Genders"),
    hispanic_origin = "Not Hispanic or Latino",
    race_option = "Single Race 6",
    race = c("Black or African American"),
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "Homicide",
    ucd_injury_mechanism = "Firearm",
    mcd_option = "MCD - ICD-10 Codes",
    mcd_icd_codes = "All",
    mcd_icd_codes_and = "All"
  )
  Sys.sleep(20)
  expect_true(grep("Single Race 6: Black or African American", df$Notes) > 0)
  expect_true(grep("Hispanic Origin: Not Hispanic or Latino", df$Notes) > 0)
  expect_true(grep("UCD - Injury Intent: Homicide", df$Notes) > 0)
  expect_true(grep("Group By: MMWR Week", df$Notes) > 0)
  expect_true(grep(paste0("Single-Year Ages: 15 years; 16 years; 17 years;",
                         " 18 years; 19 years; 20 years; 21 years; 22 years;",
                         " 23 years; 24 years; 25 years"), df$Notes) > 0)

  df <- mcd_provisional(
    wonder_url = wonder_url_mcod18,
    save = FALSE,
    group_by_1 = "Month",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = "All",
    period_option = "Year",
    period = 2019:2022,
    residence_fips = "All",
    residence_urbanization_year = c("2006"),
    residence_urbanization = "All Categories",
    occurrence_fips = "All",
    occurrence_urbanization_year = c("2006"),
    occurrence_urbanization = "All Categories",
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All Genders"),
    hispanic_origin = "Not Hispanic or Latino",
    race_option = "Single Race 6",
    race = c("Black or African American"),
    ucd_option = "Drug/Alcohol Induced Causes",
    ucd_drug_alcohol = "Alcohol-induced causes",
    mcd_option = "MCD - Drug/Alcohol Induced Causes",
    mcd_drug_alcohol = "Drug poisonings (overdose) Unintentional (X40-X44)",
    mcd_drug_alcohol_and = "All other alcohol-induced causes"
  )
  Sys.sleep(20)
  expect_true(grep("UCD - Drug/Alcohol Induced Causes: Alcohol-induced causes",
                   df$Notes) > 0)
  expect_true(grep(
    paste0("MCD - Drug/Alcohol Induced Causes: Drug poisonings",
          " \\(overdose\\) Unintentional \\(X40-X44\\)"), df$Notes) > 0)
  expect_true(grep(
    "MCD - Drug/Alcohol Induced Causes: All other alcohol-induced causes",
    df$Notes) > 0)
  expect_true(grep("Single Race 6: Black or African American", df$Notes) > 0)
  expect_true(grep("Hispanic Origin: Not Hispanic or Latino", df$Notes) > 0)
  expect_true(grep("Group By: Month", df$Notes) > 0)
})
