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


test_that("test that provisional MCOD results match data from WONDER website", {
  wonder_url_mcod18 <- session_mcd_provisional(I_Agree = TRUE)
  df <- suppressWarnings(mcd_provisional(
    wonder_url = wonder_url_mcod18,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c(
      "1-4", "5-14", "15-24", "25-34", "35-44",
      "45-54", "55-64", "65-74"
    ),
    period = 2018:2022,
    period_option = "Year",
    residence_fips = c(
      "02", "04", "05", "06", "08",
      "09", "10", "11", "12",
      "13", "15", "16", "17", "18", "19",
      "20", "21", "22", "23", "24",
      "25", "26", "27", "28", "29", "30",
      "31", "32", "33", "34", "35",
      "36", "37", "38", "39", "40", "41",
      "42", "44", "45", "46", "47",
      "48", "49", "50", "51", "53", "54", "55", "56"
    ),
    residence_urbanization_year = c("2013"),
    residence_urbanization = c(
      "Large Central Metro",
      "Large Fringe Metro",
      "Medium Metro",
      "Small Metro",
      "Micropolitan (Nonmetro)"
    ),
    occurrence_fips = c(
      "02", "04", "05", "06", "08",
      "09", "10", "11", "12",
      "13", "15", "16", "17", "18", "19",
      "20", "21", "22", "23", "24",
      "25", "26", "27", "28", "29", "30",
      "31", "32", "33", "34", "35",
      "36", "37", "38", "39", "40", "41",
      "42", "44", "45", "46", "47",
      "48", "49", "50", "51", "53", "54", "55", "56"
    ),
    occurrence_urbanization_year = c("2013"),
    occurrence_urbanization = c(
      "Large Central Metro",
      "Large Fringe Metro",
      "Medium Metro",
      "Small Metro",
      "Micropolitan (Nonmetro)"
    ),
    autopsy = c("Yes", "Unknown"),
    place_of_death = c(
      "Medical Facility - Inpatient",
      "Medical Facility - Outpatient or ER",
      "Medical Facility - Dead on Arrival",
      "Medical Facility - Status unknown",
      "Decedent's home"
    ),
    gender = c("Female", "Male"),
    hispanic_origin = "Not Hispanic or Latino",
    race_option = "Single Race 6",
    race = c("Black or African American"),
    ucd_option = "ICD-10 113 Cause List",
    ucd_cause_113 = "#COVID-19 (U07.1)",
    mcd_option = "MCD - ICD-10 113 Cause List",
    mcd_cause_113 = "#Diabetes mellitus (E10-E14)",
    mcd_cause_113_and = "All"
  ))
  original <- read.csv("../mcd_example/mcd_wonder.txt", sep = "\t")
  # Query parameter order can vary so only test the Deaths, Crude Rate, etc
  # There's also a timestamp
  # Can only test up to 2020 since 2021 and 2022 data are provisional
  # and may change in the future
  expect_identical(df[1:3, ], original[1:3, ])
  # check that the included states are the same
  expect_identical(df[12:16, ], original[12:16, ])
  Sys.sleep(30)
})
