test_that("test that provisional MCOD results match data from WONDER website", {
  wonder_url_mcod18 <- session_mcd_final18(I_Agree = TRUE)
  df <- suppressWarnings(mcd_final18(
    wonder_url = wonder_url_mcod18,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = TRUE,
    show_standard_error = TRUE,
    age = c(
      "< 1", "5-14", "35-44",
      "75-84", "85+"
    ),
    period = 2018:2021,
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
    race = c("Black or African American", "White"),
    ucd_option = "ICD-10 113 Cause List",
    ucd_cause_113 = "#COVID-19 (U07.1)",
    mcd_option = "MCD - ICD-10 113 Cause List",
    mcd_cause_113 = "#Diabetes mellitus (E10-E14)",
    mcd_cause_113_and = "All"
  ))
  original <- read.csv("../mcd_final18_example/mcd_final18_wonder.txt", sep = "\t")
  # Query parameter order can vary so only test the Deaths, Crude Rate, etc
  # There's also a timestamp
  # Can only test up to 2020 since 2021 and 2022 data are provisional
  # and may change in the future
  expect_identical(df[1:4, ],
                   original[1:4, ])
  # check that the included states are the same
  expect_identical(df[16:22, ], original[16:22, ])
  Sys.sleep(30)
})
