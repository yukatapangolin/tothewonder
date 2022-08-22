test_that("UCD ICD-10 Codes test", {
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = "All Ages",
    period = 1999:2020,
    residence_urbanization_year = "2006",
    residence_urbanization = "Large Central Metro",
    residence_fips = "All",
    weekday = c("All"),
    autopsy = c("All"),
    place_of_death = c("All"),
    gender = c("All"),
    hispanic_origin = "All",
    race = "All",
    ucd_option = "ICD-10 Codes",
    ucd_icd_codes = c("U01", "U07.1") # Terrorism Homicides and COVID
  )
  Sys.sleep(10)
  expect_true(grep("2006 Urbanization: Large Central Metro", df$Notes) > 0)
  expect_true(any(grepl("U01", df$Notes) == TRUE))
  expect_true(grep("U07.1", df$Notes) > 0)
})
