test_that("ucd99_drug_alcohol", {
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = as.character(c(15, 20)),
    period = 2015:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("Male"),
    hispanic_origin = "All",
    race = "Asian or Pacific Islander",
    ucd_option = "Drug/Alcohol Induced Causes",
    ucd_drug_alcohol = "Drug-induced causes"
  )
  Sys.sleep(10)
  expect_true(grep("Gender: Male", df$Notes) > 0)
  expect_true(grep("Race: Asian or Pacific Islander", df$Notes) > 0)
  expect_true(grep(
    "Drug/Alcohol Induced Causes: Drug-induced causes",
    df$Notes
  ) > 0)
  expect_true(grep("Single-Year Ages: 15 years; 20 years", df$Notes) > 0)
  expect_true(grep(
    "Year/Month: 2015; 2016; 2017; 2018; 2019; 2020",
    df$Notes
  ) > 0)
})
