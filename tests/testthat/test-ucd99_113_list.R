test_that("UCD 113 list", {
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Census Region",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = "1-4",
    period = 1999:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("Monday"),
    autopsy = c("Yes"),
    place_of_death = c("Medical Facility - Inpatient"),
    gender = c("All"),
    hispanic_origin = "Hispanic or Latino",
    race = "Black or African American",
    ucd_option = "ICD-10 113 Cause List",
    ucd_cause_113 = "#Salmonella infections (A01-A02)"
  )
  Sys.sleep(10)
  expect_true(grep(
    "ICD-10 113 Cause List: #Salmonella infections \\(A01-A02\\)",
    df$Notes
  ) > 0)
  expect_true(grep("Autopsy: Yes", df$Notes) > 0)
  expect_true(grep("Weekday: Monday", df$Notes) > 0)
  expect_true(grep(
    "Place of Death: Medical Facility - Inpatient",
    df$Notes) > 0)
})
