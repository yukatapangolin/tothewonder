test_that("ucd data matches website download", {
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c(
      "1-4", "5-14", "15-24", "25-34", "35-44",
      "45-54", "55-64", "65-74", "75-84"
    ),
    period = 1999:2015,
    residence_urbanization_year = "2013",
    residence_urbanization = c(
      "Large Central Metro",
      "Large Fringe Metro",
      "Medium Metro",
      "Small Metro",
      "Micropolitan (Nonmetro)"
    ),
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
    weekday = c(
      "Sunday", "Monday", "Tuesday", "Wednesday",
      "Thursday", "Friday"
    ),
    autopsy = c("No", "Yes"),
    place_of_death = c("All Places"),
    gender = c("Male", "Female"),
    hispanic_origin = c("Hispanic or Latino", "Not Hispanic or Latino"),
    race = c("White", "Black or African American", "Asian or Pacific Islander"),
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = c(
      "Unintentional", "Suicide",
      "Undetermined", "Legal Intervention / Operations of War"
    ),
    ucd_injury_mechanism = "All Causes of Death"
  )
  original <- read.csv("../ucd99_example/ucd_wonder.txt", sep = "\t")
  # Query parameter order can vary so only test the Deaths, Crude Rate, etc
  # There's also a timestamp
  expect_identical(df[1:18, 1:6], original[1:18, 1:6])
  Sys.sleep(30)
})
