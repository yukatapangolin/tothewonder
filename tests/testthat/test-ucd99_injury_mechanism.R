test_that("UCD Injury Intent and Injury Mechanism", {
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "Month",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("5-14", "15-24"),
    period = 1999:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All"),
    hispanic_origin = "Not Hispanic or Latino",
    race = "Black or African American",
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "Homicide",
    ucd_injury_mechanism = "All Causes of Death"
  )
  Sys.sleep(10)
  expect_true(grep("Race: Black or African American", df$Notes) > 0)
  expect_true(grep("Hispanic Origin: Not Hispanic or Latino", df$Notes) > 0)
  expect_true(grep(
    "Ten-Year Age Groups: 5-14 years; 15-24 years",
    df$Notes
  ) > 0)

  test_counties <- c(
    "01001", "01003", "01005", "01007", "01009", "01011",
    "01013", "01015", "01017", "01019", "01021", "01023",
    "01025", "01027", "01029", "01031", "01033", "01035",
    "01037", "01039", "01041", "01043", "01045", "01047",
    "01049", "01051", "11001"
  )
  df <- ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("25-34"),
    period = 1999:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = test_counties,
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All"),
    hispanic_origin = "Not Hispanic or Latino",
    race = "White",
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "All",
    ucd_injury_mechanism = "Firearm"
  )
  Sys.sleep(10)
  expect_true({
    ## Check that the CDC data has got all the counties
    (\() {
      for (FIPS in test_counties) {
        ## District of Columbia, 11001 FIPS Code, is a special case
        ## The CDC uses only the state code "11"
        if (FIPS == "11001") {
          FIPS <- "\\(11\\)"
          if (!any(grepl(FIPS, df$Notes))) {
            return(FALSE)
          }
        }
      }
      ## Check that no extra counties were included
      for (FIPS in setdiff(tothewonder:::counties_fips, test_counties)) {
        if (any(grepl(FIPS, df$Notes))) {
          return(FALSE)
        }
      }
      return(TRUE)
    })()
  })
  expect_true(grep("Ten-Year Age Groups: 25-34 years", df$Notes) > 0)
  expect_true(grep("Race: White", df$Notes) > 0)
  expect_true(grep(
    "Injury Mechanism & All Other Leading Causes: Firearm",
    df$Notes
  ) > 0)
})


test_that("ucd99 errors", {
  ## Invalid period
  expect_error(ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("25-34"),
    period = 1998:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All"),
    hispanic_origin = "Not Hispanic or Latino",
    race = "White",
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "Homicide",
    ucd_injury_mechanism = "Firearm"
  ), "period must be greater than 1999")
  ## weekday includes All and other options
  expect_error(
    ucd99(
      wonder_url = wonder_url_ucd99_test,
      group_by_1 = "Year",
      group_by_2 = "None",
      group_by_3 = "None",
      group_by_4 = "None",
      show_confidence_interval = FALSE,
      show_standard_error = FALSE,
      age = c("25-34"),
      period = 1999:2020,
      residence_urbanization_year = "2013",
      residence_urbanization = "All Categories",
      residence_fips = "All",
      weekday = c("All", "Monday"),
      autopsy = c("All Values"),
      place_of_death = c("All Places"),
      gender = c("All"),
      hispanic_origin = "Not Hispanic or Latino",
      race = "White",
      ucd_option = "Injury Intent and Mechanism",
      ucd_injury_intent = "Homicide",
      ucd_injury_mechanism = "Firearm"
    ),
    "weekday: you can't have both 'All' and other options selected. Please select either 'All' or specific options."
  )
  ## race is invalid
  expect_error(
    ucd99(
      wonder_url = wonder_url_ucd99_test,
      group_by_1 = "Year",
      group_by_2 = "None",
      group_by_3 = "None",
      group_by_4 = "None",
      show_confidence_interval = FALSE,
      show_standard_error = FALSE,
      age = c("25-34"),
      period = 1999:2020,
      residence_urbanization_year = "2013",
      residence_urbanization = "All Categories",
      residence_fips = "All",
      weekday = c("All"),
      autopsy = c("All Values"),
      place_of_death = c("All Places"),
      gender = c("All"),
      race = c("All", "White"),
      hispanic_origin = "Not Hispanic or Latino",
      ucd_option = "Injury Intent and Mechanism",
      ucd_injury_intent = "Homicide",
      ucd_injury_mechanism = "Firearm"
    ),
    "race: you can't have both 'All' and other options selected. Please select either 'All' or specific options."
  )

  ## group_by must be unique
  expect_error(ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "Year",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("25-34"),
    period = 1999:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All"),
    hispanic_origin = "Not Hispanic or Latino",
    race = "White",
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "Homicide",
    ucd_injury_mechanism = "Firearm"
  ), "group_bys must be unique")

  ## different age groups
  expect_error(ucd99(
    wonder_url = wonder_url_ucd99_test,
    group_by_1 = "Year",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("25-34", "10-14"),
    period = 1999:2020,
    residence_urbanization_year = "2013",
    residence_urbanization = "All Categories",
    residence_fips = "All",
    weekday = c("All Weekdays"),
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All"),
    hispanic_origin = "Not Hispanic or Latino",
    race = "White",
    ucd_option = "Injury Intent and Mechanism",
    ucd_injury_intent = "Homicide",
    ucd_injury_mechanism = "Firearm"
  ), "Invalid age groups")
})

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
  original <- read.csv("../ucd_example/ucd_wonder.txt", sep = "\t")
  # Query parameter order can vary so only test the Deaths, Crude Rate, etc
  # There's also a timestamp
  expect_identical(df[1:18, 1:6], original[1:18, 1:6])
})
