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
    "54",
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
    ## Check that the CDC data has all the counties we requested
    (\() {
      for (FIPS in test_counties) {
        ## District of Columbia, 11001 FIPS Code, is a special case
        ## The CDC uses the state code "11"
        if (FIPS == "11001") {
          FIPS <- "\\(11\\)"
          if (!any(grepl(paste0("(", FIPS, ")"), df$Notes))) {
            return(FALSE)
          }
        }
      }
      ## Check that no extra counties were included
      for (FIPS in setdiff(tothewonder:::COUNTIES_FIPS, test_counties)) {
        if (any(grepl(paste0("(", FIPS, ")"), df$Notes))) {
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
  ), "period must be greater or equal to 1999")
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
    paste0("weekday: you can't have both 'All' and other options selected. ",
           "Please select either 'All' or a specific option.")
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
    paste0("race: you can't have both 'All' and other ",
           "options selected. Please select either 'All' or a specific option.")
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
