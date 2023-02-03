test_that("final multiple cause of death function test", {
  ## Alaska, Arizona, Colorado, Connecticut, Delaware, Georgia, Indiana,
  ## Iowa, Kansas, Kentucky, Maine, Maryland, Massachusetts, Michigan,
  ## Minnesota, Nevada, New Hampshire, New Jersey, New Mexico, New York,
  ## North Carolina, Ohio, Oklahoma, Oregon,  Rhode Island, South Carolina,
  ## Utah, Vermont, Virginia,  West Virginia, and Wisconsin
  ## Los Angeles, Sacramento, Shasta, and Siskiyou
  test_counties_states <- c(2L, 4L, 8L, 9L, 10L, 13L, 18L, 19L, 20L,
                            21L, 23L, 24L, 25L,
                            26L, 27L, 32L, 33L, 34L, 35L, 36L, 37L, 39L,
                            40L, 41L, 44L, 45L,
                            49L, 50L, 51L, 54L, 55L, 6037, 6067, 6089, 6093)
  wonder_url_final18 <- session_mcd_final18(I_Agree = TRUE)
  df <- mcd_final18(
    wonder_url = wonder_url_final18,
    ## save = TRUE,
    group_by_1 = "Month",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("15-19", "20-24", "25-29", "30-34", "35-39", "100+"),
    period = c("2019/01", "2019/02", "2019/03", "2019/04", "2019/05", "2019/06",
               "2019/07", "2019/08", "2019/09", "2019/10", "2019/11", "2019/12",
               "2020",
               "2021/01", "2021/02", "2021/03", "2021/04", "2021/05", "2021/06",
               "2021/07", "2021/08", "2021/09", "2021/10", "2021/11"
    ),
    residence_fips = test_counties_states,
    residence_urbanization_year = c("2013"),
    residence_urbanization = "All Categories",
    weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                "Saturday"),
    autopsy = c("Yes", "No"),
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
  expect_equal(df$Deaths[1:20], c(224L, 164L, 165L, 162L, 218L,
                                  233L, 260L, 269L, 237L, 234L,
                                  211L, 219L, 241L, 208L, 194L,
                                  235L, 293L, 369L, 354L, 354L))
  expect_true(grep("Single Race 6: Black or African American$", df$Notes) > 0)
  expect_true(grep("Hispanic Origin: Not Hispanic or Latino$", df$Notes) > 0)
  expect_true(grep("UCD - Injury Intent: Homicide$", df$Notes) > 0)
  expect_true(grep("Group By: Month", df$Notes) > 0)
  expect_true(grep(
    "Weekday: Monday; Tuesday; Wednesday; Thursday; Friday; Saturday$",
    df$Notes) > 0)
  expect_true(grep("Autopsy: Yes; No", df$Notes) > 0)
  expect_true(grep(
    paste0("Year/Month: 2019; 2020; Jan., 2021 ; Feb., 2021 ;",
           " Mar., 2021 ; Apr., 2021 ; May, 2021 ; Jun., 2021 ;",
           " Jul., 2021 ; Aug., 2021 ;$"), df$Notes) > 0)
  expect_true(grep("^Sep., 2021 ; Oct., 2021 ; Nov., 2021$", df$Notes) > 0)
  expect_true(grep(
    paste0("Five-Year Age Groups: 15-19 years; 20-24 years; 25-29 years;",
           " 30-34 years; 35-39 years; 100\\+ years$"), df$Notes) > 0)
  test_counties_states <- ifelse(nchar(test_counties_states) == 4,
                           paste0("0", test_counties_states),
                           as.character(test_counties_states)
  )
  test_counties_states <- ifelse(nchar(test_counties_states) == 1,
                           paste0("0", test_counties_states),
                           as.character(test_counties_states)
  )
  expect_true({
    ## Check that the CDC data has all the counties we requested
    (\() {
      for (FIPS in test_counties_states) {
          if (!any(grepl(paste0("(", FIPS, ")"), df$Notes))) {
            return(FALSE)
            print(FIPS)
          }
      }
      ## Check that no extra counties were included
      for (FIPS in setdiff(tothewonder:::COUNTIES_FIPS, test_counties_states)) {
        if (any(grepl(paste0("(", FIPS, ")"), df$Notes))) {
          return(FALSE)
          print(FIPS)
        }
      }
      return(TRUE)
    })()
  })


})
