test_that("UCD cause of death function errors", {
  fake_session <-
    "https://wonder.cdc.gov/controller/datarequest/D76;jsessionid="
  expect_error(ucd99(wonder_url = fake_session,
                     group_by_1 = "Week"),
               "group_by_1` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     period = "Week"),
               "period must follow the pattern")
  expect_error(ucd99(wonder_url = fake_session,
                     group_by_1 = "None"),
               "`group_by_1` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     age = "31-35"),
               "`age` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     residence_urbanization_year = "2011"),
               "`residence_urbanization_year` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     residence_urbanization = "MMWR Week"),
               "`residence_urbanization` must be one of ")
  expect_error(ucd99(wonder_url = fake_session,
                     gender = "MMWR Week"),
               "`gender` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     hispanic_origin = "MMWR Week"),
               "`hispanic_origin` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     race_option = "MMWR Week"))
  expect_error(ucd99(wonder_url = fake_session,
                     race = "Australoid"),
               "race` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     period = "2009/001"),
               "period must follow the pattern ")
  expect_error(ucd99(wonder_url = fake_session,
                     period = "80/01"),
               "period must follow the pattern")
  expect_error(ucd99(wonder_url = fake_session,
                     autopsy = "Maybe"),
               "`autopsy` must be one of ")
  expect_error(ucd99(wonder_url = fake_session,
                     place_of_death = "MMWR Week"),
               "place_of_death` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     ucd_option = "MMWR Week"),
               "ucd_option` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     ucd_option = "Injury Intent and Mechanism",
                     ucd_injury_intent = "MMWR Week"),
               "ucd_injury_intent` must be one of")

  expect_error(ucd99(wonder_url = fake_session,
                     ucd_option = "Injury Intent and Mechanism",
                     ucd_injury_intent = "Unintentional",
                     ucd_injury_mechanism = "AAAa"),
               "ucd_injury_mechanism` must be one of")
  expect_error(ucd99(wonder_url = fake_session,
                     ucd_option = "Drug/Alcohol Induced Causes",
                     ucd_drug_alcohol = "MMWR Week"),
               "ucd_drug_alcohol` must be one of")

  expect_error(ucd99(wonder_url = fake_session,
                     ucd_option = "ICD-10 113 Cause List",
                     ucd_cause_113 = "MMWR Week"),
               "ucd_cause_113` must be one of")

  expect_error(ucd99(wonder_url = fake_session,
                     period = 1999:2022),
               "period must be smaller or equal to 2020")
})
