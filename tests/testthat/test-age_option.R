test_that("age_option works correctly", {
  # age_option(age, show_age_adjusted, "76", opts2,
  # group_by_1, group_by_2, group_by_3, group_by_4)
  expect_identical(
    age_option(0:2,
               FALSE,
               "76",
               list(),
               "Single-Year Ages",
               "Year",
               "None",
               "None"),
    list(V_D76.V52 = "0", O_age = "D76.V52", O_aar = "aar_none",
         V_D76.V52 = "1",  V_D76.V52 = "2"))
  expect_error(
    age_option("< 1",
               FALSE,
               "76",
               list(),
               "Single-Year Ages",
               "Year",
               "None",
               "None"),
    "`age` must be one of ")

  expect_identical(
    age_option(c("< 1", "1-4"),
               FALSE,
               "76",
               list(),
               "Five-Year Ages",
               "Year",
               "None",
               "None"),
    list(V_D76.V51 = "1", O_age = "D76.V51", O_aar = "aar_none",
         V_D76.V51 = "1-4"))

  expect_identical(
    age_option(c("< 1", "1-4"),
               FALSE,
               "76",
               list(),
               "Ten-Year Ages",
               "Year",
               "None",
               "None"),
    list(V_D76.V5 = "1", O_age = "D76.V5", V_D76.V5 = "1-4", O_aar = "aar_none",
         O_aar_CI = "false", O_aar_SE = "false"))


})
