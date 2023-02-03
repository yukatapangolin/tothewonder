test_that("multiplication works", {
  ll <- list(
    "sad" = "sfd",
    "adsf" = "fdsa"
  )
  SINGLE_RACE_6_KEY <- c(
    "All Races" = "*All*",
    "American Indian or Alaska Native" = "1002-5",
    "Asian" = "A",
    "Black or African American" = "2054-5",
    "Native Hawaiian or Other Pacific Islander" = "NHOPI",
    "White" = "2106-3",
    "More than one race" = "M"
  )
  expect_equal(
    append_list(
      c("aa", "bb"),
      c(1, 2),
      ll
    ),
    list(sad = "sfd", adsf = "fdsa", aa = 1, bb = 2)
  )
  expect_equal(
    append_list_recode(
      c("aa", "bb"),
      c("All Races", "Asian"),
      SINGLE_RACE_6_KEY,
      ll
    ),
    list(sad = "sfd", adsf = "fdsa", aa = "*All*", bb = "A")
  )
})
