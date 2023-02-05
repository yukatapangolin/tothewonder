test_that("wonder_session test", {
  expect_match(
    session_mcd_final18(),
    "^https://wonder.cdc.gov/controller/datarequest/D157;jsessionid=[A-Z0-9]+")
  Sys.sleep(10)
  expect_match(
    session_mcd_provisional(),
    "^https://wonder.cdc.gov/controller/datarequest/D176;jsessionid=[A-Z0-9]+")
  Sys.sleep(10)
  expect_match(
    session_ucd99(),
    "^https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=[A-Z0-9]+")
  Sys.sleep(10)
})
