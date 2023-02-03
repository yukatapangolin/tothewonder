test_that("Test that UCD queries are saved correctly", {
  wonder_url_ucd99 <- session_ucd99(I_Agree = TRUE)
  save_url_ucd99 <- ucd99(wonder_url = wonder_url_ucd99,
                          save = TRUE,
                          group_by_1 = "Month",
                          group_by_2 = "None",
                          group_by_3 = "None",
                          group_by_4 = "None",
                          show_totals = FALSE,
                          show_confidence_interval = FALSE,
                          show_standard_error = FALSE,
                          age = "All",
                          period = c("2018/01", as.character(2019:2020)),
                          residence_fips = c("01007", "01009", "01015",
                                             "01019", "06037"),
                          residence_urbanization_year = c("2013"),
                          residence_urbanization = "All Categories",
                          place_of_death = c("All Places"),
                          gender = c("All Genders"),
                          hispanic_origin = "Hispanic or Latino",
                          race = "White",
                          ucd_option = "ICD-10 Codes",
                          ucd_icd_codes = c("U07.1", "U01")) # COVID
                                                             # and Terrorism
  Sys.sleep(10)
  ## Simulate clicking the "I Agree" button and visiting the page
  res <- httr::GET(
    url = save_url_ucd99,
    httr::add_headers(.headers = c(
      "user-agent" = tothewonder:::get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = paste0("text/html,application/xhtml+xml,",
                        "application/xml;q=0.9,image/webp,*/*;q=0.8"),
      "Accept-Language" = "en-us,en;q=0.5"
    ))
  )
  Sys.sleep(10)
  saved_txt <- httr::content(res, as = "text")
  new_session <- paste0(
    "https://wonder.cdc.gov",
    sub(".*(/controller/datarequest/D76;jsessionid=[A-Z0-9]+).*", "\\1",
        saved_txt)
  )
  res <- httr::POST(
    url = new_session,
    body = "stage=about&saved_id=D297F751&action-I+Agree=I+Agree",
    encode = "raw",
    httr::add_headers(.headers = c(
      "user-agent" = tothewonder:::get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = paste0("text/html,application/xhtml+xml,",
                         "application/xml;q=0.9,image/webp,*/*;q=0.8"),
      "Accept-Language" = "en-us,en;q=0.5"
    )),
    httr::content_type("application/x-www-form-urlencoded")
  )
  Sys.sleep(10)
  saved_txt <- httr::content(res, as = "text")

  expect_true(grepl('<option value="01007" selected>',
                    saved_txt))
  expect_true(grepl('<option value="01009" selected>',
                    saved_txt))
  expect_true(grepl('<option value="01015" selected>',
                    saved_txt))
  expect_true(grepl('<option value="01019" selected>',
                    saved_txt))
  expect_true(grepl('<option value="2018/01" selected>',
                    saved_txt))
  expect_true(grepl('<option value="2019" selected>',
                    saved_txt))
  expect_true(grepl('<option value="2020" selected>',
                    saved_txt))
  expect_true(grepl('<option selected value="2135-2">',
                    saved_txt))
  expect_true(grepl('<option selected value="2106-3">',
                    saved_txt))
  expect_true(grepl('<option value="U01" selected>',
                    saved_txt))
  expect_true(grepl('<option value="U07.1" selected>',
                    saved_txt))
})
