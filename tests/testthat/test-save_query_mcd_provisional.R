test_that("Test that MCD queries are saved correctly", {
  wonder_url_mcod18 <- session_mcd_provisional(I_Agree = TRUE)
  save_url_ucd99 <- mcd_provisional(
    wonder_url = wonder_url_mcod18,
    save = TRUE,
    group_by_1 = "Month",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_totals = FALSE,
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = "All",
    period_option = "Year",
    period = c("2018/01", as.character(2019:2022)),
    residence_fips = c("01007", "01009", "01015", "01019", "06037"),
    residence_urbanization_year = c("2013"),
    residence_urbanization = "All Categories",
    occurrence_fips = c("01007", "01009", "01015", "01019", "06037"),
    occurrence_urbanization_year = c("2013"),
    occurrence_urbanization = "All Categories",
    autopsy = c("All Values"),
    place_of_death = c("All Places"),
    gender = c("All Genders"),
    hispanic_origin = "Hispanic or Latino",
    race_option = "Single Race 6",
    race = "White",
    ucd_option = "Drug/Alcohol Induced Causes",
    ucd_drug_alcohol = "Alcohol poisonings (overdose) (X45, X65, Y15)",
    mcd_option = "MCD - ICD-10 Codes",
    mcd_icd_codes = c("W00-X59", "V01-V99"),
    mcd_icd_codes_and = "All")
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
  mcd_saved_txt <- httr::content(res, as = "text")
  new_session <- paste0(
    "https://wonder.cdc.gov",
    sub(".*(/controller/datarequest/D176;jsessionid=[A-Z0-9]+).*", "\\1",
        mcd_saved_txt)
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
  mcd_saved_txt <- httr::content(res, as = "text")

  expect_true(grepl('<option value="01007" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="01009" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="01015" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="01019" selected>',
                   mcd_saved_txt))
  expect_true(grepl('<option value="06037" selected>',
                   mcd_saved_txt))
  expect_true(grepl('<option value="2018/01" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="2019" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="2020" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="2135-2">',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="2106-3">',
                    mcd_saved_txt))
  expect_true(grepl('option value="A1" selected',
                    mcd_saved_txt))

  expect_true(grepl(
    paste0("input checked onclick=\"toggleOptions\\('D176.V25',",
           " 'D176.V2,D176.V4,D176.V12,D176.V22',false\\)"),
    mcd_saved_txt))

  expect_true(grepl(paste0('</label><textarea wrap="off" ',
                           'id="TD176\\.V13-AND1" name="V_D176\\.V13">W00-',
                           "X59\r\nV01-V99</textarea>"),
                    mcd_saved_txt))

})
