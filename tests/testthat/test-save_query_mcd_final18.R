test_that("Test that MCD final18 queries are saved correctly", {
  wonder_url_mcod18 <- session_mcd_final18(I_Agree = TRUE)
  save_url_mcd_final18 <- mcd_final18(
    wonder_url = wonder_url_mcod18,
    save = TRUE,
    group_by_1 = "Month",
    group_by_2 = "None",
    group_by_3 = "None",
    group_by_4 = "None",
    show_totals = FALSE,
    show_confidence_interval = FALSE,
    show_standard_error = FALSE,
    age = c("15-19", "20-24", "25-29", "30-34", "35-39", "100+"),
    period = c("2018/01", as.character(2019:2021)),
    residence_fips = c("01007", "01009", "01015", "01019", "06037", "48",
                       "36", "12"),
    residence_urbanization_year = c("2006"),
    residence_urbanization = c("Large Central Metro", "Large Fringe Metro",
                               "Medium Metro", "Small Metro",
                               "Micropolitan (Nonmetro)"),
    weekday = c("Monday", "Sunday", "Tuesday", "Wednesday",
                "Thursday", "Friday"),
    autopsy = c("Yes", "No"),
    ## Missing Hospice Facility
    place_of_death = c("Medical Facility - Inpatient",
                      "Medical Facility - Outpatient or ER",
                      "Medical Facility - Dead on Arrival",
                      "Medical Facility - Status unknown",
                      "Decedent's home",
                      "Nursing home/long term care",
                      "Other"),
    gender = c("Female", "Male"),
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
    url = save_url_mcd_final18,
    httr::add_headers(.headers = c(
      "user-agent" = tothewonder:::get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = paste0("text/html,application/xhtml+xml,application",
                        "/xml;q=0.9,image/webp,*/*;q=0.8"),
      "Accept-Language" = "en-us,en;q=0.5"
    ))
  )
  Sys.sleep(10)
  mcd_saved_txt <- httr::content(res, as = "text")
  new_session <- paste0(
    "https://wonder.cdc.gov",
    sub(".*(/controller/datarequest/D157;jsessionid=[A-Z0-9]+).*", "\\1",
        mcd_saved_txt
        )
  )
  res <- httr::POST(
    url = new_session,
    body = "stage=about&saved_id=D297F751&action-I+Agree=I+Agree",
    encode = "raw",
    httr::add_headers(.headers = c(
      "user-agent" = tothewonder:::get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = paste0("text/html,application/xhtml+xml,application",
                        "/xml;q=0.9,image/webp,*/*;q=0.8"),
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
  expect_true(grepl('<option value="48" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="36" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="12" selected>',
                    mcd_saved_txt))


  expect_true(grepl(paste0('<option selected value="15-19">&nbsp;15-19',
                           " years&nbsp;</option>"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="20-24">&nbsp;20-24',
                           " years&nbsp;</option>"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="25-29">&nbsp;25-29',
                           " years&nbsp;</option>"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="30-34">&nbsp;30-34',
                           " years&nbsp;</option>"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="35-39">&nbsp;35-39',
                           " years&nbsp;</option>"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="100\\+">&nbsp;',
                           "100\\+ years&nbsp;</option>"),
                    mcd_saved_txt))

  expect_true(grepl('<option selected value="F">&nbsp;Female&nbsp',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="M">&nbsp;Male&nbsp;',
                    mcd_saved_txt))


  expect_true(grepl('<option selected value="1">&nbsp;Sunday&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="2">&nbsp;Monday&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="3">&nbsp;Tuesday&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="4">&nbsp;Wednesday&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="5">&nbsp;Thursday&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="6">&nbsp;Friday&nbsp;',
                    mcd_saved_txt))

  expect_true(grepl('<option selected value="Y">&nbsp;Yes&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="N">&nbsp;No&nbsp;',
                    mcd_saved_txt))

  expect_true(grepl(paste0('<option selected value="1">&nbsp;Medical',
                           " Facility - Inpatient&nbsp;"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="2">&nbsp;Medical',
                           " Facility - Outpatient or ER&nbsp;"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="3">&nbsp;Medical',
                           " Facility - Dead on Arrival&nbsp;"),
                    mcd_saved_txt))
  expect_true(grepl(paste0('<option selected value="10">&nbsp;Medical',
                          " Facility - Status unknown&nbsp;"),
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="4">&nbsp;Decedent\'s home&nbsp;',
                    mcd_saved_txt))
  expect_true(grepl(
    '<option selected value="6">&nbsp;Nursing home/long term care&nbsp;',
    mcd_saved_txt))
  expect_true(grepl('<option selected value="7">&nbsp;Other&nbsp;',
                    mcd_saved_txt))

  expect_true(grepl('<option value="2018/01" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="2019" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="2020" selected>',
                    mcd_saved_txt))
  expect_true(grepl('<option value="2021" selected>',
                    mcd_saved_txt))

  expect_true(grepl('<option selected value="2135-2">',
                    mcd_saved_txt))
  expect_true(grepl('<option selected value="2106-3">',
                    mcd_saved_txt))
  expect_true(grepl('option value="A1" selected',
                    mcd_saved_txt))

  expect_true(grepl(paste0("input checked onclick=\"toggleOptions",
                           "\\('D157.V25', 'D157.V2,D157.V4,D157.V12,",
                           "D157.V22',false"),
                    mcd_saved_txt))

  expect_true(grepl(paste0("</label><textarea wrap=\"off\"",
                           " id=\"TD157.V13-AND1\"",
                           " name=\"V_D157.V13\">W00-X59\r\n",
                           "V01-V99</textarea>"),
                    mcd_saved_txt))

})
