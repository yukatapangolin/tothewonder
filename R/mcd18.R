
process_ucd_mcod <- function(opts3,
                             ucd_option,
                             ucd_injury_intent,
                             ucd_injury_mechanism,
                             ucd_drug_alcohol,
                             ucd_icd_codes,
                             ucd_cause_113) {
  set_ucd_opts <- function(ll, opts3) {
    purrr::pwalk(
      .l = ll,
      .f = ~ for (i in ..1) {
        ## if the call has 3 arguments (4 including .f)
        ## should use the key
        if (length(match.call()) == 4) {
          opts3[length(opts3) + 1] <<- dplyr::recode(i, !!!..3)
        } else {
          opts3[length(opts3) + 1] <<- i
        }
        names(opts3)[length(opts3)] <<- ..2
      }
    )
    return(opts3)
  }
  opts3$`O_ucd` <- dplyr::recode(ucd_option, !!!mcd_ucd_key)
  if (ucd_option == "Injury Intent and Mechanism") {
    rlang::arg_match(ucd_injury_intent, injury_intent_opts, multiple = TRUE)
    rlang::arg_match(ucd_injury_mechanism, injury_mechanism_opts,
      multiple =
        TRUE
    )
    opts3 <- set_ucd_opts(
      list(
        list(ucd_injury_intent, ucd_injury_mechanism),
        c("V_D176.V22", "V_D176.V23"),
        list(injury_intent_key, injury_mechanism_key)
      ),
      opts3
    )
  } else if (ucd_option == "Drug/Alcohol Induced Causes") {
    rlang::arg_match(ucd_drug_alcohol, drug_alcohol_opts, multiple = TRUE)
    opts3$F_D176.V25 <- NULL
    opts3 <- append_list_recode("F_D176.V25", ucd_drug_alcohol,
                                drug_alcohol_key, opts3)
  } else if (ucd_option == "ICD-10 113 Cause List") {
    rlang::arg_match(ucd_cause_113, icd10_113_list_opts, multiple = TRUE)
    opts3$V_D176.V4 <- NULL
    opts3 <- append_list_recode("V_D176.V4", ucd_cause_113,
                                icd10_113_list_key, opts3)
  } else if (ucd_option == "ICD-10 Codes") {
    opts3$F_D176.V2 <- NULL
    opts3 <- append_list("F_D176.V2", ucd_icd_codes, opts3)
  } else if (ucd_option == "ICD-10 130 Cause List (Infants)") {
    stop("Not implemented yet")
  }
  return(opts3)
}


#' Multiple Cause of Death, Injury Intent and Mechanism Results
#'
#' Download UCD 1999-2020 CDC WONDER death certificate data from \url{https://wonder.cdc.gov/mcd-icd10-expanded.html}
#'
#' @param wonder_url CDC WONDER url with the 'session id'. See
#' [session_mcd_provisional()]
#' @param save Save the query instead of downloading data from WONDER
#' @param group_by_1 One of `r paste("\x0a*", tothewonder:::mcd_group_by_1_opts, collapse = "")`
#' @param group_by_2 One of `r paste("\x0a*", tothewonder:::mcd_group_by_2_opts, collapse = "")`
#' @param group_by_3 One of `r paste("\x0a*", tothewonder:::mcd_group_by_2_opts, collapse = "")`
#' @param group_by_4 One of `r paste("\x0a*", tothewonder:::mcd_group_by_2_opts, collapse = "")`
#' @param show_age_adjusted Show age adjusted values and confidence bands
#' @param show_totals Show totals
#' @param show_confidence_interval Show the confidence interval of the rate
#' @param show_standard_error Show the standard error of the rate,
#' @param residence_fips "All" or a list of state/county FIPS codes
#' @param residence_urbanization_year One of
#' * 2006
#' * 2013
#' @param residence_urbanization One or more of `r paste("\x0a*", tothewonder:::urbanization_opts, collapse = "")`
#' @param occurrence_fips "All" or a list of state/county FIPS codes
#' @param occurrence_urbanization_year One of
#' * 2006
#' * 2013
#' @param occurrence_urbanization One or more of `r paste("\x0a*", tothewonder:::urbanization_opts, collapse = "")`
#' @param age One or more of `r paste("\x0a*", unique(tothewonder:::all_age_groups_opts), collapse = "")`
#' @param gender One or more of `r paste("\x0a*", tothewonder:::gender_opts, collapse = "")`
#' @param hispanic_origin One or more of `r paste("\x0a*", tothewonder:::hispanic_origin_opts, collapse = "")`
#' @param race_option One or more of `r paste("\x0a*", tothewonder:::race_options_opts, collapse = "")`
#' @param race One or more of `r paste("\x0a*", tothewonder:::race_18_opts, collapse = "")`
#' @param period_option One or more of `r paste("\x0a*", tothewonder:::period_opts, collapse = "")`
#' @param period A vector of years/months/weeks in the interval 2018/01:2021/12 for months or 2018/01:2021/52 for weeks (or last year available)
#' @param weekday One or more of `r paste("\x0a*", tothewonder:::weekday_opts, collapse = "")`
#' @param autopsy One or more of `r paste("\x0a*", tothewonder:::autopsy_opts, collapse = "")`
#' @param place_of_death One or more of `r paste("\x0a*", tothewonder:::place_of_death_opts, collapse = "")`
#' @param ucd_option One of `r paste("\x0a*", tothewonder:::ucd_opts, collapse = "")`
#' @param ucd_injury_intent One or more of `r paste("\x0a*", tothewonder:::injury_intent_opts, collapse = "")`
#' @param ucd_injury_mechanism One or more of `r paste("\x0a*", tothewonder:::injury_mechanism_opts, collapse = "")`
#' @param ucd_drug_alcohol One or more of `r paste("\x0a*", tothewonder:::drug_alcohol_opts, collapse = "")`
#' @param ucd_icd_codes An ICD-10 code: Visit \url{https://wonder.cdc.gov/mcd-icd10-provisional.html} for the complete list
#' @param ucd_cause_113 One or more of `r paste("\x0a*", tothewonder:::icd10_113_list_opts, collapse = "")`
#' @param mcd_option One or more of `r paste("\x0a*", tothewonder:::mcd_mcd_opts, collapse = "")`
#' @param mcd_drug_alcohol One or more of `r paste("\x0a*", tothewonder:::drug_alcohol_opts, collapse = "")`
#' @param mcd_drug_alcohol_and One or more of `r paste("\x0a*", tothewonder:::drug_alcohol_opts, collapse = "")`
#' @param mcd_icd_codes An ICD-10 code: See \url{https://wonder.cdc.gov/mcd-icd10-provisional.html} for the complete list
#' @param mcd_icd_codes_and An ICD-10 code: See \url{https://wonder.cdc.gov/mcd-icd10-provisional.html} for the complete list
#' @param mcd_infant_list Not implemented
#' @param mcd_infant_list_and Not implemented
#' @param mcd_cause_113 One or more of `r paste("\x0a*", tothewonder:::icd10_113_list_opts, collapse = "")`
#' @param mcd_cause_113_and One or more of `r paste("\x0a*", tothewonder:::icd10_113_list_opts, collapse = "")`
#'
#' @return A data.frame with the data from wonder
#' @export
#'
#' @md
mcd_provisional <- function(wonder_url,
                            save = FALSE,
                            group_by_1 = mcd_group_by_1_opts,
                            group_by_2 = mcd_group_by_2_opts,
                            group_by_3 = mcd_group_by_2_opts,
                            group_by_4 = mcd_group_by_2_opts,
                            show_age_adjusted = FALSE,
                            show_totals = FALSE,
                            show_confidence_interval = FALSE,
                            show_standard_error = FALSE,
                            residence_fips = "All",
                            residence_urbanization_year = c("2006", "2013"),
                            residence_urbanization = urbanization_opts,
                            occurrence_fips = "All",
                            occurrence_urbanization_year = c("2006", "2013"),
                            occurrence_urbanization = urbanization_opts,
                            age = all_age_groups_opts,
                            gender = gender_opts,
                            hispanic_origin = hispanic_origin_opts,
                            race_option = race_options_opts,
                            race = "All",
                            period_option = period_opts,
                            period = "All",
                            weekday = weekday_opts,
                            autopsy = autopsy_opts,
                            place_of_death = place_of_death_opts,
                            ucd_option = mcd_ucd_opts,
                            ucd_injury_intent = injury_intent_opts,
                            ucd_injury_mechanism = injury_mechanism_opts,
                            ucd_drug_alcohol = drug_alcohol_opts,
                            ucd_icd_codes = "All",
                            ucd_cause_113 = icd10_113_list_opts,
                            mcd_option = mcd_mcd_opts,
                            mcd_drug_alcohol = drug_alcohol_opts,
                            mcd_drug_alcohol_and = drug_alcohol_opts,
                            mcd_icd_codes = "All",
                            mcd_icd_codes_and = "All",
                            mcd_infant_list = "All",
                            mcd_infant_list_and = "All",
                            mcd_cause_113 = icd10_113_list_opts,
                            mcd_cause_113_and = icd10_113_list_opts) {
  if (missing(wonder_url)) {
    stop("wonder_url is missing")
  }
  if (!grepl(
    "^https://wonder.cdc.gov/controller/datarequest/D176;jsessionid=",
    wonder_url
  )) {
    stop("wonder_url is badly formatted")
  }
  stopifnot(is.logical(save))
  # Assign the first option to any missing argument
  defined <- ls()
  passed <- names(as.list(match.call())[-1])
  if (any(!defined %in% passed)) {
    for (i in setdiff(defined, passed)) {
      assign(i, get(i)[1])
    }
  }
  # Replace "All" with the proper argument
  list2env(replace_All(
    residence_urbanization = residence_urbanization,
    occurrence_urbanization = occurrence_urbanization,
    age = age,
    gender = gender,
    race = race,
    hispanic_origin = hispanic_origin,
    weekday = weekday,
    autopsy = autopsy,
    place_of_death = place_of_death,
    ucd_injury_intent = ucd_injury_intent,
    ucd_injury_mechanism = ucd_injury_mechanism,
    ucd_drug_alcohol = ucd_drug_alcohol,
    ucd_cause_113 = ucd_cause_113,
    mcd_drug_alcohol = mcd_drug_alcohol,
    mcd_drug_alcohol_and = mcd_drug_alcohol_and,
    mcd_cause_113 = mcd_cause_113,
    mcd_cause_113_and = mcd_cause_113_and
  ),
  envir = environment()
  )

  ## Build the query parameter by parameter
  opts3 <- mcd_form_options

  ##########################################################################
  ## params for group by
  rlang::arg_match(group_by_1, mcd_group_by_1_opts)
  rlang::arg_match(group_by_2, mcd_group_by_2_opts)
  rlang::arg_match(group_by_3, mcd_group_by_2_opts)
  rlang::arg_match(group_by_4, mcd_group_by_2_opts)

  v <- c(group_by_1, group_by_2, group_by_3, group_by_4)
  if (length(v[v != "None"]) != length(unique(v[v != "None"]))) {
    stop("group_bys must be unique")
  }
  opts3$B_1 <- dplyr::recode(group_by_1, !!!mcd_group_by_1_key)
  opts3$B_2 <- dplyr::recode(group_by_2, !!!mcd_group_by_2_key)
  opts3$B_3 <- dplyr::recode(group_by_3, !!!mcd_group_by_2_key)
  opts3$B_4 <- dplyr::recode(group_by_4, !!!mcd_group_by_2_key)


  ############################################################################
  ## Uncertainty
  stopifnot(is.logical(show_totals))
  stopifnot(is.logical(show_confidence_interval))
  stopifnot(is.logical(show_standard_error))
  # params for uncertainaty options
  if (!show_totals) {
    opts3$`O_show_totals` <- "false"
  }
  if (show_confidence_interval) {
    opts3$`M_32` <- "D176.M32"
  }
  if (show_standard_error) {
    opts3$`M_31` <- "D176.M31"
  }

  ############################################################################
  ## FIPS residence code for states/counties
  rlang::arg_match(residence_urbanization_year, c("2006", "2013"))
  rlang::arg_match(residence_urbanization, urbanization_opts, multiple = TRUE)
  ## Add a leading zero to FIPS codes if length eq 4
  residence_fips <- ifelse(nchar(residence_fips) == 4,
    paste0("0", residence_fips),
    as.character(residence_fips)
  )
  if (!all(residence_fips %in% c("All", FIPS_mcd))) {
    warning(paste0("Looks like you are using a FIPS code unknown to WONDER. ",
                   "This may cause the query to error."))
    warning(setdiff(residence_fips, FIPS_mcd))
  }
  opts3$`O_location` <- "D176.V9"
  if (any(residence_fips == "All")) {
    residence_fips <- "*All*"
  }
  opts3 <- append_list("F_D176.V9", residence_fips, opts3)
  # opts3$"I_D176.V9" <- gsub(
  #   "%20", "+",
  #   utils::URLencode(
  #     paste(
  #       dplyr::recode(sort(residence_fips), !!!FIPS_key)
  #     ),
  #     reserved = TRUE
  # )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
  # opts3$"I_D176.V9" <- paste0(opts3$"I_D176.V9", "%0D%0A")

  ## param residence urbanization
  if (residence_urbanization_year == 2013) {
    urb_key <- "V_D176.V19"
    opts3$`O_urban` <- "D176.V19"
    opts3$`V_D176.V11` <- "*All*"
  } else if (residence_urbanization_year == 2006) {
    urb_key <- "V_D176.V11"
    opts3$`O_urban` <- "D176.V11"
    opts3$`V_D176.V19` <- "*All*"
  }
  ## fill list with urban codes
  opts3 <- append_list_recode(
    urb_key, residence_urbanization,
    urbanization_key, opts3
  )

  ##########################################################
  ## FIPS occurrence  for states/counties
  rlang::arg_match(occurrence_urbanization_year, c("2006", "2013"))
  rlang::arg_match(occurrence_urbanization, urbanization_opts, multiple = TRUE)
  occurrence_fips <- ifelse(nchar(occurrence_fips) == 4,
    paste0("0", occurrence_fips),
    as.character(occurrence_fips)
  )
  if (!all(occurrence_fips %in% c("All", FIPS_mcd))) {
    warning("Looks like you are using a FIPS code unknown to WONDER. This may cause the query to error.")
    warning(setdiff(occurrence_fips, FIPS_mcd))
  }
  opts3$`O_death_location` <- "D176.V79"
  if (any(occurrence_fips == "All")) {
    occurrence_fips <- "*All*"
  }
  opts3 <- append_list("F_D176.V79", occurrence_fips, opts3)
  ## param occurrence urbanization
  if (occurrence_urbanization_year == 2013) {
    urb_key <- "D176.V89"
    opts3$`O_death_urban` <- "D176.V89"
    opts3$`D176.V81` <- "*All*"
  } else if (occurrence_urbanization_year == 2006) {
    urb_key <- "D176.V81"
    opts3$`O_death_urban` <- "D176.V81"
    opts3$`D176.V89` <- "*All*"
  }
  ## fill list with urban codes
  opts3 <- append_list_recode(
    urb_key, occurrence_urbanization,
    urbanization_key, opts3
  )

  ###########################################################################
  ## Age
  stopifnot(is.logical(show_age_adjusted))
  if (!is.character(age)) {
    age <- as.character(age)
  }
  rlang::arg_match(age, all_age_groups_opts, multiple = TRUE)
  ## age param
  if (all(age %in% ten_year_age_groups_opts)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!ten_year_age_groups_key)
      names(opts3)[length(opts3)] <- "V_D176.V5"
      opts3$`O_age` <- "D176.V5"
    }
    # Add age adjusted column if 2 or more ten-year age groups are requested
    if ((length(age) >= 2 | identical(age, "All Ages")) & show_age_adjusted) {
      opts3$`O_aar_enable` <- "true"
      opts3$`O_aar` <- "aar_std"
      opts3$`O_aar_CI` <- "true"
      opts3$`O_aar_SE` <- "true"
    } else {
      opts3$`O_aar` <- "aar_none"
      opts3$`O_aar_CI` <- "false"
      opts3$`O_aar_SE` <- "false"
    }
  } else if (all(age %in% five_year_age_groups_opts)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!five_year_age_groups_key)
      names(opts3)[length(opts3)] <- "V_D176.V51"
      opts3$`O_age` <- "D176.V51"
      opts3$`O_aar` <- "aar_none"
    }
  } else if (all(age %in% single_year_ages_opts)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!single_year_ages_key)
      names(opts3)[length(opts3)] <- "V_D176.V52"
      opts3$`O_age` <- "D176.V52"
      opts3$`O_aar` <- "aar_none"
    }
  } else if (all(age %in% infant_age_groups_opts)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!infant_age_groups_key)
      names(opts3)[length(opts3)] <- "V_D176.V6"
      opts3$`O_age` <- "D176.V6"
      opts3$`O_aar` <- "aar_none"
    }
  } else {
    stop("Invalid age groups")
  }

  ##########################################################################
  ## Gender
  rlang::arg_match(gender, gender_opts, multiple = TRUE)
  opts3 <- append_list_recode("V_D176.V7", gender, gender_key, opts3)

  ###########################################################################
  ## Hispanic
  rlang::arg_match(hispanic_origin, hispanic_origin_opts, multiple = TRUE)
  opts3 <- append_list_recode(
    "V_D176.V17",
    hispanic_origin, hispanic_origin_key, opts3
  )

  ##########################################################################
  ## race
  rlang::arg_match(race_option, race_options_opts, multiple = TRUE)
  if (race_option == "Single Race 6") {
    rlang::arg_match(race, single_race_6_opts, multiple = TRUE)
    opts3$`O_race` <- "D176.V42"
    opts3$`V_D176.V43` <- "*All*"
    opts3$`V_D176.V44` <- "*All*"
    opts3 <- append_list_recode("V_D176.V42", race, single_race_6_key, opts3)
  } else if (race_option == "Single Race 15") {
    rlang::arg_match(race, single_race_15_opts, multiple = TRUE)
    opts3$`O_race` <- "D176.V43"
    opts3$`V_D176.V42` <- "*All*"
    opts3$`V_D176.V44` <- "*All*"
    opts3 <- append_list_recode("V_D176.V43", race, single_race_15_key, opts3)
  } else if (race_option == "Single/Multi Race 31") {
    rlang::arg_match(race, single_multi_race_31_opts, multiple = TRUE)
    opts3$`O_race` <- "D176.V44"
    opts3$`V_D176.V42` <- "*All*"
    opts3$`V_D176.V43` <- "*All*"
    opts3 <- append_list_recode(
      "V_D176.V44", race, single_multi_race_31_key,
      opts3
    )
  }


  #########################################################################
  ## period param
  ## check period is in the right format

  rlang::arg_match(period_option, c("Year", "MMWR"), multiple = TRUE)
  if (period_option == "Year") {
    if (any(period == "All")) {
      period <- "*All*"
    } else {
      if (!all(nchar(period) %in% c(4, 7))) {
        stop("period must follow the pattern '%Y/%m' or '%Y' (e.g. 2002 or 1999/01 or 2020/11)")
      }
      if (any(as.numeric(gsub(
        "([0-9]{4}).*", "\\1",
        period
      )) < 2018)) {
        stop("period must be greater than 2018")
      }
      test_char <- function(x) {
        if (nchar(x) == 7) {
          return(
            tryCatch(!is.na(as.Date(paste0(x, "/01"),
              format = "%Y/%m/%d"
            )),
            error = function(err) {
              FALSE
            }
            )
          )
        }
        if (nchar(x) == 4) {
          return(tryCatch(!is.na(as.Date(paste0(x, "/01/01"),
            format = "%Y/%m/%d"
          )),
          error = function(err) {
            FALSE
          }
          ))
        }
      }
      if (!all(sapply(period, test_char))) {
        stop("period must follow the pattern '%Y/%m' or '%Y' (e.g. 2002 or 1999/01 or 2020/11)")
      }
    }
    opts3$`O_dates` <- "YEAR"
    opts3$`F_D176.V100` <- "*All*"
    opts3 <- append_list("F_D176.V1", period, opts3)
  } else if (period_option == "MMWR") {
    if (any(period == "All")) {
      period <- "*All*"
    } else {
      if (!all(nchar(period) %in% c(4, 7))) {
        stop("period must follow the pattern '%Y/%m' or '%Y' (e.g. 2002 or 1999/01 or 2020/11)")
      }
      if (any(as.numeric(gsub(
        "([0-9]{4}).*", "\\1",
        period
      )) < 2018)) {
        stop("period must be greater than 2018")
      }
    }
    opts3$`O_dates` <- "MMWR"
    opts3$`F_D176.V1` <- "*All*"
    opts3 <- append_list("F_D176.V100", period, opts3)
  }


  ##########################################################################
  ## autopsy
  rlang::arg_match(autopsy, autopsy_opts, multiple = TRUE)
  opts3 <- append_list_recode("V_D176.V20", autopsy, autopsy_key, opts3)

  ##########################################################################
  ## place of death
  rlang::arg_match(place_of_death, place_of_death_opts, multiple = TRUE)
  opts3 <- append_list_recode(
    "V_D176.V21", place_of_death,
    place_of_death_key, opts3
  )

  ##########################################################################
  ## Underlying cause of death
  rlang::arg_match(ucd_option, mcd_ucd_opts)
  if (ucd_option == "ICD-10 Codes") {
    if (any(ucd_icd_codes == "All")) {
      ucd_icd_codes <- "*All*"
    }
    if (any(ucd_icd_codes == "All Causes of Death")) {
      ucd_icd_codes <- "*All*"
    }
    if (any(ucd_icd_codes == "*All*(All Causes of Death)")) {
      ucd_icd_codes <- "*All*"
    }
    check_icd_codes(ucd_icd_codes)
  }
  opts3 <- process_ucd_mcod(
    opts3,
    ucd_option,
    ucd_injury_intent,
    ucd_injury_mechanism,
    ucd_drug_alcohol,
    ucd_icd_codes,
    ucd_cause_113
  )

  ##########################################################################
  ## Multiple cause of death
  rlang::arg_match(mcd_option, mcd_mcd_opts)

  opts3$O_mcd <- dplyr::recode(mcd_option, !!!mcd_mcd_key)
  if (mcd_option == "MCD - Drug/Alcohol Induced Causes") {
    rlang::arg_match(mcd_drug_alcohol, drug_alcohol_opts, multiple = TRUE)
    rlang::arg_match(mcd_drug_alcohol_and, drug_alcohol_opts, multiple = TRUE)

    opts3$"V_D176.V26" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(mcd_drug_alcohol, !!!drug_alcohol_key),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D176.V26" <- I(paste0(opts3$"V_D176.V26", "%0D%0A"))

    # opts3$"V_D176.V26" <- utils::URLencode(paste(gsub(
    #   " ",
    #   "+",
    #   dplyr::recode(
    #     mcd_drug_alcohol,
    #     !!!drug_alcohol_key
    #   )
    # ),
    # collapse = "+\r\n"
    # ))

    opts3$"V_D176.V26_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(mcd_drug_alcohol_and, !!!drug_alcohol_key),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A

    opts3$"V_D176.V26_AND" <- I(paste0(opts3$"V_D176.V26_AND", "%0D%0A"))
    # opts3$"V_D176.V26_AND" <- utils::URLencode(paste(gsub(
    #   " ",
    #   "+",
    #   dplyr::recode(
    #     mcd_drug_alcohol_and,
    #     !!!drug_alcohol_key
    #   )
    # ),
    # collapse = "+\r\n"
    # ))
  } else if (mcd_option == "MCD - ICD-10 113 Cause List") {
    rlang::arg_match(mcd_cause_113, icd10_113_list_opts, multiple = TRUE)
    rlang::arg_match(mcd_cause_113_and, icd10_113_list_opts, multiple = TRUE)

    opts3$"V_D176.V15" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(
          mcd_cause_113,
          !!!icd10_113_list_key
        ),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D176.V15" <- I(paste0(opts3$"V_D176.V15", "%0D%0A"))
    opts3$"V_D176.V15_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(
          mcd_cause_113_and,
          !!!icd10_113_list_key
        ),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D176.V15_AND" <- I(paste0(opts3$"V_D176.V15_AND", "%0D%0A"))

  } else if (mcd_option == "MCD - ICD-10 Codes") {
    if (any(mcd_icd_codes == "All")) {
      mcd_icd_codes <- "*All*"
    }
    if (any(mcd_icd_codes_and == "All")) {
      mcd_icd_codes_and <- "*All*"
    }
    if (any(mcd_icd_codes == "All Causes of Death")) {
      mcd_icd_codes <- "*All*"
    }
    if (any(mcd_icd_codes == "*All*(All Causes of Death)")) {
      mcd_icd_codes <- "*All*"
    }
    if (any(mcd_icd_codes_and == "All Causes of Death")) {
      mcd_icd_codes_and <- "*All*"
    }
    if (any(mcd_icd_codes_and == "*All*(All Causes of Death)")) {
      mcd_icd_codes_and <- "*All*"
    }
    check_icd_codes(mcd_icd_codes)
    check_icd_codes(mcd_icd_codes_and)
    opts3$"V_D176.V13" <- gsub(
      "%20", "+",
      utils::URLencode(
        mcd_icd_codes,
        reserved = TRUE
    )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D176.V13" <- I(paste0(opts3$"V_D176.V13", "%0D%0A"))
    opts3$"V_D176.V13_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        mcd_icd_codes_and,
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D176.V13_AND" <- I(paste0(opts3$"V_D176.V13_AND", "%0D%0A"))
  } else if (mcd_option == "MCD - ICD-10 130 Cause List (Infants)") {
    stop("Not implemented yet")
  }

  ##############################################################################
  ## Save query
  if(save) {

    ## To save a location/period/drug-alcohol/icd10 code item
    ## you have to open up the selection box so that subitems
    ## are visible
    ## Change the query to a simple one to avoid hitting the
    ## WONDER server
    simple_mcd_form2 <- simple_mcd_form
    ## remove all keys that match select items to open
    for(key in c("F_D176.V9", "F_D176.V79",
                 "F_D176.V1", "F_D176.V100",
                 "F_D176.V25", "F_D176.V2",
                 "F_D176.V26", "F_D176.V13"))
      simple_mcd_form2 <- simple_mcd_form2[ - which(names(simple_mcd_form2) ==
                                                      key)]

    open_item(simple_mcd_form2, opts3, "Open",
              "F_D176.V9", state_FIPS, wonder_url)
    open_item(simple_mcd_form2, opts3, "Open",
              "F_D176.V79", state_FIPS, wonder_url)
    if (period_option == "Year")
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D176.V1", as.character(2018:2022), wonder_url)
    else
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D176.V100", as.character(2018:2022), wonder_url)
    if (ucd_option == "Drug/Alcohol Induced Causes")
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D176.V25", c("A", "O", "N"), wonder_url)
    else if (ucd_option == "ICD-10 Codes")
      open_item(simple_mcd_form2, opts3, "Open Fully",
                "F_D176.V2", icd10_top_items_mcd, wonder_url)
    if (mcd_option == "MCD - Drug/Alcohol Induced Causes")
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D176.V26", c("A", "O", "N"), wonder_url)
    else if (mcd_option == "MCD - ICD-10 Codes")
      open_item(simple_mcd_form2, opts3, "Open Fully",
                "F_D176.V13", icd10_top_items_mcd, wonder_url)
    opts3$`action-Save` <- "Save"
    opts3$`action-Send` <- NULL
  }

  opts3$"dataset_label" <- I(opts3$"dataset_label")
  #opts3$"dataset_vintage" <- I("May+21%2C+2022+as+of+June+05%2C+2022")

  ## 1) Query WONDER
  res <- query_wonder(wonder_url, opts3)
  wonder_text <- httr::content(res, as = "text")
  if(save){
    saved_query <- extract_saved(wonder_text, "D176")
    if (grepl("Your session timed out after ", wonder_text)) {
      stop("Your session timed out, please start another wonder_session")
    }
    if (grepl('"error-message"', wonder_text)) {
      print_errors(wonder_text)
    }
    return(saved_query)
  }


  df <- rvest::html_node(
    rvest::read_html(httr::content(res, as = "raw")),
    "table.response-form"
  )

  ## If there's no table something went wrong with our query
  if (length(df) == 0) {
    if (grepl("Your session timed out after ", wonder_text)) {
      stop("Your session timed out, please start another wonder_session")
    } else {
      print_errors(wonder_text)
      stop("Error in data received from WONDER")
    }
  }
  ## This are really warnings since WONDER output some data
  if (grepl('"error-message"', wonder_text)) {
    print_errors(wonder_text)
  }

  ## save query option
  # if(save) {
  #   res <- query_wonder(wonder_url, mcd_save_form)
  #
  #   saved_query_text <- httr::content(res, as = "text")
  #   saved_query <- extract_saved(saved_query_text, "D176")
  #   return (saved_query)
  # }

  ## 2) Export data from WONDER as tab separated text
  res <- query_wonder(wonder_url, mcd_download_form)




  text_table <- httr::content(res, as = "text")
  df <- utils::read.csv(text = text_table, sep = "\t")

  Sys.sleep(3)
  return(df)
}
