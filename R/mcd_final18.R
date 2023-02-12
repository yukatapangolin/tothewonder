
process_ucd_mcod_final18 <- function(opts3,
                             ucd_option,
                             ucd_injury_intent,
                             ucd_injury_mechanism,
                             ucd_drug_alcohol,
                             ucd_icd_codes,
                             ucd_cause_113) {
  set_ucd__opts <- function(ll, opts3) {
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
  opts3$`O_ucd` <- dplyr::recode(ucd_option, !!!MCD_FINAL18_UCD_KEY)
  if (ucd_option == "Injury Intent and Mechanism") {
    ucd_injury_intent <- rlang::arg_match(ucd_injury_intent,
                                          INJURY_INTENT_OPTS,
                                          multiple = TRUE)
    ucd_injury_mechanism <- rlang::arg_match(ucd_injury_mechanism,
                                             INJURY_MECHANISM_OPTS,
                                             multiple = TRUE
    )
    opts3 <- set_ucd__opts(
      list(
        list(ucd_injury_intent, ucd_injury_mechanism),
        c("V_D157.V22", "V_D157.V23"),
        list(INJURY_INTENT_KEY, INJURY_MECHANISM_KEY)
      ),
      opts3
    )
  } else if (ucd_option == "Drug/Alcohol Induced Causes") {
    ucd_drug_alcohol <- rlang::arg_match(ucd_drug_alcohol,
                                         DRUG_ALCOHOL_OPTS,
                                         multiple = TRUE)
    opts3$F_D157.V25 <- NULL
    opts3 <- append_list_recode("F_D157.V25", ucd_drug_alcohol,
                                DRUG_ALCOHOL_KEY, opts3)
  } else if (ucd_option == "ICD-10 113 Cause List") {
    ucd_cause_113 <- rlang::arg_match(ucd_cause_113,
                                      ICD10_113_LIST_OPTS,
                                      multiple = TRUE)
    opts3$V_D157.V4 <- NULL
    opts3 <- append_list_recode("V_D157.V4", ucd_cause_113,
                                ICD10_113_LIST_KEY, opts3)
  } else if (ucd_option == "ICD-10 Codes") {
    opts3$F_D157.V2 <- NULL
    opts3 <- append_list("F_D157.V2", ucd_icd_codes, opts3)
  } else if (ucd_option == "ICD-10 130 Cause List (Infants)") {
    stop("Not implemented yet")
  }
  return(opts3)
}


#' Final Multiple Cause of Death, Injury Intent and Mechanism Results
#'
#' Download Multiple Cause of Death, 2018-2021,
#' Single Race death certificate data
#' from \url{https://wonder.cdc.gov/mcd-icd10-expanded.html}
#'
#' @param wonder_url CDC WONDER url with the 'session id'. See
#' [session_mcd_final18()]
#' @param save Save the query instead of downloading data from WONDER
#' @param group_by_1 One of
#' `r paste("\x0a*", tothewonder:::MCD_FINAL18_GROUP_BY_1_OPTS, collapse = "")`
#' @param group_by_2 One of
#' `r paste("\x0a*", tothewonder:::MCD_FINAL18_GROUP_BY_2_OPTS, collapse = "")`
#' @param group_by_3 One of
#' `r paste("\x0a*", tothewonder:::MCD_FINAL18_GROUP_BY_2_OPTS, collapse = "")`
#' @param group_by_4 One of
#' `r paste("\x0a*", tothewonder:::MCD_FINAL18_GROUP_BY_2_OPTS, collapse = "")`
#' @param show_age_adjusted Show age adjusted values and confidence bands
#' @param show_totals Show totals
#' @param show_confidence_interval Show the confidence interval of the rate
#' @param show_standard_error Show the standard error of the rate,
#' @param residence_fips "All" or a list of state/county FIPS codes
#' @param residence_urbanization_year One of
#' * 2006
#' * 2013
#' @param residence_urbanization One or more of
#' `r paste("\x0a*", tothewonder:::URBANIZATION_OPTS, collapse = "")`
#' @param age One or more of
#' `r paste("\x0a*", unique(tothewonder:::ALL_AGE_GROUPS_OPTS), collapse = "")`
#' @param gender One or more of
#' `r paste("\x0a*", tothewonder:::GENDER_OPTS, collapse = "")`
#' @param hispanic_origin One or more of
#' `r paste("\x0a*", tothewonder:::HISPANIC_ORIGIN_OPTS, collapse = "")`
#' @param race_option One or more of
#' `r paste("\x0a*", tothewonder:::RACE_OPTIONS_OPTS, collapse = "")`
#' @param race One or more of
#' `r paste("\x0a*", tothewonder:::RACE_18_OPTS, collapse = "")`
#' @param period A vector of years/months in the interval
#' 2018/01:2021/12 for months (or last year available)
#' @param weekday One or more of
#' `r paste("\x0a*", tothewonder:::WEEKDAY_OPTS, collapse = "")`
#' @param autopsy One or more of
#' `r paste("\x0a*", tothewonder:::AUTOPSY_OPTS, collapse = "")`
#' @param place_of_death One or more of
#' `r paste("\x0a*", tothewonder:::PLACE_OF_DEATH_OPTS, collapse = "")`
#' @param ucd_option One of
#' `r paste("\x0a*", tothewonder:::UCD_OPTS, collapse = "")`
#' @param ucd_injury_intent One or more of
#' `r paste("\x0a*", tothewonder:::INJURY_INTENT_OPTS, collapse = "")`
#' @param ucd_injury_mechanism One or more of
#' `r paste("\x0a*", tothewonder:::INJURY_MECHANISM_OPTS, collapse = "")`
#' @param ucd_drug_alcohol One or more of
#' `r paste("\x0a*", tothewonder:::DRUG_ALCOHOL_OPTS, collapse = "")`
#' @param ucd_icd_codes An ICD-10 code: Visit
#' \url{https://wonder.cdc.gov/wonder/help/
#' mcd-provisional.html#ICD-10\%20Codes}
#' for the complete list
#' @param ucd_cause_113 One or more of
#' `r paste("\x0a*", tothewonder:::ICD10_113_LIST_OPTS, collapse = "")`
#' @param mcd_option One or more of
#' `r paste("\x0a*", tothewonder:::MCD_MCD_OPTS, collapse = "")`
#' @param mcd_drug_alcohol One or more of
#' `r paste("\x0a*", tothewonder:::DRUG_ALCOHOL_OPTS, collapse = "")`
#' @param mcd_drug_alcohol_and One or more of
#' `r paste("\x0a*", tothewonder:::DRUG_ALCOHOL_OPTS, collapse = "")`
#' @param mcd_icd_codes An ICD-10 code: See
#' \url{https://wonder.cdc.gov/wonder/help/
#' mcd-provisional.html#MCD\%20-\%20ICD-10\%20Codes}
#' for the complete list
#' @param mcd_icd_codes_and An ICD-10 code: See
#' \url{https://wonder.cdc.gov/wonder/help/mcd-provisional.html
#' #MCD\%20-\%20ICD-10\%20Codes}
#' for the complete list
#' @param mcd_infant_list Not implemented
#' @param mcd_infant_list_and Not implemented
#' @param mcd_cause_113 One or more of
#' `r paste("\x0a*", tothewonder:::ICD10_113_LIST_OPTS, collapse = "")`
#' @param mcd_cause_113_and One or more of
#' `r paste("\x0a*", tothewonder:::ICD10_113_LIST_OPTS, collapse = "")`
#'
#' @return A data.frame with the data from wonder
#' @export
#'
#' @md
mcd_final18 <- function(wonder_url,
                            save = FALSE,
                            group_by_1 = MCD_FINAL18_GROUP_BY_1_OPTS,
                            group_by_2 = MCD_FINAL18_GROUP_BY_2_OPTS,
                            group_by_3 = MCD_FINAL18_GROUP_BY_2_OPTS,
                            group_by_4 = MCD_FINAL18_GROUP_BY_2_OPTS,
                            show_age_adjusted = FALSE,
                            show_totals = FALSE,
                            show_confidence_interval = FALSE,
                            show_standard_error = FALSE,
                            residence_fips = "All",
                            residence_urbanization_year = c("2006", "2013"),
                            residence_urbanization = URBANIZATION_OPTS,
                            age = ALL_AGE_GROUPS_OPTS,
                            gender = GENDER_OPTS,
                            hispanic_origin = HISPANIC_ORIGIN_OPTS,
                            race_option = RACE_OPTIONS_OPTS,
                            race = "All",
                            period = "All",
                            weekday = WEEKDAY_OPTS,
                            autopsy = AUTOPSY_OPTS,
                            place_of_death = PLACE_OF_DEATH_OPTS,
                            ucd_option = MCD_UCD_OPTS,
                            ucd_injury_intent = INJURY_INTENT_OPTS,
                            ucd_injury_mechanism = INJURY_MECHANISM_OPTS,
                            ucd_drug_alcohol = DRUG_ALCOHOL_OPTS,
                            ucd_icd_codes = "All",
                            ucd_cause_113 = ICD10_113_LIST_OPTS,
                            mcd_option = MCD_MCD_OPTS,
                            mcd_drug_alcohol = DRUG_ALCOHOL_OPTS,
                            mcd_drug_alcohol_and = DRUG_ALCOHOL_OPTS,
                            mcd_icd_codes = "All",
                            mcd_icd_codes_and = "All",
                            mcd_infant_list = "All",
                            mcd_infant_list_and = "All",
                            mcd_cause_113 = ICD10_113_LIST_OPTS,
                            mcd_cause_113_and = ICD10_113_LIST_OPTS) {
  if (missing(wonder_url)) {
    stop("wonder_url is missing")
  }
  if (!grepl(
    "^https://wonder.cdc.gov/controller/datarequest/D157;jsessionid=",
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
  list2env(replace_all_with_correct_text(
    residence_urbanization = residence_urbanization,
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
  opts3 <- MCD_FINAL18_FORM_OPTIONS

  ##########################################################################
  ## params for group by
  group_by_1 <- rlang::arg_match(group_by_1, MCD_FINAL18_GROUP_BY_1_OPTS)
  group_by_2 <- rlang::arg_match(group_by_2, MCD_FINAL18_GROUP_BY_2_OPTS)
  group_by_3 <- rlang::arg_match(group_by_3, MCD_FINAL18_GROUP_BY_2_OPTS)
  group_by_4 <- rlang::arg_match(group_by_4, MCD_FINAL18_GROUP_BY_2_OPTS)

  v <- c(group_by_1, group_by_2, group_by_3, group_by_4)
  if (length(v[v != "None"]) != length(unique(v[v != "None"]))) {
    stop("group_bys must be unique")
  }
  opts3$B_1 <- dplyr::recode(group_by_1, !!!MCD_FINAL18_GROUP_BY_1_KEY)
  opts3$B_2 <- dplyr::recode(group_by_2, !!!MCD_FINAL18_GROUP_BY_2_KEY)
  opts3$B_3 <- dplyr::recode(group_by_3, !!!MCD_FINAL18_GROUP_BY_2_KEY)
  opts3$B_4 <- dplyr::recode(group_by_4, !!!MCD_FINAL18_GROUP_BY_2_KEY)


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
    opts3$`M_32` <- "D157.M32"
  }
  if (show_standard_error) {
    opts3$`M_31` <- "D157.M31"
  }

  ############################################################################
  ## FIPS residence code for states/counties
  residence_urbanization_year <- rlang::arg_match(residence_urbanization_year,
                                                  c("2006", "2013"))
  residence_urbanization <- rlang::arg_match(residence_urbanization,
                                             URBANIZATION_OPTS,
                                             multiple = TRUE)
  ## Add a leading zero to FIPS codes if length eq 4
  residence_fips <- ifelse(nchar(residence_fips) == 4,
    paste0("0", residence_fips),
    as.character(residence_fips)
  )
  residence_fips <- ifelse(nchar(residence_fips) == 1,
                           paste0("0", residence_fips),
                           as.character(residence_fips)
  )
  if (!all(residence_fips %in% c("All", FIPS_MCD))) {
    w_txt <- paste0("Looks like you are using a FIPS code unknown to WONDER. ",
                    "This may cause the query to error.")
    warning(w_txt, "\n", paste0(
      setdiff(residence_fips, FIPS_MCD),
      collapse = ","))
  }
  opts3$`O_location` <- "D157.V9"
  if (any(residence_fips == "All")) {
    residence_fips <- "*All*"
  }
  opts3 <- append_list("F_D157.V9", residence_fips, opts3)
  # opts3$"I_D157.V9" <- gsub(
  #   "%20", "+",
  #   utils::URLencode(
  #     paste(
  #       dplyr::recode(sort(residence_fips), !!!FIPS_key)
  #     ),
  #     reserved = TRUE
  # )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
  # opts3$"I_D157.V9" <- paste0(opts3$"I_D157.V9", "%0D%0A")

  ## param residence urbanization
  if (residence_urbanization_year == 2013) {
    urb_key <- "V_D157.V19"
    opts3$`O_urban` <- "D157.V19"
    opts3$`V_D157.V11` <- "*All*"
  } else if (residence_urbanization_year == 2006) {
    urb_key <- "V_D157.V11"
    opts3$`O_urban` <- "D157.V11"
    opts3$`V_D157.V19` <- "*All*"
  }
  ## fill list with urban codes
  opts3 <- append_list_recode(
    urb_key, residence_urbanization,
    URBANIZATION_KEY, opts3
  )
#
#   ##########################################################
#   ## FIPS occurrence  for states/counties
#   rlang::arg_match(occurrence_urbanization_year, c("2006", "2013"))
#   rlang::arg_match(occurrence_urbanization, URBANIZATION_OPTS,
#                   multiple = TRUE)
#   occurrence_fips <- ifelse(nchar(occurrence_fips) == 4,
#     paste0("0", occurrence_fips),
#     as.character(occurrence_fips)
#   )
#   if (!all(occurrence_fips %in% c("All", FIPS_MCD))) {
#     warning("Looks like you are using a FIPS code unknown to WONDER.
#              This may cause the query to error.")
#     warning(setdiff(occurrence_fips, FIPS_MCD))
#   }
#   opts3$`O_death_location` <- "D157.V79"
#   if (any(occurrence_fips == "All")) {
#     occurrence_fips <- "*All*"
#   }
#   opts3 <- append_list("F_D157.V79", occurrence_fips, opts3)
#   ## param occurrence urbanization
#   if (occurrence_urbanization_year == 2013) {
#     urb_key <- "D157.V89"
#     opts3$`O_death_urban` <- "D157.V89"
#     opts3$`D157.V81` <- "*All*"
#   } else if (occurrence_urbanization_year == 2006) {
#     urb_key <- "D157.V81"
#     opts3$`O_death_urban` <- "D157.V81"
#     opts3$`D157.V89` <- "*All*"
#   }
#   ## fill list with urban codes
#   opts3 <- append_list_recode(
#     urb_key, occurrence_urbanization,
#     URBANIZATION_KEY, opts3
#   )

  ###########################################################################
  ## Age
  stopifnot(is.logical(show_age_adjusted))
  if (!is.character(age)) {
    age <- as.character(age)
  }
  age <- rlang::arg_match(age, ALL_AGE_GROUPS_OPTS, multiple = TRUE)
  ## age param
  ## Population and Rates are not available when values are selected for
  ## Single-Year Ages or Ten-Year Age Groups in section 3 so default to
  ## five-year age groups
  if (all(age %in% FIVE_YEAR_AGE_GROUPS_OPTS)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!FIVE_YEAR_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <- "V_D157.V51"
      opts3$`O_age` <- "D157.V51"
      opts3$`O_aar` <- "aar_none"
    }
  }  else if (all(age %in% TEN_YEAR_AGE_GROUPS_OPTS)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!TEN_YEAR_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <- "V_D157.V5"
      opts3$`O_age` <- "D157.V5"
    }
    # Add age adjusted column if 2 or more ten-year age groups are requested
    if ((length(age) >= 2 || identical(age, "All Ages")) && show_age_adjusted) {
      opts3$`O_aar_enable` <- "true"
      opts3$`O_aar` <- "aar_std"
      opts3$`O_aar_CI` <- "true"
      opts3$`O_aar_SE` <- "true"
    } else {
      opts3$`O_aar` <- "aar_none"
      opts3$`O_aar_CI` <- "false"
      opts3$`O_aar_SE` <- "false"
    }
  } else if (all(age %in% SINGLE_YEAR_AGES_OPTS)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!SINGLE_YEAR_AGES_KEY)
      names(opts3)[length(opts3)] <- "V_D157.V52"
      opts3$`O_age` <- "D157.V52"
      opts3$`O_aar` <- "aar_none"
    }
  } else if (all(age %in% INFANT_AGE_GROUPS_OPTS)) {
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!INFANT_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <- "V_D157.V6"
      opts3$`O_age` <- "D157.V6"
      opts3$`O_aar` <- "aar_none"
    }
  } else {
    stop("Invalid age groups")
  }

  ##########################################################################
  ## Gender
  gender <- rlang::arg_match(gender, GENDER_OPTS, multiple = TRUE)
  opts3 <- append_list_recode("V_D157.V7", gender, GENDER_KEY, opts3)

  ###########################################################################
  ## Hispanic
  hispanic_origin <- rlang::arg_match(hispanic_origin,
                                      HISPANIC_ORIGIN_OPTS,
                                      multiple = TRUE)
  opts3 <- append_list_recode(
    "V_D157.V17",
    hispanic_origin, HISPANIC_ORIGIN_KEY, opts3
  )

  ##########################################################################
  ## race
  race_option <- rlang::arg_match(race_option, RACE_OPTIONS_OPTS)
  if (race_option == "Single Race 6") {
    race <- rlang::arg_match(race, SINGLE_RACE_6_OPTS, multiple = TRUE)
    opts3$`O_race` <- "D157.V42"
    opts3$`V_D157.V43` <- "*All*"
    opts3$`V_D157.V44` <- "*All*"
    opts3 <- append_list_recode("V_D157.V42", race, SINGLE_RACE_6_KEY, opts3)
  } else if (race_option == "Single Race 15") {
    race <- rlang::arg_match(race, SINGLE_RACE_15_OPTS, multiple = TRUE)
    opts3$`O_race` <- "D157.V43"
    opts3$`V_D157.V42` <- "*All*"
    opts3$`V_D157.V44` <- "*All*"
    opts3 <- append_list_recode("V_D157.V43", race, SINGLE_RACE_15_KEY, opts3)
  } else if (race_option == "Single/Multi Race 31") {
    race <- rlang::arg_match(race, SINGLE_MULTI_RACE_31_OPTS, multiple = TRUE)
    opts3$`O_race` <- "D157.V44"
    opts3$`V_D157.V42` <- "*All*"
    opts3$`V_D157.V43` <- "*All*"
    opts3 <- append_list_recode(
      "V_D157.V44", race, SINGLE_MULTI_RACE_31_KEY,
      opts3
    )
  }


  #########################################################################
  ## period param
  ## check period is in the right format


  if (any(period == "All")) {
    period <- "*All*"
  } else {
    if (!all(nchar(period) %in% c(4, 7))) {
      stop(paste0("period must follow the pattern '%Y/%m' or '%Y'",
                  "(e.g. 2002 or 1999/01 or 2020/11)"))
    }
    if (!all(grepl("^[[:digit:]]{4}|[[:digit:]]{4}/[[:digit:]]{2}$", period))) {
      stop(paste0("period must follow the pattern '%Y/%m' or '%Y'",
                  "(e.g. 2002 or 1999/01 or 2020/11)"))
    }
    if (any(as.numeric(gsub(
      "([0-9]{4}).*", "\\1",
      period
    )) < 2018)) {
      stop("period must be greater than or equal to 2018")
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
      stop(paste0("period must follow the pattern '%Y/%m' or '%Y'",
                  "(e.g. 2002 or 1999/01 or 2020/11)"))
    }
  }
  #opts3$`O_dates` <- "YEAR"
  #opts3$`F_D157.V100` <- "*All*"
  opts3 <- append_list("F_D157.V1", period, opts3)

  ##########################################################################
  ## weekday
  weekday <- rlang::arg_match(weekday, WEEKDAY_OPTS, multiple = TRUE)
  opts3 <- append_list_recode("V_D157.V24", weekday, WEEKDAY_KEY, opts3)


  ##########################################################################
  ## autopsy
  autopsy <- rlang::arg_match(autopsy, AUTOPSY_OPTS, multiple = TRUE)
  opts3 <- append_list_recode("V_D157.V20", autopsy, AUTOPSY_KEY, opts3)

  ##########################################################################
  ## place of death
  place_of_death <- rlang::arg_match(place_of_death,
                                     PLACE_OF_DEATH_OPTS,
                                     multiple = TRUE)
  opts3 <- append_list_recode(
    "V_D157.V21", place_of_death,
    PLACE_OF_DEATH_KEY, opts3
  )

  ##########################################################################
  ## Underlying cause of death
  ucd_option <- rlang::arg_match(ucd_option, MCD_UCD_OPTS)

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
  if (ucd_option == "Drug/Alcohol Induced Causes")
    ucd_drug_alcohol <- rlang::arg_match(ucd_drug_alcohol,
                                         DRUG_ALCOHOL_OPTS,
                                         multiple = TRUE)
  if (ucd_option == "ICD-10 113 Cause List")
    ucd_cause_113 <- rlang::arg_match(ucd_cause_113,
                                      ICD10_113_LIST_OPTS,
                                      multiple = TRUE)
  if (ucd_option == "Injury Intent and Mechanism") {
    ucd_injury_intent <- rlang::arg_match(ucd_injury_intent,
                                          INJURY_INTENT_OPTS,
                                          multiple = TRUE)
    ucd_injury_mechanism <- rlang::arg_match(ucd_injury_mechanism,
                                             INJURY_MECHANISM_OPTS,
                                             multiple = TRUE
    )
  }

  opts3 <- process_ucd_mcod_final18(
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
  mcd_option <- rlang::arg_match(mcd_option, MCD_MCD_OPTS)

  opts3$O_mcd <- dplyr::recode(mcd_option, !!!MCD_FINAL18_MCD_KEY)
  if (mcd_option == "MCD - Drug/Alcohol Induced Causes") {
    mcd_drug_alcohol <- rlang::arg_match(mcd_drug_alcohol,
                                         DRUG_ALCOHOL_OPTS,
                                         multiple = TRUE)
    mcd_drug_alcohol_and <- rlang::arg_match(mcd_drug_alcohol_and,
                                             DRUG_ALCOHOL_OPTS,
                                             multiple = TRUE)

    opts3$"V_D157.V26" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(mcd_drug_alcohol, !!!DRUG_ALCOHOL_KEY),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D157.V26" <- I(paste0(opts3$"V_D157.V26", "%0D%0A"))

    # opts3$"V_D157.V26" <- utils::URLencode(paste(gsub(
    #   " ",
    #   "+",
    #   dplyr::recode(
    #     mcd_drug_alcohol,
    #     !!!DRUG_ALCOHOL_KEY
    #   )
    # ),
    # collapse = "+\r\n"
    # ))

    opts3$"V_D157.V26_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(mcd_drug_alcohol_and, !!!DRUG_ALCOHOL_KEY),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A

    opts3$"V_D157.V26_AND" <- I(paste0(opts3$"V_D157.V26_AND", "%0D%0A"))
    # opts3$"V_D157.V26_AND" <- utils::URLencode(paste(gsub(
    #   " ",
    #   "+",
    #   dplyr::recode(
    #     mcd_drug_alcohol_and,
    #     !!!DRUG_ALCOHOL_KEY
    #   )
    # ),
    # collapse = "+\r\n"
    # ))
  } else if (mcd_option == "MCD - ICD-10 113 Cause List") {
    mcd_cause_113 <- rlang::arg_match(mcd_cause_113,
                                      ICD10_113_LIST_OPTS,
                                      multiple = TRUE)
    mcd_cause_113_and <- rlang::arg_match(mcd_cause_113_and,
                                          ICD10_113_LIST_OPTS,
                                          multiple = TRUE)

    opts3$"V_D157.V15" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(
          mcd_cause_113,
          !!!ICD10_113_LIST_KEY
        ),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D157.V15" <- I(paste0(opts3$"V_D157.V15", "%0D%0A"))
    opts3$"V_D157.V15_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        dplyr::recode(
          mcd_cause_113_and,
          !!!ICD10_113_LIST_KEY
        ),
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D157.V15_AND" <- I(paste0(opts3$"V_D157.V15_AND", "%0D%0A"))

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
    check_mcd_final_icd_codes(mcd_icd_codes)
    check_mcd_final_icd_codes(mcd_icd_codes_and)
    opts3$"V_D157.V13" <- gsub(
      "%20", "+",
      utils::URLencode(
        mcd_icd_codes,
        reserved = TRUE
    )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D157.V13" <- I(paste0(opts3$"V_D157.V13", "%0D%0A"))
    opts3$"V_D157.V13_AND" <- gsub(
      "%20", "+",
      utils::URLencode(
        mcd_icd_codes_and,
        reserved = TRUE
      )) |> paste0(collapse = "%0D%0A") # \r\n == %0D%0A
    opts3$"V_D157.V13_AND" <- I(paste0(opts3$"V_D157.V13_AND", "%0D%0A"))
  } else if (mcd_option == "MCD - ICD-10 130 Cause List (Infants)") {
    stop("Not implemented yet")
  }

  ##############################################################################
  ## Save query
  if (save) {

    ## To save a location/period/drug-alcohol/icd10 code item
    ## you have to open up the selection box so that subitems
    ## are visible
    ## Change the query to a simple one to avoid hitting the
    ## WONDER server
    simple_mcd_form2 <- SIMPLE_FINAL18_MCD_FORM
    ## remove all keys that match select items to open
    for (key in c("F_D157.V9",
                 "F_D157.V1",
                 "F_D157.V25", "F_D157.V2",
                 "F_D157.V26", "F_D157.V13"))
      simple_mcd_form2 <- simple_mcd_form2[- which(names(simple_mcd_form2) ==
                                                      key)]

    VALID_YEARS <- wonder_years("D157")
    if (!identical(period, "*All*") && any(as.numeric(gsub(
      "([0-9]{4}).*", "\\1",
      period
    )) > max(as.numeric(VALID_YEARS)))) {
      stop(paste0("period must be smaller or equal to ",
                  max(as.numeric(VALID_YEARS))))
    }
    open_item(simple_mcd_form2, opts3, "Open",
              "F_D157.V9", STATE_FIPS, wonder_url)
    open_item(simple_mcd_form2, opts3, "Open",
              "F_D157.V1", VALID_YEARS, wonder_url)

    if (ucd_option == "Drug/Alcohol Induced Causes")
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D157.V25", c("A", "O"), wonder_url)
    else if (ucd_option == "ICD-10 Codes")
      open_item(simple_mcd_form2, opts3, "Open Fully",
                "F_D157.V2", UCD_ICD10_TOP_ITEMS_MCD_FINAL18, wonder_url)
    if (mcd_option == "MCD - Drug/Alcohol Induced Causes")
      open_item(simple_mcd_form2, opts3, "Open",
                "F_D157.V26", c("A", "O", "N"), wonder_url)
    else if (mcd_option == "MCD - ICD-10 Codes")
      open_item(simple_mcd_form2, opts3, "Open Fully",
                "F_D157.V13", MCD_ICD10_TOP_ITEMS_MCD_FINAL18, wonder_url)
    opts3$`action-Save` <- "Save"
    opts3$`action-Send` <- NULL
  }

  opts3$"dataset_label" <- I(opts3$"dataset_label")
  #opts3$"dataset_vintage" <- I("May+21%2C+2022+as+of+June+05%2C+2022")

  ## 1) Query WONDER
  res <- query_wonder(wonder_url, opts3)
  wonder_text <- httr::content(res, as = "text")
  if (save) {
    saved_query <- extract_saved(wonder_text, "D157")
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
  #   saved_query <- extract_saved(saved_query_text, "D157")
  #   return (saved_query)
  # }

  ## 2) Export data from WONDER as tab separated text
  res <- query_wonder(wonder_url, MCD_FINAL18_DOWNLOAD_FORM)




  text_table <- httr::content(res, as = "text")
  df <- utils::read.csv(text = text_table, sep = "\t")

  Sys.sleep(3)
  return(df)
}
