

check_params <- function(wonder_url,
                         group_by_1 = group_by_1_opts,
                         group_by_2 = group_by_2_opts,
                         group_by_3 = group_by_2_opts,
                         group_by_4 = group_by_2_opts,
                         show_age_adjusted,
                         show_totals,
                         show_confidence_interval,
                         show_standard_error,
                         residence_fips,
                         residence_urbanization_year = c("2006", "2013"),
                         residence_urbanization = urbanization_opts,
                         age = all_age_groups_opts,
                         gender = gender_opts,
                         hispanic_origin = hispanic_origin_opts,
                         race = race_opts,
                         period,
                         weekday = weekday_opts,
                         autopsy = autopsy_opts,
                         place_of_death = place_of_death_opts,
                         ucd_option = ucd_opts) {
  ## Argument checking
  defined <- ls()
  passed <- names(as.list(match.call())[-1])

  if (any(!defined %in% passed)) {
    stop(paste("Missing values for", paste(setdiff(defined, passed),
      collapse = ", "
    )))
  }
  rlang::arg_match(group_by_1, group_by_1_opts)
  rlang::arg_match(group_by_2, group_by_2_opts)
  rlang::arg_match(group_by_3, group_by_2_opts)
  rlang::arg_match(group_by_4, group_by_2_opts)
  stopifnot(is.logical(show_age_adjusted))
  stopifnot(is.logical(show_totals))
  stopifnot(is.logical(show_confidence_interval))
  stopifnot(is.logical(show_standard_error))
  rlang::arg_match(gender, gender_opts, multiple = TRUE)
  rlang::arg_match(residence_urbanization_year, c("2006", "2013"))
  rlang::arg_match(residence_urbanization, urbanization_opts, multiple = TRUE)
  if (!is.character(age)) {
    age <- as.character(age)
  }
  rlang::arg_match(age, all_age_groups_opts, multiple = TRUE)
  rlang::arg_match(hispanic_origin, hispanic_origin_opts, multiple = TRUE)
  rlang::arg_match(race, race_opts, multiple = TRUE)
  rlang::arg_match(weekday, weekday_opts, multiple = TRUE)
  rlang::arg_match(autopsy, autopsy_opts, multiple = TRUE)
  rlang::arg_match(place_of_death, place_of_death_opts, multiple = TRUE)
  rlang::arg_match(ucd_option, ucd_opts)

  ## Build the query
  opts2 <- ucd_form_options

  ## check period is in the right format
  if (any(period == "All")) {
    period <- "*All*"
  } else {
    if (!all(nchar(period) %in% c(4, 7))) {
      stop("period must follow the pattern '%Y/%m' or '%Y' (e.g. 2002 or 1999/01 or 2020/11)")
    }
    if (any(as.numeric(gsub(
      "([0-9]{4}).*", "\\1",
      period
    )) < 1999)) {
      stop("period must be greater than 1999")
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
  for (i in period) {
    opts2[length(opts2) + 1] <- as.character(i)
    names(opts2)[length(opts2)] <- "F_D76.V1"
  }

  ## age param
  if (all(age %in% ten_year_age_groups_opts)) {
    for (i in age) {
      opts2[length(opts2) + 1] <- dplyr::recode(i, !!!ten_year_age_groups_key)
      names(opts2)[length(opts2)] <- "V_D76.V5"
      opts2$`O_age` <- "D76.V5"
    }
    # Add age adjusted column if 2 or more ten-year age groups are requested
    if ((length(age) >= 2 | identical(age, "All Ages")) & show_age_adjusted) {
      opts2$`O_aar_enable` <- "true"
      opts2$`O_aar` <- "aar_std"
      opts2$`O_aar_CI` <- "true"
      opts2$`O_aar_SE` <- "true"
    } else {
      opts2$`O_aar` <- "aar_none"
      opts2$`O_aar_CI` <- "false"
      opts2$`O_aar_SE` <- "false"
    }
  } else if (all(age %in% five_year_age_groups_opts)) {
    for (i in age) {
      opts2[length(opts2) + 1] <- dplyr::recode(i, !!!five_year_age_groups_key)
      names(opts2)[length(opts2)] <- "V_D76.V51"
      opts2$`O_age` <- "D76.V51"
      opts2$`O_aar` <- "aar_none"
    }
  } else if (all(age %in% single_year_ages_opts)) {
    for (i in age) {
      opts2[length(opts2) + 1] <- dplyr::recode(i, !!!single_year_ages_key)
      names(opts2)[length(opts2)] <- "V_D76.V52"
      opts2$`O_age` <- "D76.V52"
      opts2$`O_aar` <- "aar_none"
    }
  } else if (all(age %in% infant_age_groups_opts)) {
    for (i in age) {
      opts2[length(opts2) + 1] <- dplyr::recode(i, !!!infant_age_groups_key)
      names(opts2)[length(opts2)] <- "V_D76.V6"
      opts2$`O_age` <- "D76.V6"
      opts2$`O_aar` <- "aar_none"
    }
  } else {
    stop("Invalid age groups")
  }

  # params for uncertainaty options
  if (!show_totals) {
    opts2$`O_show_totals` <- "false"
  }
  if (show_confidence_interval) {
    opts2$`M_32` <- "D76.M32"
  }
  if (show_standard_error) {
    opts2$`M_31` <- "D76.M31"
  }

  ## params for group by
  v <- c(group_by_1, group_by_2, group_by_3, group_by_4)
  if (length(v[v != "None"]) != length(unique(v[v != "None"]))) {
    stop("group_bys must be unique")
  }
  opts2$B_1 <- dplyr::recode(group_by_1, !!!group_by_1_key)
  opts2$B_2 <- dplyr::recode(group_by_2, !!!group_by_2_key)
  opts2$B_3 <- dplyr::recode(group_by_3, !!!group_by_2_key)
  opts2$B_4 <- dplyr::recode(group_by_4, !!!group_by_2_key)

  ## FIPS code for states/counties
  residence_fips <- ifelse(nchar(residence_fips) == 4,
    paste0("0", residence_fips),
    as.character(residence_fips)
  )
  if (!all(residence_fips %in% c("All", states_fips, counties_fips))) {
    warning("Looks like you are using a FIPS code unknown to WONDER. This may cause the query to error.")
    warning(setdiff(residence_fips, c(states_fips, counties_fips)))
  }
  for (fips in residence_fips) {
    if (fips == "All") {
      fips <- "*All*"
    }
    opts2[length(opts2) + 1] <- fips
    names(opts2)[length(opts2)] <- "F_D76.V9"
  }

  ## param residence_urbanization
  if (residence_urbanization_year == 2013) {
    urb_key <- "V_D76.V19"
    opts2$`O_urban` <- "D76.V19"
    opts2$`V_D76.V11` <- "*All*"
  } else if (residence_urbanization_year == 2006) {
    urb_key <- "V_D76.V11"
    opts2$`O_urban` <- "D76.V11"
    opts2$`V_D76.V19` <- "*All*"
  }

  ## Every other param
  purrr::pwalk(
    .l = list(
      list(
        residence_urbanization,
        gender, hispanic_origin, race,
        weekday, autopsy,
        place_of_death
      ),
      c(
        urb_key,
        "V_D76.V7", "V_D76.V17", "V_D76.V8",
        "V_D76.V24", "V_D76.V20",
        "V_D76.V21"
      ),
      list(
        urbanization_key,
        gender_key, hispanic_origin_key, race_key,
        weekday_key, autopsy_key,
        place_of_death_key
      )
    ),
    .f = ~ for (i in ..1) {
      opts2[length(opts2) + 1] <<- dplyr::recode(i, !!!..3)
      names(opts2)[length(opts2)] <<- ..2
    }
  )

  return(opts2)
}

process_ucd <- function(opts2,
                        ucd_option,
                        ucd_injury_intent,
                        ucd_injury_mechanism,
                        ucd_drug_alcohol,
                        ucd_icd_codes,
                        ucd_cause_113) {
  set_ucd_opts <- function(ll, opts2) {
    purrr::pwalk(
      .l = ll,
      .f = ~ for (i in ..1) {
        # if the call has 3 arguments (4 including .f) should use the key
        if (length(match.call()) == 4) {
          opts2[length(opts2) + 1] <<- dplyr::recode(i, !!!..3)
        } else {
          opts2[length(opts2) + 1] <<- i
        }
        names(opts2)[length(opts2)] <<- ..2
      }
    )
    return(opts2)
  }
  opts2$`O_ucd` <- dplyr::recode(ucd_option, !!!ucd_key)
  if (ucd_option == "Injury Intent and Mechanism") {
    rlang::arg_match(ucd_injury_intent, injury_intent_opts, multiple = TRUE)
    rlang::arg_match(ucd_injury_mechanism, injury_mechanism_opts,
      multiple = TRUE
    )
    opts2 <- set_ucd_opts(
      list(
        list(ucd_injury_intent, ucd_injury_mechanism),
        c("V_D76.V22", "V_D76.V23"),
        list(injury_intent_key, injury_mechanism_key)
      ),
      opts2
    )
  } else if (ucd_option == "Drug/Alcohol Induced Causes") {
    rlang::arg_match(ucd_drug_alcohol, drug_alcohol_opts, multiple = TRUE)
    opts2 <- set_ucd_opts(
      list(
        list(ucd_drug_alcohol),
        c("F_D76.V25"),
        list(drug_alcohol_key)
      ),
      opts2
    )
  } else if (ucd_option == "ICD-10 113 Cause List") {
    rlang::arg_match(ucd_cause_113, icd10_113_list_opts, multiple = TRUE)
    opts2 <- set_ucd_opts(
      list(
        list(ucd_cause_113),
        c("V_D76.V4"),
        list(icd10_113_list_key)
      ),
      opts2
    )
  } else if (ucd_option == "ICD-10 Codes") {
    opts2 <- set_ucd_opts(
      list(
        list(ucd_icd_codes),
        c("F_D76.V2")
      ),
      opts2
    )
  } else if (ucd_option == "ICD-10 130 Cause List (Infants)") {
    stop("Not implemented yet")
  }
  return(opts2)
}

#' Underlying Cause of Death, Injury Intent and Mechanism Results
#'
#' Download UCD 1999-2020 CDC WONDER death certificate data from \url{https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html}
#'
#' @param wonder_url CDC WONDER url with the 'session id'. See
#' [session_ucd99()]
#' @param save Save the query instead of downloading data from WONDER
#' @param group_by_1 One of `r paste("\x0a*", tothewonder:::group_by_1_opts, collapse = "")`
#' @param group_by_2 One of `r paste("\x0a*", tothewonder:::group_by_2_opts, collapse = "")`
#' @param group_by_3 One of `r paste("\x0a*", tothewonder:::group_by_2_opts, collapse = "")`
#' @param group_by_4 One of `r paste("\x0a*", tothewonder:::group_by_2_opts, collapse = "")`
#' @param show_age_adjusted Show age adjusted values and confidence bands
#' @param show_totals Show totals
#' @param show_confidence_interval Show the confidence interval of the rate
#' @param show_standard_error Show the standard error of the rate,
#' @param residence_fips "All" or a list of state/county FIPS codes
#' @param residence_urbanization_year One of
#' * 2006
#' * 2013
#' @param residence_urbanization One or more of `r paste("\x0a*", tothewonder:::urbanization_opts, collapse = "")`
#' @param age One or more of `r paste("\x0a*", unique(tothewonder:::all_age_groups_opts), collapse = "")`
#' @param gender One or more of `r paste("\x0a*", tothewonder:::gender_opts, collapse = "")`
#' @param hispanic_origin One or more of `r paste("\x0a*", tothewonder:::hispanic_origin_opts, collapse = "")`
#' @param race One or more of `r paste("\x0a*", tothewonder:::race_opts, collapse = "")`
#' @param period A vector of years/months in the interval 1999/01:2020/12 (or last year/month available)  to filter the data
#' @param weekday One or more of `r paste("\x0a*", tothewonder:::weekday_opts, collapse = "")`
#' @param autopsy One or more of `r paste("\x0a*", tothewonder:::autopsy_opts, collapse = "")`
#' @param place_of_death One or more of `r paste("\x0a*", tothewonder:::place_of_death_opts, collapse = "")`
#' @param ucd_option One of `r paste("\x0a*", tothewonder:::ucd_opts, collapse = "")`
#' @param ucd_injury_intent One or more of `r paste("\x0a*", tothewonder:::injury_intent_opts, collapse = "")`
#' @param ucd_injury_mechanism One or more of `r paste("\x0a*", tothewonder:::injury_mechanism_opts, collapse = "")`
#' @param ucd_drug_alcohol One or more of `r paste("\x0a*", tothewonder:::drug_alcohol_opts, collapse = "")`
#' @param ucd_icd_codes An ICD-10 code: See \url{https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html} for the complete list
#' @param ucd_infant_list Not implemented
#' @param ucd_cause_113 One or more of `r paste("\x0a*", tothewonder:::icd10_113_list_opts, collapse = "")`
#'
#' @return A data.frame with the data from wonder
#' @export
#'
#' @md
ucd99 <- function(wonder_url,
                  save = FALSE,
                  group_by_1 = group_by_1_opts,
                  group_by_2 = group_by_2_opts,
                  group_by_3 = group_by_2_opts,
                  group_by_4 = group_by_2_opts,
                  show_age_adjusted = FALSE,
                  show_totals = FALSE,
                  show_confidence_interval = FALSE,
                  show_standard_error = FALSE,
                  residence_fips = "All",
                  residence_urbanization_year = c("2006", "2013"),
                  residence_urbanization = urbanization_opts,
                  age = all_age_groups_opts,
                  gender = gender_opts,
                  hispanic_origin = hispanic_origin_opts,
                  race = race_opts,
                  period = "All",
                  weekday = weekday_opts,
                  autopsy = autopsy_opts,
                  place_of_death = place_of_death_opts,
                  ucd_option = ucd_opts,
                  ucd_injury_intent = injury_intent_opts,
                  ucd_injury_mechanism = injury_mechanism_opts,
                  ucd_drug_alcohol = drug_alcohol_opts,
                  ucd_icd_codes = "All",
                  ucd_infant_list = "All",
                  ucd_cause_113 = icd10_113_list_opts) {
  if (missing(wonder_url)) {
    stop("wonder_url is missing")
  }
  if (!grepl(
    "^https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=",
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
  list2env(replace_All(
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
    ucd_cause_113 = ucd_cause_113
  ),
  envir = environment()
  )

  opts2 <- check_params(
    wonder_url,
    group_by_1,
    group_by_2,
    group_by_3,
    group_by_4,
    show_age_adjusted,
    show_totals,
    show_confidence_interval,
    show_standard_error,
    residence_fips,
    residence_urbanization_year,
    residence_urbanization,
    age,
    gender,
    hispanic_origin,
    race,
    period,
    weekday,
    autopsy,
    place_of_death,
    ucd_option
  )

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
  opts2 <- process_ucd(
    opts2,
    ucd_option,
    ucd_injury_intent,
    ucd_injury_mechanism,
    ucd_drug_alcohol,
    ucd_icd_codes,
    ucd_cause_113
  )

  ## Save query
  if(save) {
    ## To save a location/period/drug-alcohol/icd10 code item
    ## you have to open up the selection box so that subitems
    ## are visible
    ## Change the query to a simple one to avoid hitting the
    ## WONDER server
    simple_ucd_form2 <- simple_ucd_form
    ## remove all keys that match select items to open
    for(keyname in c("F_D76.V9",   # residence
                 "F_D76.V1",   # period
                 "F_D76.V2",   # icd-10 codes
                 "F_D76.V25")) {# alcohol
      simple_ucd_form2 <- simple_ucd_form2[ - which(names(simple_ucd_form2) == keyname)]
  }

    open_item(simple_ucd_form2, opts2, "Open",
              "F_D76.V9", state_FIPS, wonder_url)
    open_item(simple_ucd_form2, opts2, "Open",
              "F_D76.V1",  as.character(1999:2020), wonder_url)
    if (ucd_option == "ICD-10 Codes")
      open_item(simple_ucd_form2, opts2, "Open Fully",
                "F_D76.V2", icd10_top_items_ucd, wonder_url)
    if (ucd_option == "Drug/Alcohol Induced Causes")
      open_item(simple_ucd_form2, opts2, "Open",
                "F_D76.V25", c("A", "O", "N"), wonder_url)

    opts2$`action-Save` <- "Save"
    opts2$`action-Send` <- NULL
  }

  ## 1) Query WONDER
  res <- query_wonder(wonder_url, opts2)
  wonder_text <- httr::content(res, as = "text")
  if(save){
    saved_query <- extract_saved(wonder_text, "D76")
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
  ## 2) Export data from WONDER as tab separated text
  res <- query_wonder(wonder_url, ucd_download_form)

  text_table <-  httr::content(res, as = "text")
  df <- utils::read.csv(text = text_table, sep = "\t")

  Sys.sleep(3)
  return(df)
}

## Print warnings/errors from WONDER
print_errors <- function(string) {
  warning(
    paste(
      sub(
        '"error-message".*nbsp;[\r\n\t]*(.*)</div>',
        "\\1",
        regmatches(
          string,
          gregexpr(
            '"error-message".*?nbsp;.*?</div>',
            string
          )
        )[[1]]
      ),
      collapse = "\n"
    )
  )
}

extract_saved <- function(string, db_num) {

  str <- paste0(".*copySavedUrlToClipboard\\('(https://wonder.cdc.gov/controller/saved/",
                db_num,
                "/[A-Z0-9]+)'\\).*")
  gsub(str,
       '\\1', string)
}
