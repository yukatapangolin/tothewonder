open_item <- function(ll, ll_old, open_type = "Open", key,
                      items, wonder_url) {
  if(is.null(ll_old[[key]]))
    return()
  if( grepl("All", ll_old[[key]]))
    return()
  ## "finder-action-D176.V9-Open"
  open_key <- paste0(
    "finder-action-",
    substring(key, 3, nchar(key)),
    "-", open_type)  # finder-action-D176.V13-Open or finder-action-D176.V13-Open+Fully
  ll[[open_key]] <- open_key
  ll <- append_list(key, items, ll)
  res <- query_wonder(wonder_url, ll)
  if (res$status_code != 200)
    stop(paste0("Error saving query. HTTP Status: ", res$status_code))
  ll[[open_key]]  <- NULL
  Sys.sleep(3)
}

append_list <- function(name, elem, ll) {
  if (length(name) != length(elem)) {
    name <- rep(name, length(elem))
  }
  for (i in seq_along(elem)) {
    ll[length(ll) + 1] <- elem[i]
    names(ll)[length(ll)] <- name[i]
  }
  return(ll)
}

append_list_recode <- function(name, elem, dict, ll) {
  if (length(name) != length(elem)) {
    name <- rep(name, length(elem))
  }
  for (i in seq_along(elem)) {
    ll[length(ll) + 1] <- dplyr::recode(elem[i], !!!dict)
    names(ll)[length(ll)] <- name[i]
  }
  return(ll)
}


query_wonder <- function(wurl, form_opts) {
  if (is.null(wurl)) {
    stop("wonder_url must no be set to NULL")
  }
  if (wurl == "") {
    stop("wonder_url must not be empty")
  }
  res <- httr::POST(
    httr::set_cookies(`s_cc` = "true"),
    url = wurl,
    body = form_opts,
    encode = "form",
    httr::add_headers(.headers = c(
      "user-agent" = get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
      "Accept-Language" = "en-us,en;q=0.5"
    ))
  )
  return(res)
}

check_icd_codes <- function(icd_codes) {
  if (!all(icd_codes %in% c("All Causes of Death", "All",
                            icd_codes_key, icd10_top_items_ucd))) {
    warning("Looks like you are using an ICD-10 code unknown to WONDER (https://wonder.cdc.gov/wonder/help/ucd.html#ICD-10%20Codes). This may cause the query to error.")
    warning(paste(setdiff(icd_codes, c(icd_codes_key,
                                       icd10_top_items_ucd)),
                  collapse = " "))
  }
}

check_mcod_icd_codes <- function(mcod_icd_codes) {
  if (!all(mcod_icd_codes %in% c("All Causes of Death", "All",
                                 mcod_icd_codes_key, icd10_top_items_mcd))) {
    warning("Looks like you are using an ICD-10 code unknown to WONDER (https://wonder.cdc.gov/wonder/help/ucd.html#ICD-10%20Codes). This may cause the query to error.")
    warning(paste(setdiff(mcod_icd_codes, c(mcod_icd_codes_key,
                                            icd10_top_items_mcd)),
                  collapse = " "))
  }
}

replace_All <- function(...) {
  tb <- list(
    residence_urbanization = "All Categories",
    occurrence_urbanization = "All Categories",
    age = "All Ages",
    gender = "All Genders",
    race = "All Races",
    hispanic_origin = "All Origins",
    weekday = "All Weekdays",
    autopsy = "All Values",
    place_of_death = "All Places",
    ucd_injury_intent = "All Causes of Death",
    ucd_injury_mechanism = "All Causes of Death",
    ucd_drug_alcohol = "All Causes of Death",
    ucd_cause_113 = "All Causes of Death",
    mcd_drug_alcohol = "All Causes of Death",
    mcd_drug_alcohol_and = "All Causes of Death",
    mcd_cause_113 = "All Causes of Death",
    mcd_cause_113_and = "All Causes of Death",
    ucd_icd_codes = "*All*",
    mcd_icd_codes = "*All*",
    mcd_icd_codes_and = "*All*"
  )
  other_args <- list(...)
  ## Check that if "All" is the argument it's unique
  for (i in seq_along(other_args)) {
    if (any(other_args[[i]] == "All") & length(other_args[[i]]) > 1) {
      stop(paste0(names(other_args)[i], ": you can't have both 'All' and other options selected. Please select either 'All' or specific options."))
    }
  }
  # Check to see if 'All...' is unique
  for (i in seq_along(other_args)) {
    if (any(other_args[[i]] == tb[[names(other_args)[i]]]) &
        length(other_args[[i]]) > 1) {
      stop(paste0(names(other_args)[i], ": you can't have both 'All' and other options selected. Please select either 'All' or specific options."))
    }
  }
  for (i in seq_along(other_args)) {
    if (any(other_args[[i]] == "All")) {
      other_args[[i]] <- tb[[names(other_args)[i]]]
    }
  }
  return(other_args)
}
