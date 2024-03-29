ua_factory <- function() {
  ua_list <- c(
    paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
           " (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"),
    paste0("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36",
           " (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"),
    paste0("Mozilla/5.0 (Windows NT 10.0) AppleWebKit/537.36",
           " (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"),
    paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_4) AppleWebKit/537.36",
           " (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"),
    paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:101.0)",
           " Gecko/20100101 Firefox/101.0"),
    paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 12.4; rv:101.0)",
           " Gecko/20100101 Firefox/101.0"),
    paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_4) AppleWebKit/605.1.15",
           " (KHTML, like Gecko) Version/15.4 Safari/605.1.15"),
    paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHT",
           "ML, like Gecko) Chrome/102.0.0.0 Safari/537.36 Edg/102.0.1245.44"),
    paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 12_4) AppleWebKit/537.36",
           " (KHTML, like Gecko)",
           "Chrome/102.0.0.0 Safari/537.36 Edg/102.0.1245.44"))
  ua <- sample(ua_list, 1)
  function() {
    return(ua)
  }
}
get_ua <- ua_factory()()


wonder_years <- function(db_num) {
  Sys.sleep(3)

  res <- httr::POST(
    httr::set_cookies(`s_cc` = "true"),
    url = paste0(
      "https://wonder.cdc.gov/controller/datarequest/",
      db_num
    ),
    body = list(
      "stage" = "about",
      "saved_id" = "",
      `action-I Agree` = I("I+Agree")
    ),
    encode = "form",
    httr::add_headers(.headers = c(
      "user-agent" = get_ua,
      "Cache-Control" = "max-age=0",
      "Accept" = paste0("text/html,application/xhtml+xml,application/xml;q=",
                        "0.9,image/webp,*/*;q=0.8"),
      "Accept-Language" = "en-us,en;q=0.5"
    ))
  )
  w_txt <- httr::content(res, as = "text")
  years <- c()
  for (year in 1999:2030) {
    if (grepl(paste0(".*<select.*?.*name=\"F_",
                      db_num,
                      ".V1\".*?(",
                      year,
                      ").*?</select>.*"),
               w_txt))
      years <- append(years, year)
  }
  as.character(years)
}


wonder_session <- function(db_num) {
  Sys.sleep(3)
  paste0(
    "https://wonder.cdc.gov",
    local({
      res <- httr::POST(
        httr::set_cookies(`s_cc` = "true"),
        url = paste0(
          "https://wonder.cdc.gov/controller/datarequest/",
          db_num
        ),
        body = list(
          "stage" = "about",
          "saved_id" = "",
          `action-I Agree` = I("I+Agree")
        ),
        encode = "form",
        httr::add_headers(.headers = c(
          "user-agent" = get_ua,
          "Cache-Control" = "max-age=0",
          "Accept" = paste0("text/html,application/xhtml+xml,application/xml;",
                            "q=0.9,image/webp,*/*;q=0.8"),
          "Accept-Language" = "en-us,en;q=0.5"
        ))
      )
      w_txt <- httr::content(res, as = "text")
      regmatches(
        w_txt,
        regexpr(
          paste0(
            "/controller/datarequest/",
            db_num,
            ";jsessionid=[A-Z0-9]+"
          ),
          w_txt
        )
      )
    })
  )
}

print_terms <- function() {
  cli::cli_inform(paste0(
    "Researchers who violate the terms of the data use restrictions",
    " will lose access to WONDER and their sponsors and institutions",
    " will be notified. Researchers who are suspected of violating",
    " the rules may be prevented from using WONDER until an investigation",
    " can be completed. Deliberately making a false statement in any",
    " matter within the jurisdiction of any department or agency of the",
    " Federal government violates 18 USC 1001 and is punishable by a fine",
    " of up to $10,000 or up to 5 years in prison, or both."),
    .envir = parent.frame())
}

#' Agree to the CDC WONDER data use restrictions
#'
#' @param I_Agree Pre-agree to the WONDER data use restrictions
#'
#' @export
#' @return An URL string including the session id
#'
session_ucd99 <- function(I_Agree = FALSE) {  # nolint
  if (!I_Agree) {
    if (!yesno(paste0("Do you agree with the CDC WONDER data use rest",
                      "rictions?\n(https://wonder.cdc.gov/datause.html)"))) {
      print_terms()
      return(wonder_session("D76"))
    } else {
      return(NULL)
    }
  } else {
    return(wonder_session("D76"))
  }
}


#' Agree to the CDC WONDER data use restrictions
#'
#' @param I_Agree Pre-agree to the WONDER data use restrictions
#'
#' @return An URL string including the session id
#' @export
#'
session_mcd_provisional <- function(I_Agree = FALSE) { # nolint
  if (!I_Agree) {
    if (!yesno(paste0("Do you agree with the CDC WONDER data use",
                      " restrictions?",
                      "\n(https://wonder.cdc.gov/datause.html)"))) {
      print_terms()
      return(wonder_session("D176"))
    } else {
      return(NULL)
    }
  } else {
    return(wonder_session("D176"))
  }
}


#' Agree to the CDC WONDER data use restrictions
#'
#' @param I_Agree Pre-agree to the WONDER data use restrictions
#'
#' @return An URL string including the session id
#' @export
#'
session_mcd_final18 <- function(I_Agree = FALSE) { # nolint
  if (!I_Agree) {
    if (!yesno(paste0(
      "Do you agree with the CDC WONDER data use restrictions?",
      "\n(https://wonder.cdc.gov/datause.html)"))) {
      print_terms()
      return(wonder_session("D157"))
    } else {
      return(NULL)
    }
  } else {
    return(wonder_session("D157"))
  }
}

# Code borrowed from devtools
# nolint start
# https://github.com/r-lib/devtools/blob/7cc2bdf74cbae90233fa6c84f221631d1876598d/R/release.R#L149
# nolint end
yesno <- function(msg, .envir = parent.frame()) {
  yeses <- c(
    "Yes", "Definitely", "For sure",
    "Yeah", "Of course", "Absolutely"
  )
  nos <- c(
    "Fauci gave me monkepox", "Google CDC covid test kits failure 2020",
    "Surgical masks in hospitals", "No",
    "Droplets!", "Let's go Brandon", "Haven't read them",
    "I'm unvaxxed", "No human-to-human transmission"
  )

  cli::cli_inform(msg, .envir = .envir)
  cli::cli_inform(paste0(
    "Any effort to determine the identity of any reported cases, or to use",
    " the information for any purpose other than for statistical reporting",
    " and analysis, is against the law. Therefore users will:"),
    .envir = .envir)

  cli::cli_inform(
    "* Use these data for statistical reporting and analysis only.",
    .envir = .envir)
  cli::cli_inform(paste0(
    "* For sub-national geography, do not present or publish death or birth",
    " counts of 9 or fewer or rates based on counts of nine or fewer (in",
    " figures, graphs, maps, table, etc.)."),
    .envir = .envir)
  cli::cli_inform(paste0(
    "* Make no attempt to learn the identity of any person or establishment",
    " included in these data."),
    .envir = .envir)
  cli::cli_inform(paste0(
    "* Make no disclosure or other use of the identity ",
    "of any person or establishment discovered ",
    "inadvertently and advise the Director, NCHS of",
    " any such discovery."),
    .envir = .envir)

  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  ## scope
  tb <- lapply(.traceback(x = 0),
               \(x) any(grepl("test_env", x))
               ) |>
    unlist()

  ## check if called from testthat or interactive
  if (any(tb) && !interactive())
    return(FALSE)

  utils::menu(qs[rand]) != which(rand == 1)
}
