age_option <- function(age, show_age_adjusted, db_num, opts3,
                       group_by_1, group_by_2, group_by_3, group_by_4) {
  if (!is.character(age)) {
    age <- as.character(age)
  }
  age <- rlang::arg_match(age, ALL_AGE_GROUPS_OPTS, multiple = TRUE)
  if (show_age_adjusted && !all(age %in% TEN_YEAR_AGE_GROUPS_OPTS))
    stop(paste0("Standard age-adjusted rates are only available for",
                " the ten-year age groups"))
  ## auto generate age_group_option
  age_opt <- dplyr::case_when(
    any(grepl("Five-Year", c(group_by_1, group_by_2,
                        group_by_3, group_by_4))) ~ "Five-Year Age Groups",
    any(grepl("Ten-Year", c(group_by_1, group_by_2,
                        group_by_3, group_by_4))) ~ "Ten-Year Age Groups",
    any(grepl("Single-Year", c(group_by_1,
                               group_by_2,
                               group_by_3,
                               group_by_4))) ~ "Single-Year Ages",
    any(grepl("Infant", c(group_by_1, group_by_2,
                          group_by_3, group_by_4))) ~ "Infant Age Groups",
    # County populations are only available for five-year age groups
    # and all age groups share the age group "< 1". If age is only
    # "< 1" we have to set as a five-year age group
    identical(age, "< 1") ~ "Five-Year Age Groups",
    identical(age, "1-4") ~ "Five-Year Age Groups",
    ## Standard age-adjusted rates are only available for the ten-year age
    ## groups
    ## https://wonder.cdc.gov/wonder/help/mcd-provisional.html
    ## #Age-Adjusted%20Rates%20Hints
    all(c(age %in% c("< 1", "1-4"),
          show_age_adjusted)) ~ "Ten-Year Age Groups",
    all(c(age %in% c("< 1", "1-4"),
          !show_age_adjusted)) ~ "Five-Year Age Groups",
    all(age %in% TEN_YEAR_AGE_GROUPS_OPTS) ~ "Ten-Year Age Groups",
    all(age %in% FIVE_YEAR_AGE_GROUPS_OPTS) ~ "Five-Year Age Groups",
    all(age %in% SINGLE_YEAR_AGES_OPTS) ~ "Single-Year Ages",
    all(age %in% INFANT_AGE_GROUPS_OPTS) ~ "Infant Age Groups",
    TRUE ~ "Invalid"
  )

  if (identical(age_opt, "Ten-Year Age Groups")) {
    rlang::arg_match(age, TEN_YEAR_AGE_GROUPS_OPTS, multiple = TRUE)
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!TEN_YEAR_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <- paste0("V_D", db_num, ".V5")
      opts3$`O_age` <-  paste0("D", db_num, ".V5")
    }
    # Add age adjusted column if 2 or more ten-year age groups are requested
    if ((length(age) >= 2 || identical(age, "All Ages"))
        && show_age_adjusted) {
      opts3$`O_aar_enable` <- "true"
      opts3$`O_aar` <- "aar_std"
      opts3$`O_aar_CI` <- "true"
      opts3$`O_aar_SE` <- "true"
    } else {
      opts3$`O_aar` <- "aar_none"
      opts3$`O_aar_CI` <- "false"
      opts3$`O_aar_SE` <- "false"
    }
  } else if (identical(age_opt, "Five-Year Age Groups")) {
    rlang::arg_match(age, FIVE_YEAR_AGE_GROUPS_OPTS, multiple = TRUE)
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!FIVE_YEAR_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <-  paste0("V_D", db_num, ".V51")
      opts3$`O_age` <-  paste0("D", db_num, ".V51")
      opts3$`O_aar` <- "aar_none"
    }
  } else if (identical(age_opt, "Single-Year Ages")) {
    rlang::arg_match(age, SINGLE_YEAR_AGES_OPTS, multiple = TRUE)
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!SINGLE_YEAR_AGES_KEY)
      names(opts3)[length(opts3)] <-  paste0("V_D", db_num, ".V52")
      opts3$`O_age` <-  paste0("D", db_num, ".V52")
      opts3$`O_aar` <- "aar_none"
    }
  } else if (identical(age_opt, "Infant Age Groups")) {
    rlang::arg_match(age, INFANT_AGE_GROUPS_OPTS, multiple = TRUE)
    for (i in age) {
      opts3[length(opts3) + 1] <- dplyr::recode(i, !!!INFANT_AGE_GROUPS_KEY)
      names(opts3)[length(opts3)] <-  paste0("V_D", db_num, ".V6")
      opts3$`O_age` <-  paste0("D", db_num, ".V6")
      opts3$`O_aar` <- "aar_none"
    }
  } else {
    stop("Invalid age groups")
  }
  return(opts3)
}
