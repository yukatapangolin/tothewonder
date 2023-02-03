MCD_UCD_KEY <- c(
  "ICD-10 Codes" = "D176.V2",
  "ICD-10 130 Cause List (Infants)" = "D176.V12",
  "Drug/Alcohol Induced Causes" = "D176.V25",
  "ICD-10 113 Cause List" = "D176.V4",
  "Injury Intent and Mechanism" = "D176.V22"
)
MCD_UCD_OPTS <- names(MCD_UCD_KEY)

MCD_MCD_KEY <- c(
  "MCD - ICD-10 Codes" = "D176.V13",
  "MCD - ICD-10 130 Cause List (Infants)" = "D176.V16",
  "MCD - Drug/Alcohol Induced Causes" = "D176.V26",
  "MCD - ICD-10 113 Cause List" = "D176.V15"
)
MCD_MCD_OPTS <- names(MCD_MCD_KEY)

MCD_GROUP_BY_1_KEY <-
  c(
    "Residence Census Region" = "D176.V10-level1",
    "Residence Census Division" = "D176.V10-level2",
    "Residence HHS Region" = "D176.V27-level1",
    "Residence State" = "D176.V9-level1",
    "Residence County" = "D176.V9-level2",
    "Residence 2013 Urbanization" = "D176.V19",
    "Residence 2006 Urbanization" = "D176.V11",
    "Occurrence Census Region" = "D176.V80-level1",
    "Occurrence Census Division" = "D176.V80-level2",
    "Occurrence HHS Region" = "D176.V77-level1",
    "Occurrence State" = "D176.V79-level1",
    "Occurrence County" = "D176.V79-level2",
    "Occurrence 2013 Urbanization" = "D176.V89",
    "Occurrence 2006 Urbanization" = "D176.V81",
    "Ten-Year Age Groups" = "D176.V5",
    "Five-Year Age Groups" = "D176.V51",
    "Single-Year Ages" = "D176.V52",
    "Infant Age Groups" = "D176.V6",
    "Gender" = "D176.V7",
    "Hispanic Origin" = "D176.V17",
    "Single Race 6" = "D176.V42",
    "Single Race 15" = "D176.V43",
    "Single/Multi Race 31" = "D176.V44",
    "Year" = "D176.V1-level1",
    "Month" = "D176.V1-level2",
    "MMWR Year" = "D176.V100-level1",
    "MMWR Week" = "D176.V100-level2",
    "Autopsy" = "D176.V20",
    "Place of Death" = "D176.V21",
    "UCD - 15 Leading Causes of Death" = "D176.V28",
    "UCD - 15 Leading Causes of Death (Infants)" = "D176.V29",
    "UCD - ICD Chapter" = "D176.V2-level1",
    "UCD - ICD Sub-Chapter" = "D176.V2-level2",
    "Underlying Cause of death" = "D176.V2-level3",
    "UCD - ICD-10 113 Cause List" = "D176.V4",
    "UCD - ICD-10 130 Cause List (Infants)" = "D176.V12",
    "UCD - Injury Intent" = "D176.V22",
    "UCD - Injury Mechanism &amp; All Other Leading Causes" = "D176.V23",
    "UCD - Drug/Alcohol Induced" = "D176.V25-level1",
    "UCD - Drug/Alcohol Induced Cause" = "D176.V25-level2",
    "MCD - ICD Chapter" = "D176.V13-level1",
    "MCD - ICD Sub-Chapter" = "D176.V13-level2",
    "Multiple Cause of death" = "D176.V13-level3",
    "MCD - ICD-10 113 Cause List" = "D176.V15",
    "MCD - ICD-10 130 Cause List (Infants)" = "D176.V16",
    "MCD - Drug/Alcohol Induced" = "D176.V26-level1",
    "MCD - Drug/Alcohol Induced Cause" = "D176.V26-level2"
  )
MCD_GROUP_BY_1_OPTS <- names(MCD_GROUP_BY_1_KEY)

MCD_GROUP_BY_2_KEY <-
  c("None" = "*None*", MCD_GROUP_BY_1_KEY)
MCD_GROUP_BY_2_OPTS <- c("None", MCD_GROUP_BY_1_OPTS)

SINGLE_RACE_6_KEY <- c(
  "All Races" = "*All*",
  "American Indian or Alaska Native" = "1002-5",
  "Asian" = "A",
  "Black or African American" = "2054-5",
  "Native Hawaiian or Other Pacific Islander" = "NHOPI",
  "White" = "2106-3",
  "More than one race" = "M"
)
SINGLE_RACE_6_OPTS <- names(SINGLE_RACE_6_KEY)

SINGLE_RACE_15_KEY <- c(
  "All Races" = "*All*",
  "White" = "1",
  "Black" = "2",
  "American Indian or Alaskan Native" = "3",
  "Asian Indian" = "4",
  "Chinese" = "5",
  "Filipino" = "6",
  "Japanese" = "7",
  "Korean" = "8",
  "Vietnamese" = "9",
  "Other Asian" = "10",
  "Hawaiian" = "11",
  "Guamanian" = "12",
  "Samoan" = "13",
  "Other Pacific Islander" = "14",
  "More than one race" = "15"
)
SINGLE_RACE_15_OPTS <- names(SINGLE_RACE_15_KEY)

SINGLE_MULTI_RACE_31_KEY <- c(
  "All Races" = "*All*",
  "White (only)" = "1",
  "Black (only)" = "2",
  "AIAN (American Indian or Alaskan Native) (only)" = "3",
  "Asian (only)" = "4",
  "NHOPI (Native Hawaiian or Other Pacific Islander) (only)" = "5",
  "Black and White" = "6",
  "Black and AIAN" = "7",
  "Black and Asian" = "8",
  "Black and NHOPI" = "9",
  "AIAN and White" = "10",
  "AIAN and Asian" = "11",
  "AIAN and NHOPI" = "12",
  "Asian and White" = "13",
  "Asian and NHOPI" = "14",
  "NHOPI and White" = "15",
  "Black, AIAN, and White" = "16",
  "Black, AIAN, and Asian" = "17",
  "Black, AIAN, and NHOPI" = "18",
  "Black, Asian, and White" = "19",
  "Black, Asian, and NHOPI" = "20",
  "Black, NHOPI, and White" = "21",
  "AIAN, Asian, and White" = "22",
  "AIAN, NHOPI, and White" = "23",
  "AIAN, Asian, and NHOPI" = "24",
  "Asian, NHOPI, and White" = "25",
  "Black, AIAN, Asian, and White" = "26",
  "Black, AIAN, Asian, and NHOPI" = "27",
  "Black, AIAN, NHOPI, and White" = "28",
  "Black, Asian, NHOPI, and White" = "29",
  "AIAN, Asian, NHOPI, and White" = "30",
  "Black, AIAN, Asian, NHOPI, and White" = "31"
)
SINGLE_MULTI_RACE_31_OPTS <- names(SINGLE_MULTI_RACE_31_KEY)

RACE_18_OPTS <- unique(c(
  SINGLE_RACE_6_OPTS,
  SINGLE_RACE_15_OPTS,
  SINGLE_MULTI_RACE_31_OPTS
))

RACE_OPTIONS_OPTS <- c(
  "Single Race 6",
  "Single Race 15",
  "Single/Multi Race 31"
)

PERIOD_KEY <- c(
  "Year" = "YEAR",
  "MMWR" = "MMWR"
)
PERIOD_OPTS <- names(PERIOD_KEY)
