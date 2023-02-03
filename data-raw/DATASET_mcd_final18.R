MCD_FINAL18_UCD_KEY <- c(
  "ICD-10 Codes" = "D157.V2",
  "ICD-10 130 Cause List (Infants)" = "D157.V12",
  "Drug/Alcohol Induced Causes" = "D157.V25",
  "ICD-10 113 Cause List" = "D157.V4",
  "Injury Intent and Mechanism" = "D157.V22"
)
MCD_FINAL18_UCD_OPTS <- names(MCD_FINAL18_UCD_KEY)

MCD_FINAL18_MCD_KEY <- c(
  "MCD - ICD-10 Codes" = "D157.V13",
  "MCD - ICD-10 130 Cause List (Infants)" = "D157.V16",
  "MCD - Drug/Alcohol Induced Causes" = "D157.V26",
  "MCD - ICD-10 113 Cause List" = "D157.V15"
)
MCD_FINAL18_MCD_OPTS <- names(MCD_FINAL18_MCD_KEY)

MCD_FINAL18_GROUP_BY_1_KEY <-
  c(
    "Census Region" = "D157.V10-level1",
    "Census Division" = "D157.V10-level2",
    "HHS Region" = "D157.V27-level1",
    "State" = "D157.V9-level1",
    "County" = "D157.V9-level2",
    "2013 Urbanization" = "D157.V19",
    "2006 Urbanization" = "D157.V11",


    "Ten-Year Age Groups" = "D157.V5",
    "Five-Year Age Groups" = "D157.V51",
    "Single-Year Ages" = "D157.V52",
    "Infant Age Groups" = "D157.V6",
    "Gender" = "D157.V7",
    "Hispanic Origin" = "D157.V17",
    "Single Race 6" = "D157.V42",
    "Single Race 15" = "D157.V43",
    "Single/Multi Race 31" = "D157.V44",

    "Year" = "D157.V1-level1",
    "Month" = "D157.V1-level2",

    "Weekday" = "D157.V24",
    "Autopsy" = "D157.V20",
    "Place of Death" = "D157.V21",


    "UCD - 15 Leading Causes of Death" = "D157.V28",
    "UCD - 15 Leading Causes of Death (Infants)" = "D157.V29",
    "UCD - ICD Chapter" = "D157.V2-level1",
    "UCD - ICD Sub-Chapter" = "D157.V2-level2",
    "Underlying Cause of death" = "D157.V2-level3",
    "UCD - ICD-10 113 Cause List" = "D157.V4",
    "UCD - ICD-10 130 Cause List (Infants)" = "D157.V12",
    "UCD - Injury Intent" = "D157.V22",
    "UCD - Injury Mechanism &amp; All Other Leading Causes" = "D157.V23",
    "UCD - Drug/Alcohol Induced" = "D157.V25-level1",
    "UCD - Drug/Alcohol Induced Cause" = "D157.V25-level2",


    "MCD - ICD Chapter" = "D157.V13-level1",
    "MCD - ICD Sub-Chapter" = "D157.V13-level2",
    "Multiple Cause of death" = "D157.V13-level3",
    "MCD - ICD-10 113 Cause List" = "D157.V15",
    "MCD - ICD-10 130 Cause List (Infants)" = "D157.V16",
    "MCD - Drug/Alcohol Induced" = "D157.V26-level1",
    "MCD - Drug/Alcohol Induced Cause" = "D157.V26-level2"

  )
MCD_FINAL18_GROUP_BY_1_OPTS <- names(MCD_FINAL18_GROUP_BY_1_KEY)

MCD_FINAL18_GROUP_BY_2_KEY <-
  c("None" = "*None*", MCD_FINAL18_GROUP_BY_1_KEY)
MCD_FINAL18_GROUP_BY_2_OPTS <- c("None", MCD_FINAL18_GROUP_BY_1_OPTS)
