injury_mechanism_key <- c(
  "All Causes of Death" = "*All*",
  "Cut/Pierce" = "GRINJ-001",
  "Drowning" = "GRINJ-002",
  "Fall" = "GRINJ-003",
  "Fire/Flame" = "GRINJ-004",
  "Hot object/Substance" = "GRINJ-005",
  "Firearm" = "GRINJ-006",
  "Machinery" = "GRINJ-007",
  "Motor Vehicle Traffic" = "GRINJ-008",
  "Other Pedal cyclist" = "GRINJ-009",
  "Other Pedestrian" = "GRINJ-010",
  "Other land transport" = "GRINJ-011",
  "Other transport" = "GRINJ-012",
  "Natural/Environmental" = "GRINJ-013",
  "Overexertion" = "GRINJ-014",
  "Poisoning" = "GRINJ-015",
  "Struck by or against" = "GRINJ-016",
  "Suffocation" = "GRINJ-017",
  "Other specified, classifiable Injury" = "GRINJ-018",
  "Other specified, not elsewhere classified Injury" = "GRINJ-019",
  "Unspecified Injury" = "GRINJ-020",
  "Non-Injury: Intestinal infections" = "GR113 001-003",
  "Non-Injury: Tuberculosis" = "GR113 005-006",
  "Non-Injury: Whooping cough" = "GR113-007",
  "Non-Injury: Scarlet fever and erysipelas" = "GR113-008",
  "Non-Injury: Meningococcal infection" = "GR113-009",
  "Non-Injury: Septicemia" = "GR113-010",
  "Non-Injury: Syphilis" = "GR113-011",
  "Non-Injury: Acute poliomyelitis" = "GR113-012",
  "Non-Injury: Arthropod-borne viral encephalitis" = "GR113-013",
  "Non-Injury: Measles" = "GR113-014",
  "Non-Injury: Viral hepatitis" = "GR113-015",
  "Non-Injury: Human immunodeficiency virus (HIV) disease" = "GR113-016",
  "Non-Injury: Malaria" = "GR113-017",
  "Non-Injury: Other and unspecified infectious and parasitic diseases and their sequelae" = "GR113-018",
  "Non-Injury: Malignant neoplasms (Cancers)" = "GR113 020-043",
  "Non-Injury: In situ neoplasms, benign neoplasms and neoplasms of uncertain or unknown behavior" = "GR113-044",
  "Non-Injury: Anemias" = "GR113-045",
  "Non-Injury: Diabetes mellitus" = "GR113-046",
  "Non-Injury: Nutritional Deficiencies" = "GR113 048-049",
  "Non-Injury: Meningitis" = "GR113-050",
  "Non-Injury: Parkinsons disease" = "GR113-051",
  "Non-Injury: Alzheimers disease" = "GR113-052",
  "Non-Injury: Diseases of Heart" = "GR113 055-068",
  "Non-Injury: Essential (primary) hypertension and hypertensive renal disease" = "GR113-069",
  "Non-Injury: Cerebrovascular diseases, including stroke" = "GR113-070",
  "Non-Injury: Atherosclerosis" = "GR113-071",
  "Non-Injury: Other diseases of the circulatory system" = "GR113 073-074",
  "Non-Injury: Other disorders of circulatory system" = "GR113-075",
  "Non-Injury: Influenza & Pneumonia" = "GR113 077-078",
  "Non-Injury: Other acute lower respiratory infections" = "GR113 079",
  "Non-Injury: Chronic lower respiratory diseases" = "GR113 082",
  "Non-Injury: Pneumoconioses and chemical effects" = "GR113-087",
  "Non-Injury: Pneumonitis due to solids and liquids" = "GR113-088",
  "Non-Injury: Other diseases of respiratory system" = "GR113-089",
  "Non-Injury: Peptic ulcer" = "GR113-090",
  "Non-Injury: Diseases of appendix" = "GR113-091",
  "Non-Injury: Hernia" = "GR113-092",
  "Non-Injury: Chronic liver disease and cirrhosis" = "GR113 093",
  "Non-Injury: Cholelithiasis and other disorders of gallbladder" = "GR113-096",
  "Non-Injury: Nephritis, nephrotic syndrome and nephrosis" = "GR113 097",
  "Non-Injury: Infections of kidney" = "GR113-102",
  "Non-Injury: Hyperplasia of prostate" = "GR113-103",
  "Non-Injury: Inflammatory diseases of female pelvic organs" = "GR113-104",
  "Non-Injury: Pregnancy with abortive outcome" = "GR113-106",
  "Non-Injury: Other complications of pregnancy, childbirth and the puerperium" = "GR113-107",
  "Non-Injury: Certain conditions originating in the perinatal period" = "GR113-108",
  "Non-Injury: Congenital malformations, deformations and chromosomal abnormalities" = "GR113-109",
  "Non-Injury: Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified" = "GR113-110",
  "Non-Injury: All other diseases (Residual)" = "GR113-111",
  "Non-Injury: Complications of medical and surgical care" = "GR113-135"
)
injury_mechanism_opts <- names(injury_mechanism_key)

ucd_key <- c(
  "ICD-10 Codes" = "D76.V2",
  "ICD-10 130 Cause List (Infants)" = "D76.V12",
  "Drug/Alcohol Induced Causes" = "D76.V25",
  "ICD-10 113 Cause List" = "D76.V4",
  "Injury Intent and Mechanism" = "D76.V22"
)
ucd_opts <- names(ucd_key)

group_by_1_key <-
  c(
    "Census Region" = "D76.V10-level1",
    "Census Division" = "D76.V10-level2",
    "HHS Region" = "D76.V27-level1",
    "State" = "D76.V9-level1",
    "County" = "D76.V9-level2",
    "2013 Urbanization" = "D76.V19",
    "2006 Urbanization" = "D76.V11",
    "Ten-Year Age Groups" = "D76.V5",
    "Five-Year Age Groups" = "D76.V51",
    "Single-Year Ages" = "D76.V52",
    "Infant Age Groups" = "D76.V6",
    "Gender" = "D76.V7",
    "Hispanic Origin" = "D76.V17",
    "Race" = "D76.V8",
    "Year" = "D76.V1-level1",
    "Month" = "D76.V1-level2",
    "Weekday" = "D76.V24",
    "Autopsy" = "D76.V20",
    "Place of Death" = "D76.V21",
    "15 Leading Causes of Death" = "D76.V28",
    "15 Leading Causes of Death (Infants)" = "D76.V29",
    "ICD Chapter" = "D76.V2-level1",
    "ICD Sub-Chapter" = "D76.V2-level2",
    "Cause of death" = "D76.V2-level3",
    "ICD-10 113 Cause List" = "D76.V4",
    "ICD-10 130 Cause List (Infants)" = "D76.V12",
    "Injury Intent" = "D76.V22",
    "Injury Mechanism & All Other Leading Causes" = "D76.V23",
    "Drug/Alcohol Induced" = "D76.V25-level1",
    "Drug/Alcohol Induced Cause" = "D76.V25-level2"
  )
group_by_1_opts <- names(group_by_1_key)

group_by_2_key <-
  c("None" = "*None*", group_by_1_key)
group_by_2_opts <- c("None", group_by_1_opts)

urbanization_key <- c(
  "All Categories" = "*All*",
  "Large Central Metro" = "1",
  "Large Fringe Metro" = "2",
  "Medium Metro" = "3",
  "Small Metro" = "4",
  "Micropolitan (Nonmetro)" = "5",
  "NonCore (Nonmetro)" = "6"
)
urbanization_opts <- names(urbanization_key)

single_year_ages_key <- c(
  "All Ages" = "*All*",
  "100+ years" = 100,
  "Not Stated" = "NS"
)

gender_key <- c(
  "All Genders" = "*All*",
  "Female" = "F",
  "Male" = "M"
)
gender_opts <- names(gender_key)

hispanic_origin_key <- c(
  "All Origins" = "*All*",
  "Not Hispanic or Latino" = "2186-2",
  "Hispanic or Latino" = "2135-2",
  "Not Stated" = "NS"
)
hispanic_origin_opts <- names(hispanic_origin_key)

race_key <- c(
  "All Races" = "*All*",
  "American Indian or Alaska Native" = "1002-5",
  "Asian or Pacific Islander" = "A-PI",
  "Black or African American" = "2054-5",
  "White" = "2106-3"
)
race_opts <- names(race_key)

injury_intent_key <- c(
  "All Causes of Death" = "*All*",
  "Unintentional" = "1",
  "Suicide" = "2",
  "Homicide" = "3",
  "Undetermined" = "4",
  "Legal Intervention / Operations of War" = "5",
  "Non-Injury, no intent classified" = "9"
)
injury_intent_opts <- names(injury_intent_key)

weekday_key <- c(
  "All Weekdays" = "*All*",
  "Sunday" = "1",
  "Monday" = "2",
  "Tuesday" = "3",
  "Wednesday" = "4",
  "Thursday" = "5",
  "Friday" = "6",
  "Saturday" = "7",
  "Unknown" = "9"
)
weekday_opts <- names(weekday_key)

autopsy_key <- c(
  "All Values" = "*All*",
  "No" = "N",
  "Yes" = "Y",
  "Unknown" = "U"
)
autopsy_opts <- names(autopsy_key)

place_of_death_key <- c(
  "All Places" = "*All*",
  "Medical Facility - Inpatient" = "1",
  "Medical Facility - Outpatient or ER" = "2",
  "Medical Facility - Dead on Arrival" = "3",
  "Medical Facility - Status unknown" = "10",
  "Decedent's home" = "4",
  "Hospice facility" = "5",
  "Nursing home/long term care" = "6",
  "Other" = "7",
  "Place of death unknown" = "9"
)
place_of_death_opts <- names(place_of_death_key)

census_region_key <- c(
  "*All*(The United States)" = "*All*",
  "+CENS-R1(Census Region 1: Northeast)" = "CENS-R1",
  "+CENS-R2(Census Region 2: Midwest)" = "CENS-R2",
  "+CENS-R3(Census Region 3: South)" = "CENS-R3",
  "+CENS-R4(Census Region 4: West)" = "CENS-R4"
)

hhs_region_key <- c(
  "*All*(The United States)" = "*All*",
  "+HHS1(HHS Region #1  CT, ME, MA, NH, RI, VT)" = "HHS1",
  "+HHS2(HHS Region #2  NJ, NY)" = "HHS2",
  "+HHS3(HHS Region #3  DE, DC, MD, PA, VA, WV)" = "HHS3",
  "+HHS4(HHS Region #4  AL, FL, GA, KY, MS, NC, SC, TN)" = "HHS4",
  "+HHS5(HHS Region #5  IL, IN, MI, MN, OH, WI)" = "HHS5",
  "+HHS6(HHS Region #6  AR, LA, NM, OK, TX)" = "HHS6",
  "+HHS7(HHS Region #7  IA, KS, MO, NE)" = "HHS7",
  "+HHS8(HHS Region #8  CO, MT, ND, SD, UT, WY)" = "HHS8",
  "+HHS9(HHS Region #9  AZ, CA, HI, NV)" = "HHS9",
  "+HHS10(HHS Region #10  AK, ID, OR, WA)" = "HHS10"
)
