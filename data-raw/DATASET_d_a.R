DRUG_ALCOHOL_KEY <- c(
  "All Causes of Death" = "*All*",
  "Drug-induced causes" = "D",
  "Drug poisonings (overdose) Unintentional (X40-X44)" = "D1",
  "Drug poisonings (overdose) Suicide (X60-X64)" = "D2",
  "Drug poisonings (overdose) Homicide (X85)" = "D3",
  "Drug poisonings (overdose) Undetermined (Y10-Y14)" = "D4",
  "All other drug-induced causes" = "D9",
  "Alcohol-induced causes" = "A",
  "Alcohol poisonings (overdose) (X45, X65, Y15)" = "A1",
  "All other alcohol-induced causes" = "A9",
  "O" = "All other non-drug and non-alcohol causes",
  "All other non-drug and non-alcohol causes" = "O9"
)
DRUG_ALCOHOL_OPTS <- names(DRUG_ALCOHOL_KEY)
