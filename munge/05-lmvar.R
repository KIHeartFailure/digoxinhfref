
# Digoxin treatment from DDR ---------------------------------------------

lmtmp <- left_join(pdata, lm, by = "LopNr")

lmtreats <- function(atc, treatname) {
  lmtmp2 <- lmtmp %>%
    mutate(
      atcneed = stringr::str_detect(ATC, atc),
      diff = as.numeric(EDATUM - shf_indexdtm)
    ) %>%
    filter(
      atcneed,
      diff >= -30.5 * 5, diff <= 14
    )

  treatname <- paste0("ddr_", treatname)

  lmtmp2 <- lmtmp2 %>%
    group_by(LopNr) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(!!treatname := "Yes") %>%
    select(LopNr, !!sym(treatname))

  pdata <<- left_join(pdata,
    lmtmp2,
    by = "LopNr"
  ) %>%
    mutate(!!treatname := replace_na(!!sym(treatname), "No"))

  metatmp <- c(treatname, stringr::str_replace_all(atc, "\\|", ","))
  if (exists("metalm")) {
    metalm <<- rbind(metalm, metatmp) # global variable, writes to global env
  } else {
    metalm <<- metatmp # global variable, writes to global env
  }
}

lmtreats("^(C01AA05)", "digoxin")

pdata <- pdata %>%
  mutate(ddr_digoxin = factor(ddr_digoxin), 
         ddr_digoxinnum = if_else(ddr_digoxin == "Yes", 1, 0)
         )

metalm <- matrix(metalm, ncol = 2)
colnames(metalm) <- c("Variable", "ATC")
metalm <- metalm %>%
  as_tibble() %>%
  mutate(
    ATC = gsub("^", "", ATC, fixed = TRUE),
    ATC = gsub("(", "", ATC, fixed = TRUE),
    ATC = gsub(")", "", ATC, fixed = TRUE),
    ATC = gsub("?!", " excl.", ATC, fixed = TRUE),
    Registry = "Dispensed Drug Registry",
    Period = "-5mo-14days",
  )


# Overtime graph ----------------------------------------------------------

overtime <- lmtmp %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(C01AA05)")) %>%
  filter(atcneed) %>%
  group_by(LopNr, AR) %>%
  slice(1) %>%
  ungroup() %>%
  count(AR)

overtime_af <- lmtmp %>%
  filter(shf_sos_com_af == "Yes") %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(C01AA05)")) %>%
  filter(atcneed) %>%
  group_by(LopNr, AR) %>%
  slice(1) %>%
  ungroup() %>%
  count(AR)

overtime_noaf <- lmtmp %>%
  filter(shf_sos_com_af == "No") %>%
  mutate(atcneed = stringr::str_detect(ATC, "^(C01AA05)")) %>%
  filter(atcneed) %>%
  group_by(LopNr, AR) %>%
  slice(1) %>%
  ungroup() %>%
  count(AR)
