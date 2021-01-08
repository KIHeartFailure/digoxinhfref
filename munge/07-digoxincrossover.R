

# Define digoxin crossover from DDR -------------------------------------------

matchplm <- left_join(matchp,
  lm,
  by = "LopNr"
) %>%
  mutate(tmp_digoxin = stringr::str_detect(
    ATC,
    "^(C01AA05)"
  )) %>%
  filter(
    EDATUM <= ymd("2018-12-31"),
    EDATUM >= shf_indexdtm + 14,
    tmp_digoxin
  )

# assume that if you are on you are on until LAST dispension + 3 mo OR 2018-08-01
# (last follow-up - 5 mo) OR death - 5 mo
# independent of how long time between dispensions
# although sort of crude, is probably the option that will reflect reality
# the best for the majority of patients.

crossoverfunc <- function(time, event) {
  time <- sym(time)
  event <- sym(event)

  matchplm2 <- matchplm %>%
    filter(EDATUM <= shf_indexdtm + !!time) %>%
    group_by(LopNr) %>%
    arrange(EDATUM) %>%
    slice(c(1, n())) %>%
    mutate(firstlast = ifelse(row_number() == 1, "firstdtm", "lastdtm")) %>%
    ungroup()

  matchplm3 <- left_join(matchp,
    matchplm2 %>% select(LopNr, firstlast, EDATUM),
    by = "LopNr"
  ) %>%
    mutate(
      enddtm = shf_indexdtm + !!time,
      crossoverdtm = case_when(
        ddr_digoxin == "Yes" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30,
        ddr_digoxin == "No" & firstlast == "firstdtm" ~ EDATUM,
        ddr_digoxin == "No" & firstlast == "lastdtm" & EDATUM <= enddtm - 5 * 30 ~ EDATUM + 3 * 30
      ),
      crossover_ddr_digoxin = case_when(
        ddr_digoxin == "Yes" ~ "No",
        ddr_digoxin == "No" & firstlast == "firstdtm" ~ "Yes",
        ddr_digoxin == "No" & firstlast == "lastdtm" ~ "No"
      ),
      crossover = 1
    ) %>%
    filter(!is.na(crossoverdtm)) %>%
    select(-ddr_digoxin, -enddtm, -EDATUM, -firstlast) %>%
    rename(ddr_digoxin = crossover_ddr_digoxin)

  matchpcrossover <- bind_rows(
    matchp,
    matchplm3
  ) %>%
    mutate(dtmuse = coalesce(crossoverdtm, shf_indexdtm)) %>%
    group_by(LopNr) %>%
    arrange(dtmuse) %>%
    mutate(
      start = case_when(
        row_number() == 1 ~ 0,
        TRUE ~ as.numeric(dtmuse - shf_indexdtm)
      ),
      stop = case_when(
        row_number() == n() ~ !!time,
        TRUE ~ lead(start)
      ),
      !!event := case_when(
        row_number() == n() ~ as.character(!!event),
        TRUE ~ "No"
      )
    ) %>%
    ungroup() %>%
    arrange(LopNr, dtmuse) %>%
    select(LopNr, shf_indexdtm, ddr_digoxin, start, stop, !!event, par, shf_sos_com_af)
  return(matchpcrossover)
}

matchpcross_deathhosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_deathhosphf"
)
matchpcross_death <- crossoverfunc(
  time = "sos_outtime_death",
  event = "sos_out_death"
)
matchpcross_hosphf <- crossoverfunc(
  time = "sos_outtime_hosphf",
  event = "sos_out_hosphf"
)
