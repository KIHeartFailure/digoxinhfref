

# Inclusion/exclusion criteria --------------------------------------------------------

pdata <- rsdata315 %>%
  filter(casecontrol == "Case")

flow <- c("Number of posts (cases) in SHFDB3", nrow(pdata))

pdata <- pdata %>%
  filter(shf_indexdtm >= ymd("2005-12-01"))
flow <- rbind(flow, c("Indexdate >= 1 Dec 2005 (start DDR 1 July 2005 + 5 months)", nrow(pdata)))

pdata <- pdata %>%
  filter(!is.na(shf_ef))
flow <- rbind(flow, c("No missing EF", nrow(pdata)))

pdata <- pdata %>%
  filter(shf_ef %in% c("30-39", "<30")) %>%
  mutate(shf_ef = droplevels(shf_ef))
flow <- rbind(flow, c("EF <=39%", nrow(pdata)))

pdata <- pdata %>%
  filter(sos_outtime_death >= 14)
flow <- rbind(flow, c(">=14 days follow-up (to avoid immortal time bias*)", nrow(pdata)))

pdata <- pdata %>%
  group_by(LopNr) %>%
  arrange(shf_indexdtm) %>%
  slice(1) %>%
  ungroup()

flow <- rbind(flow, c("First post / patient", nrow(pdata)))

colnames(flow) <- c("Criteria", "N")
