

# Propensity scores -------------------------------------------------------

ps <- data.frame(matrix(NA, nrow = nrow(pdata), ncol = 11))

for (i in 1:10) {
  impdata_ps <- mice::complete(imp, i)
  if (i == 1) ps[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0(
    "ddr_digoxinnum ~ ",
    paste(modvars,
      collapse = " + "
    )
  )),
  data = impdata_ps,
  family = binomial
  )
  ps[, i + 1] <- pslog$fitted
}

pdata <- left_join(pdata,
  ps %>%
    mutate(ps = rowSums(.[2:11]) / 10) %>%
    select(X1, ps),
  by = c("LopNr" = "X1")
)

cal <- c(0.01 / sd(pdata$ps))

set.seed(2334325)
match1 <- Match(
  Tr = pdata$ddr_digoxinnum,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)
set.seed(2334325)
match2 <- Match(
  Tr = pdata$ddr_digoxinnum,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 2
)
set.seed(2334325)
match3 <- Match(
  Tr = pdata$ddr_digoxinnum,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 3
)
set.seed(2334325)
match4 <- Match(
  Tr = pdata$ddr_digoxinnum,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 4
)
set.seed(2334325)
match5 <- Match(
  Tr = pdata$ddr_digoxinnum,
  X = pdata$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 5
)
matchingn <- paste0(
  "1:1: N = ", match1$wnobs, ", ",
  "1:2: N = ", match2$wnobs, ", ",
  "1:3: N = ", match3$wnobs, ", ",
  "1:4: N = ", match4$wnobs, ", ",
  "1:5: N = ", match5$wnobs
)

pdata$par <- rep(NA, nrow(pdata))

pdata$par[c(unique(match1$index.treated), match1$index.control)] <- c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))
matchp <- pdata[c(unique(match1$index.treated), match1$index.control), ]


# AF ----------------------------------------------------------------------

ps_af <- data.frame(matrix(NA, nrow = nrow(pdata_af), ncol = 11))

for (i in 1:10) {
  impdata_ps <- mice::complete(imp_af, i)
  if (i == 1) ps_af[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0(
    "ddr_digoxinnum ~ ",
    paste(modvars[modvars != "shf_sos_com_af"],
          collapse = " + "
    )
  )),
  data = impdata_ps,
  family = binomial
  )
  ps_af[, i + 1] <- pslog$fitted
}

pdata_af <- left_join(pdata_af,
                   ps_af %>%
                     mutate(ps = rowSums(.[2:11]) / 10) %>%
                     select(X1, ps),
                   by = c("LopNr" = "X1")
)

cal <- c(0.01 / sd(pdata_af$ps))

set.seed(2334325)
match1 <- Match(
  Tr = pdata_af$ddr_digoxinnum,
  X = pdata_af$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)

pdata_af$par <- rep(NA, nrow(pdata_af))

pdata_af$par[c(unique(match1$index.treated), match1$index.control)] <- c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))
matchp_af <- pdata_af[c(unique(match1$index.treated), match1$index.control), ]


# No AF --------------------------------------------------------------------

ps_noaf <- data.frame(matrix(NA, nrow = nrow(pdata_noaf), ncol = 11))

for (i in 1:10) {
  impdata_ps <- mice::complete(imp_noaf, i)
  if (i == 1) ps_noaf[, 1] <- impdata_ps$LopNr
  pslog <- glm(formula(paste0(
    "ddr_digoxinnum ~ ",
    paste(modvars[modvars != "shf_sos_com_af"],
          collapse = " + "
    )
  )),
  data = impdata_ps,
  family = binomial
  )
  ps_noaf[, i + 1] <- pslog$fitted
}

pdata_noaf <- left_join(pdata_noaf,
                      ps_noaf %>%
                        mutate(ps = rowSums(.[2:11]) / 10) %>%
                        select(X1, ps),
                      by = c("LopNr" = "X1")
)

cal <- c(0.01 / sd(pdata_noaf$ps))

set.seed(2334325)
match1 <- Match(
  Tr = pdata_noaf$ddr_digoxinnum,
  X = pdata_noaf$ps,
  estimand = "ATT",
  caliper = cal,
  replace = F,
  ties = F,
  M = 1
)

pdata_noaf$par <- rep(NA, nrow(pdata_noaf))

pdata_noaf$par[c(unique(match1$index.treated), match1$index.control)] <- c(1:match1$wnobs, rep(1:match1$wnobs, each = 1))
matchp_noaf <- pdata_noaf[c(unique(match1$index.treated), match1$index.control), ]
