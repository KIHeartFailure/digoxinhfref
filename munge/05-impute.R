

# Impute missing values ---------------------------------------------------

noimpvars <- names(pdata)[!names(pdata) %in% modvars]

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(sos_outtime_hosphf, sos_out_deathhosphf == "Yes") ~ 1,
  data = pdata, method = "breslow"
))
pdata <- left_join(pdata, na, by = c("sos_outtime_hosphf" = "time"))

ini <- mice(pdata, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "shf_nyha")] <- "polr"
meth[noimpvars] <- ""

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imp <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "pdata"),
    .packages = "mice"
  ) %dopar% {
    mice(pdata,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# Impute missing values AF -------------------------------------------------

noimpvars <- c(names(pdata_af)[!names(pdata_af) %in% modvars], "shf_sos_com_af")

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(sos_outtime_hosphf, sos_out_deathhosphf == "Yes") ~ 1,
                    data = pdata_af, method = "breslow"
))
pdata_af <- left_join(pdata_af, na, by = c("sos_outtime_hosphf" = "time"))

ini <- mice(pdata_af, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "shf_nyha")] <- "polr"
meth[noimpvars] <- ""

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imp_af <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "pdata_af"),
    .packages = "mice"
  ) %dopar% {
    mice(pdata_af,
         m = m_2_use, maxit = 10, method = meth,
         predictorMatrix = pred,
         printFlag = FALSE
    )
  }
stopImplicitCluster()

# Impute missing values No AF ----------------------------------------------

noimpvars <- c(names(pdata_noaf)[!names(pdata_noaf) %in% modvars], "shf_sos_com_af")

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(sos_outtime_hosphf, sos_out_deathhosphf == "No") ~ 1,
                    data = pdata_noaf, method = "breslow"
))
pdata_noaf <- left_join(pdata_noaf, na, by = c("sos_outtime_hosphf" = "time"))

ini <- mice(pdata_noaf, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "shf_nyha")] <- "polr"
meth[noimpvars] <- ""

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imp_noaf <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "pdata_noaf"),
    .packages = "mice"
  ) %dopar% {
    mice(pdata_noaf,
         m = m_2_use, maxit = 10, method = meth,
         predictorMatrix = pred,
         printFlag = FALSE
    )
  }
stopImplicitCluster()
