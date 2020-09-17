

# Variables for tabs/mods -------------------------------------------------


tabvars <- c(
  # demo
  "shf_sex",
  "shf_age",
  "shf_age_cat",

  # organizational
  "shf_indexyear",
  "shf_indexyear_cat",
  "shf_location",
  "shf_followuphfunit",
  "shf_followuplocation",

  # clinical factors and lab measurments
  "shf_ef",
  "shf_durationhf",
  "shf_nyha",
  "shf_nyha_cat",
  "shf_bmi",
  "shf_bmi_cat",
  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_map_cat",
  "shf_heartrate",
  "shf_heartrate_cat",
  "shf_gfrckdepi",
  "shf_ckd",
  "shf_potassium",
  "shf_potassium_cat",
  "shf_hb",
  "shf_anemia",
  "shf_ntpropbnp",
  "shf_ntpropbnp_cat",

  # treatments
  "shf_rasarni",
  "shf_mra",
  "shf_diuretic",
  "shf_nitrate",
  "shf_asaantiplatelet",
  "shf_anticoagulantia",
  "shf_statin",
  "shf_bbl",
  "shf_device_cat",

  # comorbs
  "shf_smoking_cat",
  "shf_sos_com_diabetes",
  "shf_sos_com_hypertension",
  "shf_sos_com_ihd",
  "sos_com_peripheralartery",
  "sos_com_stroke",
  "shf_sos_com_af",
  "sos_com_valvular",
  "sos_com_liver",
  "sos_com_cancer3y",
  "sos_com_copd",

  # socec
  "scb_famtype",
  "scb_child",
  "scb_education",
  "scb_dispincome_cat2"
)

# vars fox log reg and cox reg
tabvars_not_in_mod <- c(
  "shf_age",

  "shf_indexyear",

  "shf_nyha_cat",

  "shf_bpsys",
  "shf_bpdia",
  "shf_map",
  "shf_heartrate",
  "shf_gfrckdepi",
  "shf_hb",
  "shf_ntpropbnp",
  "shf_potassium",

  "shf_bmi"
)

modvars <- tabvars[!(tabvars %in% tabvars_not_in_mod)]

modvarsstrata <- modvars
stratavars <- c("shf_age_cat", "shf_location", "shf_sos_com_ihd")
modvarsstrata[modvars %in% stratavars] <- paste0("strata(", stratavars, ")")
