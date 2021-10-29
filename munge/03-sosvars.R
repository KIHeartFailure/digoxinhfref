
pdata <- create_sosvar(
  type = "com",
  name = "afdur",
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  comduration = TRUE,
  diakod = " I48",
  valsclass = "fac",
  stoptime = -10 * 365.25,
  warnings = TRUE
)

pdata <- create_sosvar(
  type = "com",
  name = "afparoxysmal",
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  diakod = " I480",
  valsclass = "fac",
  stoptime = -5 * 365.25,
  warnings = TRUE
)

pdata <- create_sosvar(
  type = "com",
  name = "afpersistent",
  sosdata = patreg,
  cohortdata = pdata,
  patid = LopNr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = DIA_all,
  diakod = " I481",
  valsclass = "fac",
  stoptime = -5 * 365.25,
  warnings = TRUE
)