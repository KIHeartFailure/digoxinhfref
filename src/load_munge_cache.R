# 1. Set options in config/global.dcf
# 2. Load packages listed in config/global.dcf
# 3. Import functions and code in lib directory
# 4. Load data in data directory
# 5. Run data manipulations in munge directory

ProjectTemplate::reload.project(
  reset = TRUE,
  data_loading = TRUE,
  munging = TRUE
)

ProjectTemplate::cache("meta.variables.Sheet.1")

ProjectTemplate::cache("flow")
ProjectTemplate::cache("pdata")
ProjectTemplate::cache("pdata_af")
ProjectTemplate::cache("pdata_noaf")

ProjectTemplate::cache("imp")
ProjectTemplate::cache("imp_af")
ProjectTemplate::cache("imp_noaf")

ProjectTemplate::cache("matchp")
ProjectTemplate::cache("matchingn")
ProjectTemplate::cache("matchp_af")
ProjectTemplate::cache("matchp_noaf")

ProjectTemplate::cache("matchpcross_deathhosphf")
ProjectTemplate::cache("matchpcross_death")
ProjectTemplate::cache("matchpcross_hosphf")
ProjectTemplate::cache("matchpcross_af_deathhosphf")
ProjectTemplate::cache("matchpcross_af_death")
ProjectTemplate::cache("matchpcross_af_hosphf")
ProjectTemplate::cache("matchpcross_noaf_deathhosphf")
ProjectTemplate::cache("matchpcross_noaf_death")
ProjectTemplate::cache("matchpcross_noaf_hosphf")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("metaout")
ProjectTemplate::cache("overtime")
ProjectTemplate::cache("overtime_af")
ProjectTemplate::cache("overtime_noaf")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarsstrata")
ProjectTemplate::cache("stratavars")
