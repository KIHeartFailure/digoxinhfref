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
ProjectTemplate::cache("imp")
ProjectTemplate::cache("matchp")
ProjectTemplate::cache("matchingn")

ProjectTemplate::cache("matchpcross_deathhosphf")
ProjectTemplate::cache("matchpcross_death")
ProjectTemplate::cache("matchpcross_hosphf")

ProjectTemplate::cache("metalm")
ProjectTemplate::cache("overtime")

ProjectTemplate::cache("tabvars")
ProjectTemplate::cache("modvars")
ProjectTemplate::cache("modvarsstrata")
ProjectTemplate::cache("stratavars")
