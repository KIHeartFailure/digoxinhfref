

# Additional variables from mainly SHF ------------------------------------

pdata <- pdata %>%
  mutate(
    shf_age_cat = case_when(
      shf_age < 75 ~ "<75",
      shf_age >= 75 ~ ">=75"
    ),

    shf_indexyear_cat = case_when(
      shf_indexyear <= 2010 ~ "2005-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2018 ~ "2016-2018"
    ),

    shf_nyha_cat = case_when(
      shf_nyha %in% c("I", "II") ~ "I-II",
      shf_nyha %in% c("III", "IV") ~ "III-IV"
    ),

    shf_smoking_cat = factor(case_when(
      shf_smoking %in% c("Former", "Never") ~ 1,
      shf_smoking == "Current" ~ 2
    ),
    labels = c("Former/Never", "Current"),
    levels = 1:2
    ),

    shf_map_cat = case_when(
      shf_map <= 90 ~ "<=90",
      shf_map > 90 ~ ">90"
    ),

    shf_potassium_cat = factor(
      case_when(
        is.na(shf_potassium) ~ NA_real_,
        shf_potassium < 3.5 ~ 2,
        shf_potassium <= 5 ~ 1,
        shf_potassium > 5 ~ 3
      ),
      labels = c("normakalemia", "hypokalemia", "hyperkalemia"),
      levels = 1:3
    ),

    shf_heartrate_cat = case_when(
      shf_heartrate <= 70 ~ "<=70",
      shf_heartrate > 70 ~ ">70"
    ),

    shf_device_cat = factor(case_when(
      is.na(shf_device) ~ NA_real_,
      shf_device %in% c("CRT", "CRT & ICD", "ICD") ~ 2,
      TRUE ~ 1
    ),
    labels = c("No", "CRT/ICD"),
    levels = 1:2
    ),

    shf_bmi_cat = case_when(
      is.na(shf_bmi) ~ NA_character_,
      shf_bmi < 30 ~ "<30",
      shf_bmi >= 30 ~ ">=30"
    ),

    shf_ckd = factor(case_when(
      is.na(shf_gfrckdepi) ~ NA_real_,
      shf_gfrckdepi >= 60 ~ 2,
      shf_gfrckdepi >= 30 ~ 1,
      shf_gfrckdepi < 30 ~ 3,
    ),
    labels = c("30-60", ">=60", "<30"),
    levels = 1:3
    ),

    shf_sos_com_af = case_when(
      sos_com_af == "Yes" |
        shf_af == "Yes" |
        shf_ekg == "Atrial fibrillation" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_ihd = case_when(
      sos_com_ihd == "Yes" |
        shf_revasc == "Yes" |
        sos_com_pci == "Yes" |
        sos_com_cabg == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_hypertension = case_when(
      shf_hypertension == "Yes" |
        sos_com_hypertension == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    shf_sos_com_diabetes = case_when(
      shf_diabetes == "Yes" |
        sos_com_diabetes == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    
    # Anemia
    shf_anemia = case_when(
      is.na(shf_hb) ~ NA_character_,
      shf_sex == "Female" & shf_hb < 120 | shf_sex == "Male" & shf_hb < 130 ~ "Yes",
      TRUE ~ "No"
    ),

    # Outcomes

    # all times min 14 days, set to 0
    sos_outtime_death = sos_outtime_death - 14,
    sos_outtime_hosphf = sos_outtime_hosphf - 14,

    # composite outcome
    sos_out_deathhosphf = case_when(
      sos_out_death == "Yes" |
        sos_out_hosphf == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),

    # competing event outcome
    sos_out_hosphf_comp = case_when(
      sos_out_hosphf == "Yes" ~ 1,
      sos_out_death == "Yes" ~ 2,
      TRUE ~ 0
    )
  )


# income

inc <- pdata %>%
  group_by(shf_indexyear) %>%
  summarise(incmed = quantile(scb_dispincome,
    probs = 0.5,
    na.rm = TRUE
  ), .groups = "drop_last")

pdata <- left_join(
  pdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat2 = case_when(
      scb_dispincome < incmed ~ 1,
      scb_dispincome >= incmed ~ 2
    ),
    scb_dispincome_cat2 = factor(scb_dispincome_cat2,
      levels = 1:2,
      labels = c("Below medium within year", "Above medium within year")
    )
  ) %>%
  select(-incmed)

# ntprobnp

ntprobnp <- pdata %>%
  group_by(shf_ef) %>%
  summarise(
    ntmed = quantile(shf_ntpropbnp,
      probs = 0.5,
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

pdata <- left_join(
  pdata,
  ntprobnp,
  by = c("shf_ef")
) %>%
  mutate(
    shf_ntpropbnp_cat = case_when(
      shf_ntpropbnp < ntmed ~ 1,
      shf_ntpropbnp >= ntmed ~ 2
    ),
    shf_ntpropbnp_cat = factor(shf_ntpropbnp_cat,
      levels = 1:2,
      labels = c("Below medium within EF", "Above medium within EF")
    )
  ) %>%
  select(-ntmed)

pdata <- pdata %>%
  mutate_if(is_character, factor)
