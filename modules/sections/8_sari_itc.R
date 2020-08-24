num_vars <- c('number_of_functional_sari_beds', 'number_of_currently_filled_sari_beds','number_of_expected_sari_discharges_in_next_24_hours',
              'number_of_functional_isolation_beds_non_sari','number_of_currently_filled_isolation_beds_non_sari','number_of_expected_isolation_discharges_in_next_24_hours_non_sari' )

clean_fn <- function(x){
  ifelse(grepl("NULL", x),0,x)
}



dru_gt_table <- dru_raw %>%
  clean_names() %>% 
  select(timestamp,facility_name, camp, agency_in_charge, currently_accepting_patient_severity, 
         able_to_manage_special_needs_of_covid_19_cases_tick_all_that_apply, contains('sari'), contains('isolation')) %>%
 # mutate(number_of_expected_sari_discharges_in_next_24_hours=ifelse(grepl("NULL", number_of_expected_sari_discharges_in_next_24_hours),0,number_of_expected_sari_discharges_in_next_24_hours)) %>% 
 mutate(across(num_vars,clean_fn)) %>% 
 #mutate(number_of_expected_sari_discharges_in_next_24_hours=ifelse(is.null(number_of_expected_sari_discharges_in_next_24_hours),0,number_of_expected_sari_discharges_in_next_24_hours)) %>% 
  mutate(mild=ifelse(grepl('Mild', currently_accepting_patient_severity) , '+', ''),
         moderate=ifelse(grepl('Moderate', currently_accepting_patient_severity) , '+', ''),
         severe=ifelse(grepl('Severe', currently_accepting_patient_severity) , '+', ''),
         paed=ifelse(grepl('Pead|Paed', able_to_manage_special_needs_of_covid_19_cases_tick_all_that_apply) , '+', ''),
         sam=ifelse(grepl('SAM', able_to_manage_special_needs_of_covid_19_cases_tick_all_that_apply) , '+', ''),
         preg=ifelse(grepl('Preg', able_to_manage_special_needs_of_covid_19_cases_tick_all_that_apply) , '+', ''),
         across(num_vars, as.numeric), 
         prop_filled_sari=(ifelse(number_of_currently_filled_sari_beds>0, (number_of_currently_filled_sari_beds/number_of_functional_sari_beds),NA)),
         prop_filled_iso=(ifelse(number_of_currently_filled_isolation_beds_non_sari>0, (number_of_currently_filled_isolation_beds_non_sari/number_of_functional_isolation_beds_non_sari),NA)),
         camp= gsub("Camp", "", camp), 
         count_beds=(number_of_functional_sari_beds+number_of_functional_isolation_beds_non_sari), 
         filled_beds=(number_of_currently_filled_sari_beds+number_of_currently_filled_isolation_beds_non_sari),
         prop_filled_total=filled_beds/count_beds) %>% 
  select(facility_name, camp, mild, moderate, severe, paed, sam, preg,
         number_of_functional_sari_beds,number_of_currently_filled_sari_beds,prop_filled_sari, number_of_expected_sari_discharges_in_next_24_hours,
         number_of_functional_isolation_beds_non_sari,number_of_currently_filled_isolation_beds_non_sari, prop_filled_iso, number_of_expected_isolation_discharges_in_next_24_hours_non_sari, 
         count_beds, filled_beds, prop_filled_total, timestamp) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  gt() %>% 
  opt_row_striping(., row_striping = TRUE) %>% 
  tab_spanner(
    label = "Currently accepting?",
    columns = vars(mild,moderate,severe)
  ) %>% 
  tab_spanner(
    label = "Severe beds",
    columns = vars(number_of_functional_sari_beds, number_of_currently_filled_sari_beds,prop_filled_sari, number_of_expected_sari_discharges_in_next_24_hours)
  ) %>% 
  tab_spanner(
    label = "Isolation beds",
    columns = vars(number_of_functional_isolation_beds_non_sari, number_of_currently_filled_isolation_beds_non_sari,prop_filled_iso, number_of_expected_isolation_discharges_in_next_24_hours_non_sari)
  ) %>% 
  tab_spanner(
    label = "Accepting special needs patients?",
    columns = vars(paed, sam,preg)
  ) %>% 
  tab_footnote(
    footnote = "Expected in next 24 hours",
    locations = cells_column_labels(columns = vars(number_of_expected_sari_discharges_in_next_24_hours,number_of_expected_isolation_discharges_in_next_24_hours_non_sari))
  ) %>% 
  tab_footnote(
    footnote = "Severe Acute Malnutrition",
    locations = cells_column_labels(columns = vars(sam))
  ) %>% 
  tab_footnote(
    footnote = "Postpartum",
    locations = cells_column_labels(columns = vars(preg))
  ) %>% 
  tab_footnote(
    footnote = "Beds with access to 24/7 oxygen",
    locations = cells_column_spanners(spanners = "Severe beds")
  ) %>% 
  # summary_rows(fns = list(Total = ~ sum(.)), columns = vars(number_of_functional_sari_beds,number_of_currently_filled_sari_beds,
  #                                                           count_beds ),
  #              formatter = fmt_number,
  #              decimals = 0,
  #              use_seps = TRUE) %>%
  summary_rows(fns = list(Total = ~ sum(.)), columns = vars(number_of_functional_sari_beds, number_of_currently_filled_sari_beds, number_of_expected_sari_discharges_in_next_24_hours,
                                                            number_of_functional_isolation_beds_non_sari, number_of_currently_filled_isolation_beds_non_sari, number_of_expected_isolation_discharges_in_next_24_hours_non_sari,
                                                            count_beds, filled_beds),
               formatter = fmt_number,
               decimals = 0,
               use_seps = TRUE) %>%
  (function(x) {
    res <- function() x$`_data` %>%
      dplyr::summarize(prop_filled_sari = sum(number_of_currently_filled_sari_beds) / sum(number_of_functional_sari_beds)) %>%
      dplyr::pull(.data$prop_filled_sari)
    
    summary_rows(x, fns = list(Total = ~ res()), columns = vars(prop_filled_sari),
                 formatter = fmt_percent,
                 decimals = 0,
                 use_seps = TRUE)
  }) %>%
  (function(x) {
    res <- function() x$`_data` %>%
      dplyr::summarize(prop_filled_iso = sum(number_of_currently_filled_isolation_beds_non_sari) / sum(number_of_functional_isolation_beds_non_sari)) %>%
      dplyr::pull(.data$prop_filled_iso)
    
    summary_rows(x, fns = list(Total = ~ res()), columns = vars(prop_filled_iso),
                 formatter = fmt_percent,
                 decimals = 0,
                 use_seps = TRUE)
  }) %>%
  (function(x) {
    res <- function() x$`_data` %>%
      dplyr::summarize(prop_filled_total = sum(number_of_currently_filled_isolation_beds_non_sari + number_of_currently_filled_sari_beds) / sum(number_of_functional_isolation_beds_non_sari + number_of_functional_sari_beds)) %>%
      dplyr::pull(.data$prop_filled_total)
    
    summary_rows(x, fns = list(Total = ~ res()), columns = vars(prop_filled_total),
                 formatter = fmt_percent,
                 decimals = 0,
                 use_seps = TRUE)
  }) %>%
  fmt_percent(
    columns = vars(prop_filled_sari,prop_filled_iso,prop_filled_total),
    decimals = 0
  ) %>% 
  fmt_missing(
    columns = 4:18,
    missing_text = ""
  ) %>% 
  cols_label(
    facility_name = "Facility name",
    camp = "Camp",
    mild = "Mild",
    moderate = "Moderate",
    severe = "Severe",
    paed = "Paediatric",
    sam = "SAM",
    preg = "Pregnant/PP",
    number_of_functional_sari_beds = "Functional (n)",
    number_of_currently_filled_sari_beds = "Filled (n)",
    prop_filled_sari = "Filled (%)",
    number_of_expected_sari_discharges_in_next_24_hours = "Discharges (n)",
    number_of_functional_isolation_beds_non_sari = "Functional (n)",
    number_of_currently_filled_isolation_beds_non_sari = "Filled (n)",
    prop_filled_iso = "Filled (%)",
    number_of_expected_isolation_discharges_in_next_24_hours_non_sari = "Discharges (n)",
    count_beds = "Total beds (n)",
    filled_beds = "Total filled (n)",
    prop_filled_total = "Total filled (%)" ,
    timestamp = "Last updated"
  ) %>% 
  data_color(
    columns = vars(prop_filled_sari,prop_filled_iso,prop_filled_total),
    colors = scales::col_numeric(
      "Reds",
      domain = c(0, 1.5), na.color = "grey89")
  ) %>% 
  tab_source_note("Data Source: DRU Live Bed monitoring Google Sheet") %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = gt::everything()
  ) %>% 
  tab_options(
    container.overflow.x = TRUE,
    container.overflow.y = TRUE,
    grand_summary_row.background.color = "white")
