## ---- quarantine_table

quar_vars <- c('new_admissions_in_the_last_24_hours_individuals', 'current_occupancy_individuals', 
               'cumulative_contacts_individuals','cumulative_new_arrivals_travellers_individuals', 
               'number_of_rooms_shelters_currently_functional', 'number_of_rooms_shelters_currently_filled')  

quarantine_table <- quarantine_raw %>% 
  filter(!is.na(facility_name)) %>% 
  filter(!facility_name=='UNHCR/BRAC Camp 4Ext Block I') %>% 
  select(location_of_facility, facility_name,supporting_agency, 
         contains("individuals") ,
         number_of_rooms_shelters_currently_functional,number_of_rooms_shelters_currently_filled, timestamp) %>% 
  mutate(across(quar_vars, as.numeric)) %>% 
  mutate(prop_occupancy=number_of_rooms_shelters_currently_filled/number_of_rooms_shelters_currently_functional) %>% 
  gt() %>% 
  opt_row_striping(., row_striping = TRUE) %>% 
  tab_spanner(
    label = "Individuals",
    columns = vars(new_admissions_in_the_last_24_hours_individuals ,current_occupancy_individuals,
                   cumulative_contacts_individuals,cumulative_new_arrivals_travellers_individuals)
  ) %>% 
  tab_spanner(
    label = "Shelters",
    columns = vars(number_of_rooms_shelters_currently_functional,number_of_rooms_shelters_currently_filled,prop_occupancy)
  ) %>% 
  cols_label(
    location_of_facility = "Location",
    facility_name = "Facility name",
    supporting_agency = "Supporting agency",
    new_admissions_in_the_last_24_hours_individuals="New admissions (n)",
    current_occupancy_individuals="Current occupancy (n)",
    cumulative_contacts_individuals="Cumulative contacts (n)",
    cumulative_new_arrivals_travellers_individuals="Cumulative new arrivals (n)",
    number_of_rooms_shelters_currently_functional = "Functional (n)",
    number_of_rooms_shelters_currently_filled = "Filled (n)",
    prop_occupancy = "Filled (%)",
    timestamp="Last updated") %>% 
  summary_rows(fns = list(Total = ~ sum(.)), columns = vars(new_admissions_in_the_last_24_hours_individuals,current_occupancy_individuals,
                                                            cumulative_contacts_individuals,cumulative_new_arrivals_travellers_individuals,
                                                            number_of_rooms_shelters_currently_filled, number_of_rooms_shelters_currently_functional),
               formatter = fmt_number,
               decimals = 0,
               use_seps = TRUE) %>% 
  (function(x) {
    res <- function() x$`_data` %>% 
      dplyr::summarize(prop_occupancy = sum(number_of_rooms_shelters_currently_filled) / sum(number_of_rooms_shelters_currently_functional)) %>% 
      dplyr::pull(.data$prop_occupancy)
    
    summary_rows(x, fns = list(Total = ~ res()), columns = vars(prop_occupancy),
                 formatter = fmt_percent,
                 decimals = 0,
                 use_seps = TRUE)
  }) %>% 
  fmt_percent(
    columns = vars(prop_occupancy),
    decimals = 0
  ) %>% 
  tab_footnote(
    footnote = "In last 24 hours",
    locations = cells_column_labels(columns = vars(new_admissions_in_the_last_24_hours_individuals))
  ) %>% 
  data_color(
    columns = vars(prop_occupancy),
    colors = scales::col_numeric(
      "Reds",
      domain = c(0, 1), na.color = "grey89")
  ) %>% 
  tab_source_note("Data Source: Quarantine monitoring Google Sheet") %>% 
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


###Additional summary of information

quarantine_valuebox_df <- quarantine_raw %>% 
  filter(timestamp=='Totals') %>% 
  # select(new_admissions_in_the_last_24_hours_individuals)
  # select(new_admissions_24h)
  select(new_admissions_in_the_last_24_hours_individuals, cumulative_new_arrivals_travellers_individuals, 
         current_occupancy_individuals, cumulative_contacts_individuals, 
         number_of_rooms_shelters_currently_functional,number_of_rooms_shelters_currently_filled)

#New admissions
new_admissions_24h <- quarantine_valuebox_df %>%  
  pull(new_admissions_in_the_last_24_hours_individuals)
  
new_admissions_24h_vb <- valueBox(
  format(new_admissions_24h, big.mark = ","),
  icon= "fas users",
  color= "#ED7D31"
)

#Cumulative travellers
cumulative_travellers <- quarantine_valuebox_df %>%  
  pull(cumulative_new_arrivals_travellers_individuals)

cumulative_travellers_vb <- valueBox(
  format(cumulative_travellers, big.mark = ","),
  icon= "fas fa-users",
  color= "#ED7D31"
)

#Current number in quarantine
current_quarantine <- quarantine_valuebox_df %>%  
  pull(current_occupancy_individuals)

current_quarantine_vb <- valueBox(
  format(current_quarantine, big.mark = ","),
  icon= "fas fa-house-user",
  color= "#ED7D31"
)

#Cumulative contacts
cumulative_contacts <- quarantine_valuebox_df %>%  
  pull(cumulative_contacts_individuals)

cumulative_contacts_vb <- valueBox(
  format(cumulative_contacts, big.mark = ","),
  icon= "fas fa-users",
  color= "#ED7D31"
)


