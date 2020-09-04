

# FDMN TABLE --------------------------------------------------------------


fdmn_table_data <- table_calc_comb %>% 
  filter(grepl('fdmn', population_group, ignore.case = T)) %>% 
  mutate(location="Total") %>% 
  select(c(population_group,location,total_cases,total_deaths,total_cases_pm,total_deaths_pm,total_cases_7day,total_deaths_7day,   
           total_cases_pm_7day,total_deaths_pm_7day,case_growth,cfr)) 

summaryTable_fdmn <- table_calc_comb_subloc %>% 
  ungroup() %>%
  filter(grepl('fdmn', population_group, ignore.case=T)) %>% 
  bind_rows(fdmn_table_data) %>% 
  mutate(location=case_when(grepl('ukhia', location, ignore.case=T)~'Camp missing (Ukhia)',
                            grepl('teknaf', location, ignore.case=T)~'Camp missing (Teknaf)',
                            TRUE ~ location)) %>% 
  mutate(total_cases_pm=ifelse(grepl('ukhia', location, ignore.case=T),NA, 
                               ifelse(grepl('teknaf', location, ignore.case=T), NA, total_cases_pm))) %>% 
  mutate(total_cases_pm_7day=ifelse(grepl('ukhia', location, ignore.case=T),NA, 
                                    ifelse(grepl('teknaf', location, ignore.case=T), NA, total_cases_pm_7day))) %>% 
  mutate(camp=location) %>% 
  mutate(camp_number=str_extract(camp, regexp)) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  select(-c(population_group,camp,total_deaths_pm,total_deaths_pm_7day,camp_number)) %>% 
  gt() %>% 
  cols_label(
    location = "Camp",
    total_cases = "Total",
    total_deaths = "Total",
    total_cases_pm = "Per million",
    total_cases_7day = "Last 7 days",
    total_deaths_7day = "Last 7 days",
    total_cases_pm_7day   = "Per million",
    cfr = "Case Fatality Risk", 
    case_growth = "Growth rate"
  ) %>% 
  tab_spanner(
    label = "Cases",
    columns = vars(total_cases, total_cases_pm,total_cases_7day,total_cases_pm_7day,case_growth)
  ) %>% 
  tab_spanner(
    label = "Deaths",
    columns = vars(total_deaths,total_deaths_7day, cfr)
  ) %>% 
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
  fmt_percent(
    columns = vars(case_growth, cfr),
    decimals = 1
  ) %>% 
  fmt_number(
    columns=vars(total_cases_pm, total_cases_pm_7day),
    decimals = 1
  ) %>% 
  tab_source_note("Data Source: FDMN/Host Linelist") %>% 
  tab_options(
    container.overflow.x = TRUE,
    container.overflow.y = TRUE,
    grand_summary_row.background.color = "lightblue") %>% 
  fmt_missing(
    columns = 2:8,
    missing_text = "-"
  ) %>% 
  tab_footnote(
    footnote = "Growth rate can only be calculated when there have been at least 10 cases",
    locations = cells_column_labels(
      columns = vars(case_growth))
  )



# FDMN GRAPH --------------------------------------------------------------


testing_fdmn_gph <-  test_nationality %>% 
  filter(name %in% c('fdmn', 'fdmn_positive')) %>% 
  mutate(name_fac=factor(name,levels=c('fdmn', 'fdmn_positive'), 
                         labels=c('Tests', 'Cases'))) %>% 
  mutate(week=isoweek(date_format)) %>% 
  group_by(name_fac,week) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ggplot(., aes(x=week, y=value, fill=name_fac)) +
  geom_bar(stat="identity",position ="identity")   +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  # scale_x_date(date_breaks = '14 day', date_minor_breaks = '3 day',
  #              date_labels = '%d-%m') +
  labs(caption="Data source: Lab data",
       x = "Week",
       y = "Count",
       fill="") +
  theme_minimal()


# FDMN MAP ----------------------------------------------------------------

shp_file_fdmn <- read_sf(list.files(here('data','shapefiles'), 
                                    pattern='1.shp$', 
                                    full.names = T)) %>% 
  
  clean_names() %>% 
  mutate(merge_col = case_when(code=='KRC' ~ 'Kutupalong RC',
                               code=='NRC' ~ 'Nyapara RC', 
                               TRUE ~ code))

case_shp_fdmn <- table_calc_comb_subloc %>% 
  filter(population_group=='Rohingya refugee/FDMN') %>% 
  filter(!location %in% c('Ukhia', 'Teknaf')) %>% 
  mutate(code=location) %>% 
  select(code,total_cases, total_deaths) %>% 
  left_join(shp_file_fdmn,.,by=c('merge_col'='code')) %>% 
  mutate(total_cases=ifelse(total_cases==0, NA, total_cases)) %>% 
  mutate(total_deaths=ifelse(total_deaths==0, NA, total_deaths))

fdmn_pal_cases_bins <-c(0, 1, 2, 5, 10, 20)
fdmn_pal_deaths_bins <-c(0, 1, 2)


fdmn_pal_cases <- colorBin( "YlOrRd", bins=fdmn_pal_cases_bins, na.color = "grey", pretty=FALSE)
fdmn_pal_deaths <- colorBin( "PuRd", bins=fdmn_pal_deaths_bins, na.color = "grey", pretty=FALSE)


fdmn_popup <- paste(
  "<strong>Camp: </strong>"
  , case_shp_fdmn$code
  , "<br><strong>Cases: </strong>"
  , case_shp_fdmn$total_cases
  , "<br><strong>Deaths: </strong>"
  , case_shp_fdmn$total_deaths
)




case_fdmn_map <- 
leaflet(case_shp_fdmn) %>% 
  addTiles() %>% 
  addPolygons(data=case_shp_fdmn,
              color="black",
              fillColor = ~ fdmn_pal_cases(total_cases),
              stroke = TRUE,   
              smoothFactor = 0.2,
              popup=fdmn_popup,
              weight = 1,
              fillOpacity = .6, group="Cases") %>% 
  addLegend(pal = fdmn_pal_cases, values = ~total_cases, title = "Cases", group="Cases", na.label="No reported cases") %>% 
  addPolygons(data=case_shp_fdmn,
              color="black",
              fillColor = ~ fdmn_pal_deaths(total_deaths),
              stroke = TRUE,   
              smoothFactor = 0.2,
              popup=fdmn_popup,
              weight = 1,
              fillOpacity = .6, group="Deaths") %>% 
  addLegend(pal = fdmn_pal_deaths, values = ~total_deaths, title = "Deaths", group="Deaths", na.label="No reported deaths") %>% 
  addLayersControl(baseGroups = c("Cases", "Deaths"), 
                   position = "topleft",
                   options = layersControlOptions(collapsed=F)) %>% hideGroup("Deaths")

