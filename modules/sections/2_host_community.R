
## ---- table 

host_table_data <- table_calc_comb %>% 
  filter(grepl('host', population_group, ignore.case = T)) %>% 
  mutate(location="Total") %>% 
  select(c(population_group,location,total_cases,total_deaths,total_cases_pm,total_deaths_pm,total_cases_7day,total_deaths_7day,   
           total_cases_pm_7day,total_deaths_pm_7day,case_growth,cfr)) 

summaryTable_host <- table_calc_comb_subloc %>% 
  ungroup() %>%
  filter(grepl('host', population_group, ignore.case=T)) %>% 
  bind_rows(host_table_data) %>% 
  #filter(!is.na(population)) %>% 
  select(-c(population,population_group,total_deaths_pm,total_deaths_pm_7day)) %>% 
  gt() %>% 
  cols_label(
    location = "Upazilla",
    total_cases = "Total",
    total_deaths = "Total",
    total_cases_pm = "Per million",
    total_cases_7day = "Last week",
    total_deaths_7day = "Last week",
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
    grand_summary_row.background.color = "lightblue")


## ---- graph 

testing_host_gph <-  test_nationality %>% 
  filter(name %in% c('host', 'host_positive')) %>% 
  mutate(name_fac=factor(name,levels=c('host', 'host_positive'), 
                         labels=c('Tests', 'Cases'))) %>% 
  #mutate(week=isoweek(date_format)) %>% 
  mutate(year_week=yearweek(date_format)) %>% 
  group_by(name_fac,year_week) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ggplot(., aes(x=year_week, y=value, fill=name_fac)) +
  geom_bar(stat="identity",position ="identity")   +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  # scale_x_date(date_breaks = '14 day', date_minor_breaks = '3 day',
  #              date_labels = '%d-%m') +
  labs(caption="Data source: Lab data",
       x = "Week",
       y = "Count",
       fill="") +
  theme_minimal()


## ---- map 

shp_file_host <- read_sf(here('data', 'shapefiles', 'host', 'cxb_shp.shp'))

case_shp_host <- table_calc_comb_subloc %>% 
  filter(population_group=='Host community') %>% 
  rename(upazilla = location) %>% 
  select(upazilla,total_cases, total_deaths) %>% 
  left_join(shp_file_host,.,by='upazilla') %>%    mutate(total_cases=ifelse(total_cases==0, NA, total_cases)) %>% 
  mutate(total_deaths=ifelse(total_deaths==0, NA, total_deaths))

#Define colour palettes based on available data
#This will need to be updated as cases increase
host_pal_cases_bins <-c(0, 250, 500, 1000, 2000, 4000, 6000)
host_pal_deaths_bins <-c(0, 5, 10, 25, 50,100,250)

host_pal_cases <- colorBin( "YlOrRd", bins=host_pal_cases_bins, na.color = "grey", pretty=FALSE)
host_pal_deaths <- colorBin( "PuRd", bins=host_pal_deaths_bins, na.color = "grey", pretty=FALSE)

host_popup <- paste(
  "<strong>Upazila: </strong>"
  , case_shp_host$upazilla
  , "<br><strong>Cases: </strong>"
  , case_shp_host$total_cases
  , "<br><strong>Deaths: </strong>"
  , case_shp_host$total_deaths
)

case_host_map <- leaflet(case_shp_host) %>% 
  addTiles() %>% 
  addPolygons(data=case_shp_host,
              color="black",
              fillColor = ~ host_pal_cases(total_cases),
              stroke = TRUE,   
              smoothFactor = 0.2,
              popup=host_popup,
              weight = 1,
              fillOpacity = .6, group="Cases") %>% 
  addLegend(pal = host_pal_cases, values = ~total_cases, title = "Cases", group="Cases", na.label="No reported cases") %>% 
  addPolygons(data=case_shp_host,
              color="black",
              fillColor = ~ host_pal_deaths(total_deaths),
              stroke = TRUE,   
              smoothFactor = 0.2,
              popup=host_popup,
              weight = 1,
              fillOpacity = .6, group="Deaths") %>% 
  addLegend(pal = host_pal_deaths, values = ~total_deaths, title = "Deaths", group="Deaths", na.label="No reported deaths") %>% 
  addLayersControl(baseGroups = c("Cases", "Deaths"), 
                   position = "topleft",
                   options = layersControlOptions(collapsed=F)) %>% hideGroup("Deaths")
