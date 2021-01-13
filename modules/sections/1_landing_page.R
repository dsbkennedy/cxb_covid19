


# HOST --------------------------------------------------------------------

##Host community placeholder

host_placeholder <- valueBox(
  format(paste0('Host community'), big.mark = ","),
  icon= "",
  color= "#ED7D31"
)


##Host community tests

host_tests <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  pull(total_tests)
host_tests_7day <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  pull(total_tests_7day)
host_tests_1day <- table_1day %>%  
  filter(population_group=='Host community') %>%  pull(total_tests_1day)

host_tests_vb <- valueBox(
  format(paste0(host_tests,' - ', host_tests_7day, ' - ', host_tests_1day), big.mark = ","),
  icon= "fas fa-vials",
  color= "#ED7D31"
)

##Host community cases

host_cases <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  
  pull(total_cases)
host_cases_7day <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  
  pull(total_cases_7day)
host_cases_1day <- table_1day %>%  
  filter(population_group=='Host community') %>%  pull(total_cases_1day)

host_cases_vb <- valueBox(
  format(paste0(host_cases,' - ', host_cases_7day, ' - ', host_cases_1day), big.mark = ","),
  icon = "fas fa-user-md",
  color= "#ED7D31"
)

##Host community deaths

host_deaths <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  
  pull(total_deaths)
host_deaths_7day <- table_calc_comb %>%  
  filter(population_group=='Host community') %>%  
  pull(total_deaths_7day)
host_deaths_1day <- table_1day %>%  
  filter(population_group=='Host community') %>%  
  pull(total_deaths_1day)


host_deaths_vb <- valueBox(
  format(paste0(host_deaths,' - ', host_deaths_7day, ' - ', host_deaths_1day), big.mark = ","),
  icon = "fas fa-procedures",
  color= "#ED7D31"
)


# FDMN --------------------------------------------------------------------

fdmn_placeholder <- valueBox(
  format(paste0('Rohingya/FDMN'), big.mark = ","),
  icon= "",
  color= "#4472C4"
)


#Testing 

fdmn_tests <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_tests)
fdmn_tests_7day <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_tests_7day)
fdmn_tests_1day <- table_1day %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_tests_1day)

fdmn_tests_vb <- valueBox(
  format(paste0(fdmn_tests,' - ', fdmn_tests_7day, ' - ', fdmn_tests_1day), big.mark = ","),
  icon= "fas fa-vials",
  color= "#4472C4"
)

##Cases

fdmn_cases <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_cases)
fdmn_cases_7day <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_cases_7day)
fdmn_cases_1day <- table_1day %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_cases_1day)

fdmn_cases_vb <- valueBox(
  format(paste0(fdmn_cases,' - ', fdmn_cases_7day, ' - ', fdmn_cases_1day), big.mark = ","),
  icon = "fas fa-user-md",
  color= "#4472C4"
)

##Deaths

fdmn_deaths <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_deaths)

fdmn_deaths_7day <- table_calc_comb %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_deaths_7day)

fdmn_deaths_1day <- table_1day %>%  
  filter(population_group=='Rohingya refugee/FDMN') %>%  
  pull(total_deaths_1day)

fdmn_deaths_vb <- valueBox(
  format(paste0(fdmn_deaths,' - ', fdmn_deaths_7day, ' - ', fdmn_deaths_1day), big.mark = ","),
  icon = "fas fa-procedures",
  color= "#4472C4"
)


# TESTS-TABLE -------------------------------------------------------------

all_tests_table <- table_calc_comb %>% 
  ungroup() %>%
  select(population_group, contains('test')) %>% 
  select(population_group, total_tests,total_tests_pm, test_pos, 
         total_tests_7day, total_tests_pm_7day, test_pos_7day) %>%
  mutate(test_pos_7day=ifelse(test_pos_7day>0.5,NA,test_pos_7day)) %>% 
  gt(rowname_col = "population_group") %>% 
  tab_stubhead(label = "Population group") %>% 
  cols_label(
    total_tests = "Total",
    total_tests_pm = "Per million",
    total_tests_7day = "Last 7 days", 
    total_tests_pm_7day   = "Per million",
    test_pos = "% positive" ,
    test_pos_7day = "% positive" 
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
    columns = vars(test_pos,test_pos_7day),
    decimals = 1
  ) %>% 
  fmt_number(
    columns=vars(total_tests_pm,total_tests_pm_7day),
    decimals = 1
  )


# CASES-TABLE -------------------------------------------------------------

all_cases_table <- table_calc_comb %>% 
  ungroup() %>%
  select(population_group, contains('case')) %>% 
  gt(rowname_col = "population_group") %>% 
  tab_stubhead(label = "Population group") %>% 
  cols_label(
    total_cases = "Total",
    total_cases_pm = "Per million",
    total_cases_7day = "Last 7 days",
    total_cases_pm_7day   = "Per million",
    case_growth = "Growth rate"
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
    columns = vars(case_growth),
    decimals = 1
  ) %>% 
  fmt_number(
    columns=vars(total_cases_pm, total_cases_pm_7day),
    decimals = 1
  )



# DEATHS-TABLE ------------------------------------------------------------

all_deaths_table <- table_calc_comb %>% 
  ungroup() %>%
  select(population_group, contains('death'), cfr) %>% 
  gt(rowname_col = "population_group") %>% 
  tab_stubhead(label = "Population group") %>% 
  cols_label(
    total_deaths = "Total",
    total_deaths_pm = "Per million", 
    total_deaths_7day = "Last 7 days",
    total_deaths_pm_7day  = "Per million", 
    cfr = "Case Fatality Risk"
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
    columns = vars(cfr),
    decimals = 1
  ) %>% 
  fmt_number(
    columns=vars(total_deaths_pm,total_deaths_pm_7day),
    decimals = 1
  )



# EPICURVE ----------------------------------------------------------------

epi_curve <- table_final_df %>% 
  mutate(week=isoweek(date)) %>% 
  #mutate(year_week=yearweek(date)) %>% 
  group_by(population_group,week) %>% 
  summarise(new_cases=sum(new_cases,na.rm=TRUE)) %>% 
  ggplot(.) +
  geom_col(aes(x = week, y = new_cases)) +
  # scale_x_date(date_breaks = '14 day', date_minor_breaks = '7 day',
  #              date_labels = '%d-%m') +
  theme_minimal() +
  labs(x = "Week case reported",
       y = "Number of cases") +
  facet_wrap(~ population_group, scales="free_y", ncol=1)


# TEST-POSITIVITY ---------------------------------------------------------

# test_positivity_gph <- test_nationality %>% 
#   filter(name %in% c('host', 'fdmn','host_positive', 'fdmn_positive')) %>% 
#   mutate(week=epiweek(date_format)) %>% 
#   filter(week>=19) %>% 
#   mutate(population_group=factor(population_group, 
#                                  labels=c('Host', 'FDMN'))) %>% 
#   mutate(indicator=case_when(grepl('positive', name) ~ 'Case', 
#                              TRUE ~ 'Test')) %>% 
#   select(-c(name)) %>% 
#   pivot_wider(names_from=indicator) %>% 
#   group_by(population_group) %>% 
#   mutate(tests_roll=roll_mean((Test),7,  align="right", fill = 0)) %>% 
#   mutate(cases_roll=roll_mean((Case),7, align="right", fill = 0)) %>% 
#   mutate(pos_roll=cases_roll/tests_roll) %>% 
#   ggplot(., aes(x=date_format, y=pos_roll, color=population_group)) +
#   geom_line()  +
#   scale_color_manual(values=c("#ED7D31","#4472C4")) +
#   scale_x_date(date_breaks = '14 day', date_minor_breaks = '7 day',
#                date_labels = '%d-%m') +
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   labs(caption="Data source: Lab data",
#        x = "Date of test",
#        y = "Test positivity (%) (7-day average)",
#        color="") +
#   theme_minimal()

test_pos_df <- gsheet_data$ari_ili %>% 
  clean_names() %>% 
  clean_data() %>% 
  mutate(camp=gsub('camp_', '', camp_patients_residence)) %>% 
  filter(nationality %in% c('fdmn', 'host')) %>% 
  filter(sample_type!=c('follow_up', 'humanitarian_worker')) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(camp_number=str_extract(camp, regexp)) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp=reorder(camp,camp_number)) %>% 
  mutate(age=as.numeric(age)) %>% 
  mutate(age_group=cut(age,breaks = breaks, labels = labs, right = FALSE)) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  #mutate(week=date2week(date_of_case_detection,week_start = "sun", floor_day = TRUE)) %>% 
  select(nationality,week,date_of_case_detection, laboratory_result, age) %>% 
  filter(week>19) %>% 
  #mutate(week=factor(week, levels=unique(week))) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) 

test_positivity_gph <- test_pos_df %>% 
  count(nationality, week, laboratory_result) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total=negative+positive) %>% 
  select(-negative) %>% 
  drop_na() %>% 
  group_by(nationality,week) %>% 
  mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(rate) %>% 
  ungroup() %>% 
  ggplot(aes(x=week, y=estimate, color=fct_rev(nationality))) +
  geom_line() +
  #coord_flip() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position = position_dodge(width = 0.1)) +
  scale_color_manual(values=c("#ED7D31","#4472C4")) +
  theme_minimal() +
  labs(x='Week', 
       y='% COVID-19 positive samples', 
       color='',
       caption='Data source:ARI/ILI linelist') +
  scale_y_continuous(labels = scales::percent)  +
  theme(legend.position='top')
