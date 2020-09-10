
## ---- wrangling

#Cases
cxb_cases <- all_cases_linelist %>% 
  group_by(population_group,date_of_case_detection) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  select(date=date_of_case_detection, population_group, new_cases=n)

#Deaths
cxb_deaths <- all_cases_linelist %>% 
  filter(x30_day_outcome=='Death') %>% 
  group_by(population_group,date_of_case_detection) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  select(date=date_of_case_detection, population_group, new_deaths=n)

#Tests
cxb_tests <- tests_data %>%  
  filter(!date %in% c('NULL', 'Total')) %>% 
  mutate(date_format=excel_numeric_to_date(as.numeric(date)))  %>% 
  select(date=date_format, fdmn, host,) %>% 
  pivot_longer(-date) %>% 
  select(date, population_group=name, new_tests=value) %>% 
  mutate(population_group=case_when(grepl('fdmn', population_group) ~ 'Rohingya refugee/FDMN',
                                    TRUE ~ 'Host community')) 

#Combine CXB data

cxb_table_data <- cxb_tests %>% 
  full_join(cxb_cases, by=c('population_group', 'date')) %>% 
  full_join(cxb_deaths, by=c('population_group', 'date')) %>% 
  mutate(population=case_when(population_group=='Rohingya refugee/FDMN' ~ fdmn_population, 
                              TRUE ~ host_population)) 
rm(cxb_cases, cxb_deaths,cxb_tests)

#Bangladesh data from Our World In Data
bgd_data <- fread('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>% 
  filter(iso_code=='BGD') %>%        
  mutate(date_format=ymd(date)) %>% 
  select(date=date_format, new_tests,new_cases,new_deaths,population) %>% 
  mutate(population_group='Bangladesh')


#Get first date from Bangladesh data
first_date <- min(bgd_data$date)
#Last date is today
last_date <- today()



#Combine Bangaladesh and CXB data
table_final_df <- bgd_data %>% 
  bind_rows(cxb_table_data) %>% 
  arrange(date, population_group) %>% 
  ungroup()

rm(bgd_data, cxb_table_data)
#Totals
table_totals <- table_final_df %>% 
  # ungroup() %>% 
  group_by(population_group, population) %>% 
  summarise(total_tests=sum(new_tests, na.rm=TRUE),
            total_cases=sum(new_cases, na.rm=TRUE),
            total_deaths=sum(new_deaths, na.rm=TRUE)) %>% 
  mutate(total_tests_pm=(total_tests/population)*1*10E5,
         total_cases_pm=(total_cases/population)*1*10E5,
         total_deaths_pm=(total_deaths/population)*1*10E5) %>% 
  select(-population)

#7-day
table_7day <- table_final_df %>% 
  # ungroup() %>% 
  filter(date > today() -7) %>% 
  group_by(population_group, population) %>% 
  summarise(total_tests_7day=sum(new_tests, na.rm=TRUE),
            total_cases_7day=sum(new_cases, na.rm=TRUE),
            total_deaths_7day=sum(new_deaths, na.rm=TRUE)) %>% 
  mutate(total_tests_pm_7day=(total_tests_7day/population)*1*10E5,
         total_cases_pm_7day=(total_cases_7day/population)*1*10E5,
         total_deaths_pm_7day=(total_deaths_7day/population)*1*10E5)%>% 
  select(-population)

#Growth rate

growth_rate <- table_final_df %>% 
  group_by(population_group) %>% 
  mutate(new_cases=coalesce(new_cases,0)) %>% 
  mutate(cumulative_cases=cumsum(new_cases)) %>%
  mutate(case_growth=ifelse(lag(cumulative_cases,7)>10, 
                            ((cumulative_cases/lag(cumulative_cases,7))^(1/7))-1,NA)) %>%
  summarise(case_growth=last(case_growth))

#Final calculations
table_calc_comb <- table_totals %>% 
  left_join(table_7day, by='population_group') %>% 
  left_join(growth_rate, by='population_group') %>% 
  mutate(test_pos=total_cases/total_tests, 
         cfr=total_deaths/total_cases)

rm(table_totals,table_7day,growth_rate)