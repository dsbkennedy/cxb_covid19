#Cases
cxb_cases_subloc <- all_cases_linelist %>% 
  group_by(population_group,date_of_case_detection,upazilla, camp_of_residence) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  select(date=date_of_case_detection, population_group,upazilla, camp_of_residence, new_cases=n)

#Deaths
cxb_deaths_subloc <- all_cases_linelist %>% 
  filter(x30_day_outcome=='Death') %>% 
  group_by(population_group,date_of_case_detection,upazilla, camp_of_residence) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  select(date=date_of_case_detection, population_group,upazilla, camp_of_residence, new_deaths=n)

cxb_cases_deaths_subloc <- cxb_cases_subloc %>% 
  full_join(cxb_deaths_subloc, by=c('population_group', 'date', 'upazilla', 'camp_of_residence')) %>% 
  mutate(location=coalesce(as.character(camp_of_residence),upazilla)) %>% 
  full_join(., population, by='location') %>% 
  filter(!is.na(population_group))

rm(cxb_cases_subloc, cxb_deaths_subloc)

#Totals
table_totals_subloc <- cxb_cases_deaths_subloc %>% 
  ungroup() %>% 
  group_by(population_group, population, location) %>% 
  summarise(total_cases=sum(new_cases, na.rm=TRUE),
            total_deaths=sum(new_deaths, na.rm=TRUE)) %>% 
  mutate(total_cases_pm=(total_cases/population)*1*10E5,
         total_deaths_pm=(total_deaths/population)*1*10E5) %>% 
  ungroup() %>% 
  select(-population)

#7-day
table_7day_subloc <- cxb_cases_deaths_subloc %>% 
  ungroup() %>% 
  filter(date > today() -7) %>% 
  group_by(population_group, population, location) %>% 
  summarise(total_cases_7day=sum(new_cases, na.rm=TRUE),
            total_deaths_7day=sum(new_deaths, na.rm=TRUE)) %>% 
  mutate(total_cases_pm_7day=(total_cases_7day/population)*1*10E5,
         total_deaths_pm_7day=(total_deaths_7day/population)*1*10E5)%>% 
  ungroup() %>% 
  select(-population)


#Growth rate
##Growth rate needs values larger than 10 so FDMN communities don't reach this criteria
growth_rate_subloc <- cxb_cases_deaths_subloc %>% 
  group_by(population_group,location) %>% 
  mutate(new_cases=coalesce(new_cases,0)) %>% 
  mutate(cumulative_cases=cumsum(new_cases)) %>%
  mutate(case_growth=ifelse(lag(cumulative_cases,7)>10, 
                            ((cumulative_cases/lag(cumulative_cases,7))^(1/7))-1,NA)) %>%
  summarise(case_growth=last(case_growth)) 


#Final calculations
table_calc_comb_subloc <- table_totals_subloc %>% 
  left_join(table_7day_subloc, by=c('population_group','location')) %>% 
  left_join(growth_rate_subloc, by=c('population_group','location')) %>% 
  mutate( cfr=total_deaths/total_cases)

rm(table_totals_subloc, table_7day_subloc,growth_rate_subloc)
