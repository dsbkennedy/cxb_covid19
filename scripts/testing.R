#Testing data


##Total tests per week

tests_week <- test_nationality %>% 
  filter(name %in% c('host', 'fdmn')) %>% 
  mutate(week=epiweek(date_format)) %>% 
  group_by(week, name) %>% 
  summarise(total_tests=sum(value)) %>% 
  ggplot(. ,aes(week,total_tests)) +
  geom_col() +
  facet_wrap(~name, scales="free", nrow=1) +
 # coord_flip() +
  labs(caption="Data source: Lab data",
       x = "Week",
       y = "Total tests (n)",
       fill="") +
  theme_minimal()



##Average positive per week

cases_week <- test_nationality %>% 
  filter(name %in% c('host_positive', 'fdmn_positive')) %>% 
  mutate(week=epiweek(date_format)) %>% 
  group_by(week, name) %>% 
  summarise(total_cases=sum(value)) %>% 
  ggplot(. ,aes(week,total_cases)) +
  geom_col() +
  facet_wrap(~name, scales="free", nrow=1) +
  #coord_flip() +
  labs(caption="Data source: Lab data",
       x = "Week",
       y = "Total cases (n)",
       fill="") +
  theme_minimal()


##Test postiivity
test <- test_nationality %>% 
  filter(name %in% c('host', 'fdmn')) %>% 
  mutate(week=epiweek(date_format)) %>% 
  group_by(week, name) %>% 
  summarise(total_tests=sum(value)) 

case <- test_nationality %>% 
          filter(name %in% c('host_positive', 'fdmn_positive')) %>% 
          mutate(week=epiweek(date_format)) %>% 
          group_by(week, name) %>% 
          summarise(total_cases=sum(value)) %>% 
        mutate(name=case_when(grepl('host', name) ~ 'host',
                        TRUE ~ 'fdmn')) %>% 
  mutate(name_fac=factor(name, levels=c('host', 'fdmn'),
                         labels=c('host', 'fdmn')))

test_positivity_gph <- test %>% 
  left_join(case, by=c('week', 'name')) %>% 
  mutate(pos=total_cases/total_tests) %>% 
  ggplot(., aes(x=week, y=pos, fill=name_fac)) +
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  labs(caption="Data source: Lab data",
       x = "Week",
       y = "Test positivity (%)",
       fill="") +
  theme_minimal()

ggarrange(tests_week, cases_week, test_positivity_gph, legend="top")

library(RcppRoll)
test_nationality %>% 
  filter(name %in% c('host', 'fdmn')) %>% 
  group_by(name) %>% 
  mutate(tests_roll=roll_mean((value),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=date_format, y=tests_roll, fill=population_group)) +
  geom_col()

test_nationality %>% 
  filter(name %in% c('host_positive', 'fdmn_positive')) %>% 
  group_by(name) %>% 
  mutate(cases_roll=roll_mean((value),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=date_format, y=cases_roll, color=population_group)) +
  geom_line()

test_nationality %>% 
  filter(name %in% c('host', 'fdmn','host_positive', 'fdmn_positive')) %>% 
  mutate(indicator=case_when(grepl('positive', name) ~ 'Case', 
                             TRUE ~ 'Test')) %>% 
  select(-name) %>% 
  pivot_wider(names_from=indicator) %>% 
  group_by(population_group) %>% 
  mutate(tests_roll=roll_mean((Test),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  mutate(cases_roll=roll_mean((Case),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  mutate(pos_roll=cases_roll/tests_roll) %>% 
  ggplot(., aes(x=date_format, y=pos_roll, color=population_group)) +
  geom_line()  

  group_by(name) %>% 
  mutate(cases_roll=roll_mean((value),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=date_format, y=cases_roll, color=population_group)) +
  geom_line()  


  #mutate(week=epiweek(date_format)) %>% 
  #group_by(week, name) %>% 
  #summarise(total_tests=sum(value)) 


  dplyr::mutate(death_03da = zoo::rollmean(deaths, k = 3, fill = NA),
                death_05da = zoo::rollmean(deaths, k = 5, fill = NA),
                death_07da = zoo::rollmean(deaths, k = 7, fill = NA),
                death_15da = zoo::rollmean(deaths, k = 15, fill = NA),
                death_21da = zoo::rollmean(deaths, k = 21, fill = NA)) %>% 
  dplyr::ungroup()

