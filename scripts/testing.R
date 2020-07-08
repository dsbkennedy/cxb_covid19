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
  geom_line()  +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  scale_x_date(date_breaks = '14 day', date_minor_breaks = '3 day',
               date_labels = '%d-%m') +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(caption="Data source: Lab data",
       x = "Date",
       y = "Test positivity (%)",
       fill="") +
  theme_minimal()




test_nationality %>% 
  filter(name %in% c('host', 'fdmn','host_positive', 'fdmn_positive')) %>% 
  mutate(indicator=case_when(grepl('positive', name) ~ 'Case', 
                             TRUE ~ 'Test')) %>% 
  select(-name) %>% 
  pivot_wider(names_from=indicator) %>% 
  group_by(population_group,date_format) %>% 
  mutate(tests_roll=roll_mean((Test),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  mutate(cases_roll=roll_mean((Case),7, na.rm=TRUE, align="right", fill = NA)) %>% 
  mutate(pos_roll=cases_roll/tests_roll) %>% 
  ggplot(., aes(x=date_format, y=pos_roll, color=population_group)) +
  geom_line()  +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  labs(caption="Data source: ARI/ILI Linelist",
       x = "Week",
       y = "Age (years)",
       fill="Population group") +
  theme_minimal()




ari_data <- gsheet_data$ari_ili %>% 
    clean_names() %>% 
    mutate(date_detect=ymd(date_of_case_detection),
           date_onset=ymd(date_of_onset_of_any_of_the_symptoms)) %>% 
    select(nationality, date_detect, date_onset,sample_type, laboratory_result, age, sex)
  
  
  
mean_age_gph <- ari_data %>% 
    filter(!nationality=='UNK') %>% 
    mutate(week=epiweek(date_detect)) %>% 
    #filter(date_detect>=ymd('2020-05-01')) %>% 
    filter(week>=19) %>% 
    group_by(week, nationality) %>% 
    #summarise(mean_age=mean(age,na.rm=TRUE)) %>% 
    #ggplot(., aes(x=week, y=mean_age, fill=nationality)) +
    #geom_col(position="dodge")
  summarise(ci = list(mean_cl_normal(age) %>% 
                        rename(mean=y, lwr=ymin, upr=ymax))) %>% 
    unnest()  %>% 
    ggplot(., aes(x=week, y=mean, fill=nationality)) +
    geom_bar(stat="identity", width=.75, position = "dodge") +
    geom_errorbar(aes(ymin=lwr, ymax=upr),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
  scale_fill_manual(values=c("#ED7D31","#4472C4")) +
  labs(caption="Data source: ARI/ILI Linelist",
       x = "Week",
       y = "Age (years)",
       fill="Population group") +
  theme_minimal()
    
  
 test <-  ari_data %>% 
    mutate(onset_test = date_detect-date_onset) 
  
    ggplot(., aes(x=onset_test, color=nationality)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  
  
  
  ggplot(df, aes(x=weight, color=sex)) +
    geom_histogram(fill="white", alpha=0.5, position="identity")
  
    
  
  

