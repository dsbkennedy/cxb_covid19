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

