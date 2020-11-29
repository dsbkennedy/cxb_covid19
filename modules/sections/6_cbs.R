
# WRANGLING ---------------------------------------------------------------

hbc_df <- read.csv(here('data', 'home_based_care.csv')) %>% 
  clean_names() %>% 
  mutate(max_week=max(week)) %>% 
  mutate(camp_number=str_extract(camp, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp=reorder(camp,camp_number)) %>% 
  filter(week>25)  


# HOUSEHOLDS-VISITED ------------------------------------------------------

hhvisit_gph <- hbc_df %>% 
  select(week, c1_total_hh_visited_week) %>% 
  filter(week>25) %>% 
  group_by(week) %>% 
  summarise(hhvisited=sum(c1_total_hh_visited_week, na.rm=TRUE)) %>% 
  ggplot(., aes(x=week, y=hhvisited)) +
  geom_col() +
  labs(title='Households visited') +
  scale_x_continuous(breaks=pretty_breaks()) +
  #labs(x='', y='Households visited') +
  theme_minimal() 


# HOUSEHOLDS-VISITED-CAMP -------------------------------------------------

population <- read.csv(here('data', 'block_level_population.csv'), skip=1) %>% 
  clean_names() %>% select(camp, total_families,total_individuals) %>%
  filter(camp!="") %>% 
  filter(grepl('Total',camp)) %>% 
  mutate(camp_number=str_extract(camp,"[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  #Dropped this and summed in next step to confirm it matches
  filter(!camp=='Grand Total') %>% 
  mutate(camp_link=gsub('Camp|Total', '', camp)) %>% 
  mutate(camp_link=trimws(camp_link)) %>% 
  #mutate(camp_link=case_when(camp_link=='4 Ext' ~ '4Ext')) %>% 
  clean_data() %>% 
  select(-camp) %>% 
  rename(camp=camp_link)


camp_activity <- hbc_df %>% 
  select(week, camp, c1_total_hh_visited_week, total_yellow_symptoms) %>% 
  ungroup() %>% 
  filter(week>25) %>% 
  mutate(camp=gsub('camp_', '', camp)) %>% 
  mutate(camp_number=str_extract(camp, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp=reorder(camp, camp_number)) %>% 
  clean_data() %>% 
  mutate(camp=as.character(camp)) %>% 
  #Clean data
  mutate(camp=ifelse(camp=='4ext', '4_ext', 
                     ifelse(camp=='ktp', 'kutupalong_rc',
                            ifelse(camp=='nyp', 'nayapara_rc',
                                   camp)))) %>% 
  group_by(week,camp) %>% 
  summarise(hhvisited=sum(c1_total_hh_visited_week, na.rm=TRUE),
            mild_ind=sum(total_yellow_symptoms, na.rm=TRUE)) %>% 
  ungroup() %>% 
  left_join(population, by=c('camp'='camp')) %>% 
  arrange(camp_number) %>% 
  select(week, camp, hhvisited,mild_ind, total_families,total_individuals, camp_number) %>% 
  mutate(prop_visited=hhvisited/total_families) %>% 
  mutate(prop_mild=mild_ind/total_individuals) %>% 
  mutate(camp=reorder(camp, camp_number)) %>% 
  filter(!is.na(prop_visited)) 



# prop_visit_camp_gph <-  ggplot(camp_activity, aes(x=week, y=prop_visited)) +
#   geom_col() +
#   scale_y_continuous(labels = scales::percent) +
#   facet_wrap(~camp) +
#   theme(legend.position = "none") +
#   theme_minimal() +
#   scale_x_continuous(breaks=pretty_breaks()) +
#   labs(y='Households visited (%)', x='Week')

prop_visit_camp_gph <- camp_activity %>% select(camp, week, prop_visited) %>% 
  filter(prop_visited<1.2) %>% 
  ggplot(.,aes(x=(camp),y=prop_visited)) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(y='Households visited (%)', x='Camp') 
  #coord_flip() 

# MILD-SYMPTOMS -----------------------------------------------------------

symptoms_df <- hbc_df %>%
  filter(week>25) %>% 
  select(reporting_year, week, contains('sympt')) %>% 
  group_by(reporting_year, week) %>% 
  summarise_all(list(sum)) %>% 
  pivot_longer(-c(reporting_year, week)) %>%
  filter(!grepl('total', name)) %>%
  mutate(symp_level=case_when(grepl('red', name) ~ 'red',
                              grepl('yellow', name) ~ 'yellow')) %>%
  mutate(sex= case_when(grepl('_m_', name) ~ 'male',
                        grepl('_f_', name) ~ 'female')) %>%
  mutate(age_grp=case_when(grepl('5_59', name) ~ '5 to 59',
                           grepl('_60', name) ~ '60 & over',
                           grepl('less_5', name) ~ 'Less than 5')) %>%
  select(-name) %>% 
  mutate(severity=case_when(symp_level=='yellow' ~ 'Mild',
                            symp_level=='red' ~ 'Moderate/Severe')) %>% 
  mutate(age_group=case_when(age_grp=='5 to 59' ~ '5-59',
                             age_grp=='Less than 5' ~ '<5',
                             age_grp=='60 & over' ~ '>=60'))

mild_symptoms_gph <- symptoms_df %>%
  ungroup() %>%
  group_by(severity,week, sex, age_group) %>%
  summarise(total_cases=sum(value,na.rm=TRUE)) %>%
  group_by(severity,week) %>%
  #mutate(prop=total_cases/sum(total_cases)) %>%
  mutate(age_group=factor(age_group, levels=c('<5', '5-59', '>=60'))) %>%
  mutate(age_group=fct_rev(age_group)) %>%
  filter(severity=='Mild') %>%
  ggplot(aes(x=week,y=total_cases, fill=interaction(sex,age_group), label=total_cases)) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = .5)) +
  theme_minimal() +
  labs(x='Week', y='Number of cases', fill='Sex & \n Age group') +
  scale_fill_brewer(palette="Dark2") +
  #scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=pretty_breaks())


# MODERATE-SEVERE-TABLE ---------------------------------------------------

# modsev_symptoms_tbl <- symptoms_df %>% 
#   group_by(week, severity, age_grp, sex) %>% 
#   summarise(total=sum(value,na.rm=TRUE)) %>% 
#   pivot_wider(names_from=c(age_grp,sex), values_from=total) %>% 
#   arrange(desc(week)) %>% 
#   ungroup() %>% 
#   filter(!severity=='Mild') %>% 
#   select(-severity) %>% 
#   clean_names() %>% 
#   select(week, contains('less_than_5'), contains('5_to_59'), contains('60_over')) %>% 
#   rowwise %>% 
#   mutate(total = sum(c_across(!week))) %>% 
#   gt(rowname_col = c("week")) %>% 
#   opt_row_striping(., row_striping = TRUE) %>% 
#   tab_stubhead('Week') %>% 
#   tab_spanner(
#     label = "Under 5",
#     columns = vars(less_than_5_male,less_than_5_female)) %>% 
#   tab_spanner(
#     label = "5 to 59",
#     columns = vars(x5_to_59_male,x5_to_59_female)) %>% 
#   tab_spanner(
#     label = "60 and over",
#     columns = vars(x60_over_male,x60_over_female)) %>% 
#   cols_label(
#     week = "Week",
#     less_than_5_male = "Male",
#     less_than_5_female = "Female",
#     x5_to_59_male = "Male",
#     x5_to_59_female = "Female",
#     x60_over_male = "Male",
#     x60_over_female = "Female", 
#     total="Total")  


modsev_symptoms_gph <- symptoms_df %>%
  ungroup() %>%
  group_by(severity,week, sex, age_group) %>%
  summarise(total_cases=sum(value,na.rm=TRUE)) %>%
  group_by(severity,week) %>%
  #mutate(prop=total_cases/sum(total_cases)) %>%
  mutate(age_group=factor(age_group, levels=c('<5', '5-59', '>=60'))) %>%
  mutate(age_group=fct_rev(age_group)) %>%
  filter(!severity=='Mild') %>%
  ggplot(aes(x=week,y=total_cases, fill=interaction(sex,age_group), label=ifelse(total_cases>0,total_cases,NA))) +
  geom_col(position = position_stack(), color = "black") +
  geom_text(position = position_stack(vjust = .5)) +
  theme_minimal() +
  labs(x='Week', y='Number of cases', fill='Sex & \n Age group') +
  scale_fill_brewer(palette="Dark2") +
  #scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=pretty_breaks())
