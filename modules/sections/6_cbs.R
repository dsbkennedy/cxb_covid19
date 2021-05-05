# WRANGLING ---------------------------------------------------------------
today_date <- ymd(today())

hbc_df <- read.csv(here('data', 'weekly', 'input', 'home_based_care.csv')) %>% 
  clean_names() %>% 
  mutate(max_week=max(week),
         camp_number=str_extract(camp, "[[:digit:]]+"),
         camp_number=as.numeric(camp_number),
         camp=reorder(camp,camp_number),
         year=as.numeric(reporting_year),
         week=as.numeric(week),
         date=make_datetime(year=year) + weeks(week),
         year_wk=yearweek(date),
         year_wk=case_when(year!=2021 ~year_wk-1, 
                           TRUE ~ year_wk)) %>% 
 filter(date>=ymd('2020-06-24') & year_wk<yearweek(today_date)-1)
  
# HOUSEHOLDS-VISITED ------------------------------------------------------

hhvisit_gph <- hbc_df %>% 
  select(year,week,year_wk,date, c1_total_hh_visited_week) %>% 
  group_by(year,week,year_wk,date) %>% 
  summarise(hhvisited=sum(c1_total_hh_visited_week, na.rm=TRUE)) %>% 
  ggplot(., aes(x=year_wk, y=hhvisited)) +
  geom_line() +
  labs(x='', y='Households visited') +
  theme_minimal() 

# HOUSEHOLDS-VISITED-CAMP -------------------------------------------------

population <- read.csv(here('data', 'reference', 'block_level_population.csv'), skip=1) %>% 
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
  mutate(camp=gsub('camp_', '', camp),
         camp_number=str_extract(camp, "[[:digit:]]+"),
         camp_number=as.numeric(camp_number),
         camp=reorder(camp, camp_number)) %>% 
  clean_data() %>% 
  #Clean data
  mutate(camp=as.character(camp),
         camp=ifelse(camp=='4ext', '4_ext', 
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
  mutate(prop_visited=hhvisited/total_families,
         prop_mild=mild_ind/total_individuals,
         camp=reorder(camp, camp_number)) %>% 
  filter(!is.na(prop_visited)) 


camp_activity_df <- camp_activity %>% select(camp, week, prop_visited,prop_mild)

camp_activity_average <- camp_activity_df %>% 
  filter(week>=(max(week)-8)& prop_visited<=1.2) %>% 
  group_by(camp) %>% 
  mutate(average_visited=mean(prop_visited),
         average_mild=mean(prop_mild)) %>% 
  select(camp, average_visited,average_mild) %>% distinct()

camp_activity_lastweek <- camp_activity_df %>% 
  filter(week>=(max(week, na.rm=TRUE))) %>% 
  select(camp, last_week_visited=prop_visited,last_week_mild=prop_mild)

prop_visit_camp_table <- camp_activity_average %>% 
  left_join(camp_activity_lastweek, by='camp') %>% 
  select(camp, last_week_visited ,average_visited, last_week_mild,average_mild) %>% 
  ungroup() %>% 
  gt() %>% 
  opt_row_striping(., row_striping = TRUE) %>% 
  tab_spanner(
    label = "Households visited (%)",
    columns = vars(last_week_visited,average_visited)
  ) %>% 
  tab_spanner(
    label = "Individuals with mild symptoms (%)",
    columns = vars(last_week_mild,average_mild)
  ) %>% 
  fmt_percent(
    columns = vars(last_week_visited,average_visited,
                   last_week_mild,average_mild),
    decimals = 1
  ) %>% 
  cols_label(
    camp = "Camp",
    last_week_visited = "Last week",
    average_visited = "4-week average",
    last_week_mild = "Last week",
    average_mild = "4-week average") %>% 
  cols_align(align = "center",
   columns = gt::everything()
      ) %>% 
     tab_options(container.overflow.y = TRUE)

hh_visit_lastweek <- camp_activity %>% select(camp, week, prop_visited,prop_mild) %>% 
  filter(week>=(max(week, na.rm=TRUE))) %>% 
  ggplot(aes(x=  forcats::fct_rev(camp), y=prop_visited, group=1)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  labs(x='Camp', y='Households visited last week')


# SYMPTOM WRANGLING -------------------------------------------------------

symptoms_df <- hbc_df %>%
  select(date, contains('sympt')) %>% 
  group_by(date) %>% 
  summarise_all(list(sum)) %>% 
  pivot_longer(-c(date)) %>%
  filter(!grepl('total', name)) %>%
  mutate(symp_level=case_when(grepl('red', name) ~ 'red',
                              grepl('yellow', name) ~ 'yellow'),
         sex= case_when(grepl('_m_', name) ~ 'male',
                        grepl('_f_', name) ~ 'female'),
         age_grp=case_when(grepl('5_59', name) ~ '5 to 59',
                           grepl('_60', name) ~ '60 & over',
                           grepl('less_5', name) ~ 'Less than 5')) %>%
  select(-name) %>% 
  mutate(severity=case_when(symp_level=='yellow' ~ 'Mild',
                            symp_level=='red' ~ 'Moderate/Severe'),
         age_group=case_when(age_grp=='5 to 59' ~ '5-59',
                             age_grp=='Less than 5' ~ '<5',
                             age_grp=='60 & over' ~ '>=60'))

# MILD-SYMPTOMS -----------------------------------------------------------
mild_symptoms_gph <- symptoms_df %>%
  ungroup() %>%
  group_by(severity,date, sex, age_group) %>%
  summarise(total_cases=sum(value,na.rm=TRUE)) %>%
  group_by(severity,date) %>%
  filter(date<=today()) %>% 
  mutate(age_group=factor(age_group, levels=c('<5', '5-59', '>=60')),
         age_group=fct_rev(age_group)) %>%
  filter(severity=='Mild') %>%
  ggplot(aes(x=date,y=total_cases, color=interaction(sex,age_group), label=total_cases)) +
  geom_line() +
  theme_minimal() +
  labs(x='Week', y='Number of cases', color='Sex & \n Age group') +
  scale_fill_brewer(palette="Dark2") 

# MODERATE-SEVERE-TABLE ---------------------------------------------------
modsev_symptoms_gph <- symptoms_df %>%
  ungroup() %>%
  filter(date<=today()) %>% 
  group_by(severity,date, sex, age_group) %>%
  summarise(total_cases=sum(value,na.rm=TRUE)) %>%
  group_by(severity,date) %>%
  mutate(age_group=factor(age_group, levels=c('<5', '5-59', '>=60')),
         age_group=fct_rev(age_group)) %>%
  filter(!severity=='Mild') %>%
  ggplot(aes(x=date,y=total_cases, fill=interaction(sex,age_group), label=ifelse(total_cases>0,total_cases,NA))) +
  geom_col(size=2.5) +
  theme_minimal() +
  labs(x='Week', y='Number of cases', fill='Sex & \n Age group') +
  scale_fill_brewer(palette="Dark2") 

