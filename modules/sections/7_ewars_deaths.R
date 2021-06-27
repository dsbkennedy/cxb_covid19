
# WRANGLING ---------------------------------------------------------------

ewars_mort_2020 <- read.csv(here('data', 'weekly', 'input', 'mortality_2020.csv')) %>% 
  clean_names() %>% 
  select(-age_in_days)

ewars_mort_2021 <- read.csv(here('data', 'weekly', 'input', 'mortality_2021.csv')) %>% 
  clean_names()


ewars_mort <- ewars_mort_2020 %>% 
  bind_rows(ewars_mort_2021) %>% 
  mutate(year=as.numeric(isoyear),
         week=as.numeric(isoweek),
         date=make_datetime(year=year) + weeks(week),
         year_wk=yearweek(date),
         year_wk=case_when(year!=2021 ~year_wk-1, 
                           TRUE ~ year_wk)) %>% 
  arrange(year_wk)  %>% 
  filter(year_wk<max(year_wk)) %>% 
  mutate(age_group=case_when(age_in_years<5 ~ 'Under 5',
                             age_in_years>=5 & age_in_years<60 ~ '5 to 59',
                             age_in_years>=60 ~'60 and over', 
                             TRUE ~ 'Age missing')) %>% 
  mutate(age_group=factor(age_group, levels=c('Under 5', '5 to 59',  '60 and over', 'Age missing')))

saveRDS(ewars_mort, here('data', 'weekly', 'output', 'ewars_deaths.Rds'))

# WEEKLY-DEATHS -----------------------------------------------------------
ewars_mort_gph <- ewars_mort %>% 
  count(year, week) %>% 
  filter(!year==2018) %>% 
  filter(!(year==2019 & (week<25))) %>% 
  arrange(year, week) %>% 
  group_by(year) %>% 
  mutate(deaths_roll=roll_mean((n),2, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=week, y=deaths_roll, color=factor(year))) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,75)) +
  labs(x='Week', y='Deaths (2-week average)', color='Year')


# WEEKLY-DEATHS-AGEGRP ----------------------------------------------------
death_age_gph <- ewars_mort %>%  
  count(age_group,year_wk) %>% 
  filter(!is.na(age_group)) %>% 
  arrange(year_wk)  %>% 
  group_by(age_group) %>% 
  mutate(deaths_roll=roll_mean((n),2, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(aes(x=year_wk, y=deaths_roll, color=(age_group))) +
  #geom_col() +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,60)) +
  labs(x='Week', y='Deaths (2-week average)', color='Age group') +
  scale_colour_brewer(palette = "Set1")


# WEEKLY-DEATHS-CAUSE -----------------------------------------------------
cause_death_gph <- ewars_mort %>%  
  count(probable_cause_of_death,year_wk) %>% 
  arrange(year_wk)  %>% 
  group_by(probable_cause_of_death) %>% 
  mutate(deaths_roll=roll_mean((n),2, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(aes(x=year_wk, y=deaths_roll, color=fct_rev(probable_cause_of_death))) +
  geom_line() +
  #geom_col() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,60)) +
  labs(x='Week', y='Deaths (2-week average)', color='Probable cause of death') +
  scale_colour_brewer(palette = "Set2")


# WEEKLY-DEATHS-CAMP ------------------------------------------------------
ewars_mort_camp_gph <-  ewars_mort %>% 
  filter(isoyear>2018) %>% 
  filter(!(isoyear==2019 & isoweek<30)) %>% 
  filter(!camp_zone=='') %>% 
  count(isoyear, year_wk, camp_zone) %>% 
  mutate(camp_number=str_extract(camp_zone, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp_zone=reorder(camp_zone, camp_number)) %>% 
  group_by(camp_zone) %>% 
  mutate(deaths_roll=roll_mean((n),2, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=year_wk, y=deaths_roll, color=camp_zone)) +
  geom_line() +
  scale_x_yearweek(date_breaks = '120 days') +
  scale_y_continuous(breaks=seq(0,10,2)) +
  facet_wrap(~camp_zone) +
  theme_minimal() +
  theme(legend.position="none")+
  labs(x='Week', y='Deaths (2-week average)')
