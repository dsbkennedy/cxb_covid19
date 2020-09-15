
# WRANGLING ---------------------------------------------------------------

ewars_mort <- read.csv(here('data', 'community_mortality.csv')) %>% clean_names() %>% 
  mutate(year=as.numeric(isoyear)) %>% 
  mutate(week=as.numeric(isoweek)) %>% 
  mutate(date=make_datetime(year=year) + weeks(week)) %>% 
  filter(date>=ymd('2019-07-01')) %>% 
  mutate(year_wk=yearweek(date)) %>% 
  mutate(year_wk=year_wk-1) %>% 
  arrange(year_wk)  %>% 
  filter(year_wk<max(year_wk)) %>% 
  mutate(age_group=case_when(age_in_years<5 ~ 'Under 5',
                             age_in_years>=5 & age_in_years<18 ~ '5 to 18',
                             age_in_years>=18 & age_in_years<60 ~ '18 to 59',
                             age_in_years>=60 ~'60 and over')) %>% 
  mutate(age_group=factor(age_group, levels=c('Under 5', '5 to 18', '18 to 59',  '60 and over', NA)))


# WEEKLY-DEATHS -----------------------------------------------------------

ewars_mort_gph <- ewars_mort %>% 
  filter(!camp_zone=='') %>% 
  count(isoyear, isoweek, year_wk) %>%
  arrange(year_wk) %>% 
  mutate(deaths_roll=roll_mean((n),4, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=isoweek, y=deaths_roll, color=factor(isoyear))) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,40)) +
  labs(x='Week', y='Deaths (4-week average)', color='Year')


# WEEKLY-DEATHS-AGEGRP ----------------------------------------------------

death_age_gph <- ewars_mort %>%  
  count(age_group,year_wk) %>% 
  filter(!is.na(age_group)) %>% 
  arrange(year_wk)  %>% 
  group_by(age_group) %>% 
  mutate(deaths_roll=roll_mean((n),4, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(aes(x=year_wk, y=deaths_roll, color=fct_rev(age_group))) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,40)) +
  labs(x='Week', y='Deaths (4-week average)', color='Age group') +
  scale_colour_brewer(palette = "Set1")


# WEEKLY-DEATHS-CAUSE -----------------------------------------------------

cause_death_gph <- ewars_mort %>%  
  count(probable_cause_of_death,year_wk) %>% 
  arrange(year_wk)  %>% 
  group_by(probable_cause_of_death) %>% 
  mutate(deaths_roll=roll_mean((n),4, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(aes(x=year_wk, y=deaths_roll, color=fct_rev(probable_cause_of_death))) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits=c(0,40)) +
  labs(x='Week', y='Deaths (4-week average)', color='Probable cause of death') +
  scale_colour_brewer(palette = "Set2")


# WEEKLY-DEATHS-CAMP ------------------------------------------------------

ewars_mort_camp_gph <-  ewars_mort %>% 
  filter(isoyear>2018) %>% 
  filter(!(isoyear==2019 & isoweek<30)) %>% 
  filter(!camp_zone=='') %>% 
  count(isoyear, year_wk, camp_zone) %>% 
  mutate(camp_number=str_extract(camp_zone, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  #arrange(camp_number) %>% 
  mutate(camp_zone=reorder(camp_zone, camp_number)) %>% 
  #arrange(year_wk)  %>% 
  group_by(camp_zone) %>% 
  mutate(deaths_roll=roll_mean((n),4, na.rm=TRUE, align="right", fill = NA)) %>% 
  ggplot(., aes(x=year_wk, y=deaths_roll, color=camp_zone)) +
  geom_line() +
  scale_x_yearweek(date_breaks = '120 days') +
  scale_y_continuous(breaks=seq(0,10,2)) +
  facet_wrap(~camp_zone) +
  theme_minimal() +
  theme(legend.position="none")+
  labs(x='Week', y='Deaths (4-week average)')
