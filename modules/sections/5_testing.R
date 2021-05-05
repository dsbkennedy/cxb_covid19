
# WRANGLING ---------------------------------------------------------------


ari_ili_2020 <- gsheet_data_2020$ari_ili

ari_ili_df <- gsheet_data$ari_ili %>% 
  bind_rows(ari_ili_2020) %>% 
  clean_names() %>% 
  clean_data() %>% 
  mutate(camp=gsub('camp_', '', camp_patients_residence)) %>% 
  filter(nationality=='fdmn') %>% 
  filter(sample_type!=c('follow_up', 'humanitarian_worker')) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(camp_number=str_extract(camp, regexp)) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp=reorder(camp,camp_number)) %>% 
  mutate(age=as.numeric(as.character(age))) %>% 
  mutate(age_group=cut(age,breaks = breaks, labels = labs, right = FALSE)) 


ari_ili_tests_df <- ari_ili_df %>% 
  count(date_of_case_detection,camp) %>% 
  complete(date_of_case_detection,camp, fill = list(n = 0)) %>% 
  group_by(camp) %>%
  mutate(cumulative_tests=cumsum(n)) %>%
  mutate(test_growth=ifelse(lag(cumulative_tests,7)>10, 
                            ((cumulative_tests/lag(cumulative_tests,7))^(1/7))-1,NA)) %>%
  mutate(roll_test=roll_mean((n),7,  align="right", fill = NA)) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  ungroup()

ari_ili_tests_total <- ari_ili_tests_df %>% 
  group_by(camp) %>% 
  summarise(tests_total=sum(n, na.rm=TRUE))

ari_ili_tests_7day <- ari_ili_tests_df %>% 
  filter(date_of_case_detection>=today()-7) %>% 
  group_by(camp) %>% 
  summarise(tests_7day=sum(n, na.rm=TRUE))

ari_ili_cases_7day_df <- ari_ili_df %>% 
  filter(date_of_case_detection>=today()-7) %>% 
  filter(laboratory_result=='positive') %>% 
  count(date_of_case_detection,camp) %>% 
  complete(date_of_case_detection,camp, fill = list(n = 0)) %>% 
  group_by(camp) %>%
  summarise(cases_7day=sum(n, na.rm=TRUE))


ari_ili_tests_growth <- ari_ili_tests_df %>% 
  select(camp, test_growth) %>% 
  group_by(camp) %>% 
  summarise(growth=last(test_growth))

ari_ili_tests_result <- ari_ili_df %>% 
  filter(nationality=='fdmn') %>% 
  count(camp, laboratory_result) %>% 
  group_by(camp) %>% 
  mutate(prop=n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from=laboratory_result, values_from=prop) %>% 
  select(camp, positive,negative)


# TABLE -------------------------------------------------------------------


tests_df <- tests_both %>% 
  filter(!date=='Total') %>% 
  mutate(date_upd=excel_numeric_to_date(as.numeric(date))) %>% 
  select(-c('date','outside_cx_b')) %>% 
  filter(date_upd>=ymd('2020-04-01')) %>% 
  mutate(week=yearweek(date_upd)) %>% 
  select(-date_upd) %>% 
  pivot_longer(-week) %>% 
  group_by(week,name) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  dplyr::arrange(desc(week))

fdmn_tests <- tests_df %>% 
  filter(grepl('fdmn', name)) %>% 
  mutate(name=case_when(name=='fdmn' ~ 'TESTS', 
                        TRUE ~ 'CASES')) %>% 
  pivot_wider(names_from=name, values_from=total) %>% 
  rename(WEEK=week) 

tests_table <- fdmn_tests %>% 
  mutate(WEEK=as.character(WEEK)) %>% 
  datatable(., 
            extensions = 'Buttons',
            colnames=c('Week', 'Tests', 'Cases'),
            rownames=FALSE,
            options = list(pageLength = 20,dom = 'Bfrtip', 
                          # columnDefs = list(list(className = 'dt-center', targets = 1:3)),
                           buttons = c('csv')))
# TEST-POSITIVITY TABLE ---------------------------------------------------

age_labs_decade <- c(paste(seq(0, 40, by = 10), seq(9, 49, by = 10),
                           sep = "-"), paste(50, "+", sep = ""))

# tests_age_group_df <- ari_ili_df %>%  
#   filter(!laboratory_result %in% c('n_a','not_done')) %>% 
#   filter(nationality=='fdmn') %>% 
#   mutate(age_group=cut(age,breaks = c(seq(0, 50, by = 10), Inf), labels = age_labs_decade, right = FALSE)) %>% 
#   select(age_group) %>% 
#   #mutate(age_group=fct_explicit_na(age_group_decade, na_level = "(Age missing)")) %>% 
#   count(age_group, .drop=FALSE) 

tests_agegrp_tbl <- ari_ili_df %>%
  filter(!laboratory_result %in% c('n_a','not_done')) %>%
  filter(nationality=='fdmn') %>%
  mutate(age_group=cut(age,breaks = c(seq(0, 50, by = 10), Inf), labels = age_labs_decade, right = FALSE)) %>% 
  count(age_group, laboratory_result) %>%
  filter(!is.na(age_group)) %>%
  pivot_wider(names_from=laboratory_result, values_from=n) %>%
  mutate(total=negative+positive) %>%
  select(-negative) %>%
  drop_na() %>%
  group_by(age_group) %>%
  mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(rate) %>%
  ungroup() 



tests_sex_tbl <- ari_ili_df %>%
  filter(!laboratory_result %in% c('n_a','not_done')) %>%
  filter(nationality=='fdmn') %>%
  count(sex, laboratory_result) %>%
  filter(!is.na(sex)) %>%
  mutate(sex=case_when(sex=='m' ~ 'Male', 
                       sex=='f' ~ 'Female')) %>% 
  filter(!is.na(sex)) %>% 
  pivot_wider(names_from=laboratory_result, values_from=n) %>% 
  mutate(total=negative+positive) %>%
  select(-negative) %>%
  drop_na() %>%
  group_by(sex) %>%
  mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
                       broom::tidy())) %>%
  unnest(rate) %>%
  ungroup() 


# tests_pos_tbl <- tests_agegrp_tbl %>% bind_rows(tests_sex_tbl) %>% 
#   mutate(indicator=case_when(is.na(age_group) ~ "Sex", 
#                              TRUE ~ "Age group")) %>% 
#   mutate(age_group=as.character(age_group)) %>% 
#   mutate(value=coalesce(age_group, sex)) %>% 
#   select(indicator,value, positive, total, estimate, conf.low, conf.high) %>% 
#   group_by(indicator) %>% 
#   mutate(estimate=round(estimate*100,2)) %>% 
#   mutate(error=paste0(round(conf.low*100,2),'-',round(conf.high*100,2))) %>% 
#   select(-c(conf.low, conf.high)) %>% 
#   gt() %>% 
#   opt_row_striping(., row_striping = TRUE) %>% 
#   # fmt_percent(
#   #   columns = 5:7,
#   #   decimals = 1
#   # ) %>% 
#   cols_label(
#     value = "",
#     total = "Tests",
#     positive = "Cases",
#     estimate = "Positivity (%)",
#     error = "LCI-UCI"
#   ) 

test_pos_age_sex <-  tests_agegrp_tbl %>% bind_rows(tests_sex_tbl) %>% 
  mutate(indicator=case_when(is.na(age_group) ~ "Sex", 
                             TRUE ~ "Age group")) %>% 
  mutate(age_group=as.character(age_group)) %>% 
  mutate(value=coalesce(age_group, sex)) %>% 
  select(indicator,value, positive, total, estimate, conf.low, conf.high) 

test_pos_age_sex_gph <- test_pos_age_sex %>% 
  ungroup() %>% 
  #filter(indicator=='Sex') %>% 
  ggplot(aes(x=value, y=estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position = position_dodge(width = 0.1)) +
  theme_minimal() +
  labs(x='',
       y='% COVID-19 + samples',
      # title = 'Test positivity by age group and sex',
       caption='Data source:IEDCR Field Lab') +
  scale_y_continuous(labels = scales::percent, limits=c(0,0.06))  +
  expand_limits(x = 0, y = 0)
  #coord_flip()


# TEST-POSITIVITY ---------------------------------------------------------

week_test_df <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  filter(!sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  #filter(date_of_case_detection>=ymd('2020-05-01')) %>% 
 # mutate(week=isoweek(date_of_case_detection)) %>% 
  mutate(year_week=yearweek(date_of_case_detection)) %>% 
  #mutate(week=date2week(date_of_case_detection,week_start = "sun", floor_day = TRUE)) %>% 
  select(year_week,date_of_case_detection, laboratory_result, age) %>% 
  #filter(week>19) %>% 
  #mutate(week=factor(week, levels=unique(week))) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) 

tests_gph <- ggplot(week_test_df, aes(x=year_week, y=age, group=factor(year_week))) + 
  #geom_jitter(colour="lightblue", alpha=0.5, width=0.1) +
  #geom_point(stat="summary", fun.y="mean") + 
  geom_boxplot(alpha = 0.80, outlier.shape = NA) +
  #geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Week", y="Age (median with interquartile range)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
 scale_y_continuous(breaks=seq(0,80,10))






# TEST-AGE GROUP ----------------------------------------------------------

age_labs_decade <- c(paste(seq(0, 40, by = 10), seq(9, 49, by = 10),
                           sep = "-"), paste(50, "+", sep = ""))

tests_age_group_df <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  mutate(age_group=cut(age,breaks = c(seq(0, 50, by = 10), Inf), labels = age_labs_decade, right = FALSE)) %>% 
  select(age_group) %>% 
  #mutate(age_group=fct_explicit_na(age_group_decade, na_level = "(Age missing)")) %>% 
  count(age_group, .drop=FALSE) 



tests_age_group_gph <-  
  tests_age_group_df %>% 
  left_join(age_group_pop, by=c('age_group'='age_group')) %>% 
  filter(!is.na(age_group)) %>% 
  mutate(tests_per10000=(n/population)*10000) %>% 
  #mutate(age_group=factor(age_group, levels=c('0-17', '18-59', '60 and over'))) %>% 
  ggplot(aes(x=age_group, y=tests_per10000, fill=age_group)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2", na.value="blue") +
  labs(x='Age groups', y='Tests per 10,000 people', fill='Age groups')

week_filter <- max(tests_df$week)-12

###TEST POSITIVTY GRAPH
test_positivity_df <- tests_df %>% 
  #filter(grepl('fdmn|host', name)) %>% 
  # mutate(name=case_when(name=='fdmn' ~ 'TESTS', 
  #                       TRUE ~ 'CASES')) %>% 
  pivot_wider(names_from=name, values_from=total) %>% 
  pivot_longer(-week) %>% 
  mutate(indicator=ifelse(grepl('positive',name), 'cases', 'tests')) %>% 
  mutate(population_group=ifelse(grepl('fdmn', name), 'FDMN', 'HOST')) %>% 
  select(-name) %>% 
  group_by(population_group) %>% 
  select(week,indicator,population_group,value) %>% 
  pivot_wider(names_from=indicator, values_from=value) %>% 
  #group_by(population_group) %>% 
  filter(cases>0) %>% 
  filter(week>=week_filter) %>%
  # filter(week!=('2020 W15')) %>% 
  #filter(week<max(week, na.rm=TRUE)) %>% 
  mutate(pos=map2(cases,tests, ~ prop.test(.x, .y, conf.level=0.95) %>% 
                    broom::tidy())) %>% 
  unnest(pos) 


test_positivity_df_last <- test_positivity_df %>% 
  group_by(population_group) %>% 
  filter(week==max(week))

test_positivity_gph <- test_positivity_df %>% 
  ggplot(aes(x=week, y=estimate, color=population_group)) +
  geom_line() +
  #geom_col(position='dodge') + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position = position_dodge(width = 0.6)) +
  scale_color_manual(values=c("#4472C4", "#ED7D31")) +
  scale_y_continuous(labels = scales::percent, 
                   #  sec.axis = dup_axis(
                    #   breaks = test_positivity_df_last$estimate,
                     #  labels = test_positivity_df_last$population_group,
                     #  name = NULL
                     )  +
  theme_minimal() +
  theme(legend.position = 'none') +
  labs(x='Week', 
       y='% COVID-19 positive samples', 
       color='',
       caption='Data source:IEDCR FIELD LAB')



# Age group test positivity -----------------------------------------------

ari_ili_testpos_df <- 
  ari_ili_df_testpos <- gsheet_data$ari_ili %>% 
  bind_rows(ari_ili_2020) %>% 
  clean_names() %>% 
  clean_data() %>% 
  filter(sample_type %in% c('ari_ili','suspected_covid_19')) %>% 
  filter(nationality %in% c('fdmn', 'host')) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(age=as.numeric(as.character(age))) %>% 
  filter(!is.na(age)) %>% 
  mutate(age_group=cut(age,breaks = c(seq(0, 50, by = 10), Inf), labels = age_labs, right = FALSE)) %>% 
  count(date_of_case_detection,nationality,age_group,laboratory_result) %>% 
  complete(date_of_case_detection,nationality,age_group,laboratory_result, fill = list(n = 0)) %>% 
  pivot_wider(names_from='laboratory_result', values_from='n') %>% 
  mutate(tests=negative+positive) %>% 
  filter(date_of_case_detection>today()-90) %>% 
  mutate(week=yearweek(date_of_case_detection)) %>% 
  group_by(nationality,age_group,week) %>% 
  summarise(total_tests=sum(tests,na.rm=TRUE),
            total_cases=sum(positive,na.rm=TRUE)) %>% 
  mutate(positivity=total_cases/total_tests) 


# ari_ili_testpos_df_last <- ari_ili_testpos_df %>% 
#   group_by(age_group) %>% 
#    filter(date_of_case_detection==max(date_of_case_detection))


ari_ili_testpos_gph <-  ari_ili_testpos_df %>% 
  ggplot(aes(x=week, y=positivity, colour=nationality)) +
  geom_line() +
  theme_tufte() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L),
                     #sec.axis = dup_axis(
                      # breaks = ari_ili_testpos_df_last$positivity,
                       #labels = ari_ili_testpos_df_last$nationality,
                       #name = NULL
                     ) +
  scale_color_manual(values=c("#4472C4", "#ED7D31")) +
  theme(legend.position = 'top') +
  labs(x = "",
       y = "Test positivity (7-day average)") +
 facet_wrap(~ age_group, ncol=2)


