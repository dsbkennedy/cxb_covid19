
# WRANGLING ---------------------------------------------------------------

ari_ili_df <- gsheet_data$ari_ili %>% 
  clean_names() %>% 
  clean_data() %>% 
  mutate(camp=gsub('camp_', '', camp_patients_residence)) %>% 
  filter(nationality=='fdmn') %>% 
  filter(sample_type!=c('follow_up', 'humanitarian_worker')) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) %>% 
  mutate(camp_number=str_extract(camp, regexp)) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  mutate(camp=reorder(camp,camp_number)) %>% 
  mutate(age=as.numeric(age)) %>% 
  mutate(age_group=cut(age,breaks = breaks, labels = labs, right = FALSE)) 


ari_ili_tests_df <- ari_ili_df %>% 
  count(date_of_case_detection,camp) %>% 
  complete(date_of_case_detection,camp, fill = list(n = 0)) %>% 
  group_by(camp) %>%
  mutate(cumulative_tests=cumsum(n)) %>%
  mutate(test_growth=ifelse(lag(cumulative_tests,7)>10, 
                            ((cumulative_tests/lag(cumulative_tests,7))^(1/7))-1,NA)) %>%
  mutate(roll_test=roll_mean((n),7,  align="right", fill = NA)) %>% 
  mutate(week=epiweek(date_of_case_detection)) %>% 
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

  # mutate(cumulative_cases=cumsum(n)) %>%
  # mutate(case_growth=ifelse(lag(cumulative_cases,7)>10, 
  #                           ((cumulative_cases/lag(cumulative_cases,7))^(1/7))-1,NA)) %>%
  # mutate(roll_cases=roll_mean((n),7,  align="right", fill = NA)) %>% 
  # mutate(week=epiweek(date_of_case_detection)) %>% 
  # ungroup()

# ari_ili_cases_7day <- ari_ili_tests_df %>% 
#   filter(date_of_case_detection>=today()-7) %>% 
#   filter(laboratory_result=='positive') %>% 
#   group_by(camp) %>% 
#   summarise(cases_7day=sum(n, na.rm=TRUE))

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

# tests_table <- ari_ili_tests_total %>% 
#   left_join(ari_ili_tests_7day, by='camp') %>% 
#   left_join(ari_ili_tests_growth, by='camp') %>% 
#   left_join(ari_ili_tests_result, by='camp') %>% 
#   mutate(camp=trimws(camp)) %>% 
#   mutate(camp_number=str_extract(camp, regexp)) %>% 
#   mutate(camp_number=as.numeric(camp_number)) %>% 
#   arrange(camp_number) %>% 
#   select(-camp_number) %>% 
#   select(-c(growth,negative)) %>% 
#   mutate(camp=sub("^0+", "", camp)) %>% 
#   gt() %>% 
#   opt_row_striping(., row_striping = TRUE) %>% 
#   fmt_percent(
#     columns = 4:4,
#     decimals = 1
#   ) %>% 
#   fmt_missing(
#     columns = 2:4,
#     missing_text = "0%"
#   ) %>% 
#   cols_label(
#     camp = "Camp",
#     tests_total = "Total tests",
#     tests_7day = "Tests in last 7 days",
#     #growth = "7-day growth(%)",
#     # negative = "Negative",
#     #not_done = "Not complete",
#     # pending = "Pending", 
#     positive = "Test positivity (%)",
#     #n_a = 'Result missing'
#   ) %>% 
#   tab_options(
#     container.overflow.x = TRUE,
#     container.overflow.y = TRUE,
#     grand_summary_row.background.color = "lightblue") 
#   # tab_spanner(
#   #   label = "Test results",
#   #   columns = 5:5,
#   # ) %>% 
#   # tab_options(
#   #   container.height = px(1000),
#   #   container.overflow.y = TRUE
#   #   #container.width = px(1000),
#   #   #table.font.size = "small"
#   # ) 

# tests_table <- ari_ili_tests_total %>% 
#   #left_join(ari_ili_cases_7day_df, by='camp') %>% 
#   left_join(ari_ili_tests_7day, by='camp') %>% 
#   left_join(ari_ili_tests_growth, by='camp') %>% 
#   left_join(ari_ili_tests_result, by='camp') %>% 
#   mutate(camp=trimws(camp)) %>% 
#   mutate(camp_number=str_extract(camp, regexp)) %>% 
#   mutate(camp_number=as.numeric(camp_number)) %>% 
#   arrange(camp_number) %>% 
#   select(-camp_number) %>% 
#   select(-c(growth,negative)) %>% 
#   mutate(camp=sub("^0+", "", camp)) %>% 
#   mutate(positive=scales::percent(positive,accuracy=0.1)) %>% 
#   datatable(., 
#             extensions = 'Buttons',
#             colnames=c('Camp', 'Total tests', 'Tests (last 7 days)', 'Test positvity'),
#             options = list(pageLength = 20,dom = 'Bfrtip', 
#                            columnDefs = list(list(className = 'dt-center', targets = 1:4)),
#                            buttons = c('csv')))

tests_df <- tests_data %>% 
  filter(!date=='Total') %>% 
  mutate(date_upd=excel_numeric_to_date(as.numeric(date))) %>% 
  select(-c('date','outside_cx_b')) %>% 
  mutate(week=isoweek(date_upd)) %>% 
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

# tests_age_group <- ari_ili_df %>%
#   filter(!laboratory_result %in% c('n_a','not_done')) %>%
#   filter(nationality=='fdmn') %>%
#   count(age_group, laboratory_result) %>%
#   filter(!is.na(age_group)) %>%
#   pivot_wider(names_from=laboratory_result, values_from=n) %>%
#   mutate(total=negative+positive) %>%
#   select(-negative) %>%
#   drop_na() %>%
#   group_by(age_group) %>%
#   mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
#                        broom::tidy())) %>%
#   unnest(rate) %>%
#   ungroup() %>%
#   ggplot(aes(x=age_group, y=estimate)) +
#   geom_point() +
#   #coord_flip() +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 position = position_dodge(width = 0.1)) +
#   theme_minimal() +
#   labs(x='Age group',
#        y='% COVID-19 + samples',
#        caption='Data source:ARI/ILI linelist') +
#   scale_y_continuous(labels = scales::percent)  +
#   theme(legend.position='top')



# tests_agegrp_gph <- ari_ili_df %>%
#   filter(!laboratory_result %in% c('n_a','not_done')) %>%
#   filter(nationality=='fdmn') %>%
#   count(age_group, laboratory_result) %>%
#   filter(!is.na(age_group)) %>%
#   pivot_wider(names_from=laboratory_result, values_from=n) %>%
#   mutate(total=negative+positive) %>%
#   select(-negative) %>%
#   drop_na() %>%
#   group_by(age_group) %>%
#   mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
#                        broom::tidy())) %>%
#   unnest(rate) %>%
#   ungroup() %>% 
#   ggplot(aes(x=age_group, y=estimate)) +
#   geom_col() +
#   coord_flip() +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 position = position_dodge(width = 0.1)) +
#   theme_minimal() +
#   labs(x='Age group',
#        y='% COVID-19 + samples',
#        caption='Data source:ARI/ILI linelist') +
#   scale_y_continuous(labels = scales::percent)  +
#   theme(legend.position='top')
# 
# 
# tests_sex_gph <- ari_ili_df %>%
#   filter(!laboratory_result %in% c('n_a','not_done')) %>%
#   filter(nationality=='fdmn') %>%
#   count(sex, laboratory_result) %>%
#   filter(!is.na(sex)) %>%
#   mutate(sex=case_when(sex=='m' ~ 'Male', 
#                        sex=='f' ~ 'Female')) %>% 
#   pivot_wider(names_from=laboratory_result, values_from=n) %>%
#   mutate(total=negative+positive) %>%
#   select(-negative) %>%
#   drop_na() %>%
#   group_by(sex) %>%
#   mutate(rate = map2(positive, total, ~ prop.test(.x, .y, conf.level=0.95) %>%
#                        broom::tidy())) %>%
#   unnest(rate) %>%
#   ungroup() %>% 
#   ggplot(aes(x=sex, y=estimate)) +
#   geom_col() +
#   coord_flip() +
#   geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
#                 position = position_dodge(width = 0.1)) +
#   theme_minimal() +
#   scale_fill_brewer(palette="Dark2", na.value="blue") +
#   labs(x='Sex',
#        y='% COVID-19 + samples',
#        caption='Data source:ARI/ILI linelist') +
#   scale_y_continuous(labels = scales::percent)  +
#   theme(legend.position='top')
# 
# tests_agegrp_gph +  tests_sex_gph

week_test_df <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  filter(!sample_type %in% c('follow_up', 'humanitarian_worker')) %>% 
  #filter(date_of_case_detection>=ymd('2020-05-01')) %>% 
  mutate(week=isoweek(date_of_case_detection)) %>% 
  #mutate(week=date2week(date_of_case_detection,week_start = "sun", floor_day = TRUE)) %>% 
  select(week,date_of_case_detection, laboratory_result, age) %>% 
  filter(week>19) %>% 
  #mutate(week=factor(week, levels=unique(week))) %>% 
  filter(laboratory_result %in% c('positive', 'negative')) 

tests_gph <- ggplot(week_test_df, aes(x=week, y=age, group=factor(week))) + 
  #geom_jitter(colour="lightblue", alpha=0.5, width=0.1) +
  #geom_point(stat="summary", fun.y="mean") + 
  geom_boxplot(alpha = 0.80) +
  #geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  labs(x="Week", y="Age (median with interquartile range)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_y_continuous(breaks=seq(0,100,10))


# TEST-AGE GROUP ----------------------------------------------------------

# population <- readxl::read_xlsx(here('data','block_population.xlsx'), sheet='Final', skip=1) %>% 
#   clean_names %>%  
#   clean_data() %>% 
#   filter(grepl('total', camp)) %>% 
#   filter(!camp=='grand_total') %>% 
#   select(-block) %>% 
#   mutate(across(c(infant_below_1:x16), as.numeric)) %>% 
#   mutate(age_0_18 = rowSums(.[4:11])) %>% 
#   mutate(age_18_59 = rowSums(.[12:13])) %>% 
#   mutate(age_over60 = rowSums(.[14:15])) %>% 
#   select(camp, contains('total'), contains('age')) %>% 
#   mutate(camp=gsub('_total', '', camp)) %>% 
#   mutate(camp=gsub('camp_', '', camp)) %>% 
#   mutate(camp=trimws(camp)) 



age_labs_decade <- c(paste(seq(0, 40, by = 10), seq(9, 49, by = 10),
                           sep = "-"), paste(50, "+", sep = ""))

tests_age_group_df <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  mutate(age_group=cut(age,breaks = c(seq(0, 50, by = 10), Inf), labels = age_labs_decade, right = FALSE)) %>% 
  select(age_group) %>% 
  #mutate(age_group=fct_explicit_na(age_group_decade, na_level = "(Age missing)")) %>% 
  count(age_group, .drop=FALSE) 


# age_group_pop <- population %>%  select(-c(total_families, total_individuals)) %>% 
#   summarise(across(contains('age'), sum)) %>% 
#   pivot_longer(age_0_18:age_over60) %>% 
#   mutate(age_group=labs) %>% 
#   select(-name)

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

