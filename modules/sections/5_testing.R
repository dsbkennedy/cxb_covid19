
# WRANGLING ---------------------------------------------------------------

breaks <- c(-Inf,5,59,Inf)
labs <- c('Under 5', '5 to 59', '60 and over')

ari_ili_df <- gsheet_data$ari_ili %>% 
  clean_names() %>% 
  clean_data() %>% 
  mutate(camp=gsub('camp_', '', camp)) %>% 
  filter(nationality=='fdmn') %>% 
  filter(sample_type!='follow_up') %>% 
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
  mutate(cumulative_cases=cumsum(n)) %>%
  mutate(case_growth=ifelse(lag(cumulative_cases,7)>10, 
                            ((cumulative_cases/lag(cumulative_cases,7))^(1/7))-1,NA)) %>%
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

ari_ili_tests_growth <- ari_ili_tests_df %>% 
  select(camp, case_growth) %>% 
  group_by(camp) %>% 
  summarise(growth=last(case_growth))

ari_ili_tests_result <- ari_ili_df %>% 
  filter(nationality=='fdmn') %>% 
  count(camp, laboratory_result) %>% 
  group_by(camp) %>% 
  mutate(prop=n/sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from=laboratory_result, values_from=prop) %>% 
  select(camp, positive,negative)


# TABLE -------------------------------------------------------------------

tests_table <- ari_ili_tests_total %>% 
  left_join(ari_ili_tests_7day, by='camp') %>% 
  left_join(ari_ili_tests_growth, by='camp') %>% 
  left_join(ari_ili_tests_result, by='camp') %>% 
  mutate(camp=trimws(camp)) %>% 
  mutate(camp_number=str_extract(camp, regexp)) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  select(-camp_number) %>% 
  select(-c(growth,negative)) %>% 
  mutate(camp=sub("^0+", "", camp)) %>% 
  gt() %>% 
  opt_row_striping(., row_striping = TRUE) %>% 
  fmt_percent(
    columns = 4:4,
    decimals = 1
  ) %>% 
  fmt_missing(
    columns = 2:4,
    missing_text = "0%"
  ) %>% 
  cols_label(
    camp = "Camp",
    tests_total = "Total tests",
    tests_7day = "Tests in last 7 days",
    #growth = "7-day growth(%)",
    # negative = "Negative",
    #not_done = "Not complete",
    # pending = "Pending", 
    positive = "Test positivity (%)",
    #n_a = 'Result missing'
  ) %>% 
  # tab_spanner(
  #   label = "Test results",
  #   columns = 5:5,
  # ) %>% 
  tab_options(
    container.height = px(1000),
    container.overflow.y = TRUE
    #container.width = px(1000),
    #table.font.size = "small"
  ) 


# TEST-POSITIVITY ---------------------------------------------------------

tests_gph <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
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
  ungroup() %>% 
  ggplot(aes(x=age_group, y=estimate)) +
  geom_point() +
  #coord_flip() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position = position_dodge(width = 0.1)) +
  theme_minimal() +
  labs(x='Age group', 
       y='% COVID-19 + samples', 
       caption='Data source:ARI/ILI linelist') +
  scale_y_continuous(labels = scales::percent)  +
  theme(legend.position='top')


# TEST-AGE GROUP ----------------------------------------------------------

population <- readxl::read_xlsx(here('data','block_population.xlsx'), sheet='Final', skip=1) %>% 
  clean_names %>%  
  clean_data() %>% 
  filter(grepl('total', camp)) %>% 
  filter(!camp=='grand_total') %>% 
  select(-block) %>% 
  mutate(across(c(infant_below_1:x16), as.numeric)) %>% 
  mutate(age_0_5 = rowSums(.[4:7])) %>% 
  mutate(age_5_59 = rowSums(.[8:13])) %>% 
  # mutate(age_18_59 = rowSums(.[12:13])) %>% 
  mutate(age_over60 = rowSums(.[14:15])) %>% 
  select(camp, contains('total'), contains('age')) %>% 
  mutate(camp=gsub('_total', '', camp)) %>% 
  mutate(camp=gsub('camp_', '', camp)) %>% 
  mutate(camp=trimws(camp)) 


tests_age_group_df <- ari_ili_df %>%  
  filter(!laboratory_result %in% c('n_a','not_done')) %>% 
  filter(nationality=='fdmn') %>% 
  count(age_group, .drop=FALSE) 


age_group_pop <- population %>%  select(-c(total_families, total_individuals)) %>% 
  summarise(across(contains('age'), sum)) %>% 
  pivot_longer(age_0_5:age_over60) %>% 
  mutate(age_group=labs) %>% 
  select(-name)

tests_age_group_gph <-  tests_age_group_df %>% 
  left_join(age_group_pop, by='age_group') %>% 
  filter(!is.na(age_group)) %>% 
  mutate(tests_per10000=(n/value)*10000) %>% 
  mutate(age_group=factor(age_group, levels=c('Under 5', '5 to 59', '60 and over'))) %>% 
  ggplot(aes(x=age_group, y=tests_per10000, fill=age_group)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position="none") +
  labs(x='Age groups', y='Tests per 10,000 people', fill='Age groups')

