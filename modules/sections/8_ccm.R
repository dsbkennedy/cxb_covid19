#conflict_prefer("filter", "dplyr")
# Data import -------------------------------------------------------------

fac_list <- read.csv(here('data', 'fac_list.csv')) %>% 
  filter(!uid %in% c(87,495,151,294))




new_data <- GET(paste0(url,new_form_id,"/submissions/?format=json"),
                add_headers(Authorization = paste("token", access_token, sep = " "))
) %>% 
  content(., as="parsed") %>%
  spread_all() %>%
  clean_data() %>%
  as_tibble() %>%
  select(-json) %>%
  select(!contains('version')) %>%
  mutate(date_time_submission=ymd_hms(submission_time)) %>%
  filter(date(date_time_submission)>ymd('2020-10-12')) %>% 
  mutate(date_report=ymd(activity_info_date_report)) %>% 
  rename(uid=health_fac_select) %>% 
  mutate(health_fac_info_contact_number_focal=paste0('0',health_fac_info_contact_number_focal)) %>% 
  mutate(health_fac_info_contact_number_referral=paste0('0',health_fac_info_contact_number_referral))

saveRDS(new_data, here('data', 'ccm_data.Rds'))


# Data wrangling  ---------------------------------------------------------

fac_cols <- c('date_time_submission','org_type_select', 'uid',
              'health_fac_info_pat_severity', 'health_fac_info_special_needs', 
              'health_fac_info_sari_beds', 'health_fac_info_non_sari_beds')


fac_data <- new_data %>% 
  dplyr::filter(activity_type_select==2) %>% 
  group_by(uid) %>% 
  #filter(!submission_time=='2020_12_05t10_43_50') %>% 
  filter(date_time_submission==last(date_time_submission)) %>% 
  #select(fac_cols) %>% 
  mutate(mild_mod=case_when(grepl('1', health_fac_info_pat_severity) ~ 'X'),
         severe=case_when(grepl('2', health_fac_info_pat_severity) ~ 'X'),
         critical=case_when(grepl('3', health_fac_info_pat_severity) ~ 'X')) %>% 
  mutate(paed=case_when(grepl('1', health_fac_info_special_needs) ~ 'X'),
         preg=case_when(grepl('2', health_fac_info_special_needs) ~ 'X'),
         sam=case_when(grepl('3', health_fac_info_special_needs) ~ 'X')) %>% 
  mutate(total_beds=as.numeric(health_fac_info_sari_beds) + as.numeric(health_fac_info_non_sari_beds)) %>% 
  select(uid,total_beds,health_fac_info_sari_beds, mild_mod:sam,
         health_fac_info_contact_number_referral)


daily_data <- new_data %>% filter(activity_type_select==1) %>% 
  select(uid,date_time_submission, date_report, contains('activity')) %>% 
  remove_empty() 


daily_vars <- c('activity_info_active_patients_beds_mild_moderate', 'activity_info_active_patients_beds_severe', 'activity_info_active_patients_beds_critical',
                'activity_info_last_24h_new_pat_admit_y_new_pat_host_admit_sum', 'activity_info_last_24h_new_pat_admit_y_new_pat_fdmn_admit_sum','activity_info_last_24h_new_pat_admit_total',
                'activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_sum', 'activity_info_last_24h_new_pat_refer_y_new_pat_refer_sum','activity_info_last_24h_new_pat_died_y_new_pat_died_sum')  

num_vars <- c('total_mild_mod','total_severe','total_critical','total_current_admit','total_beds',
              'total_admissions_24h','total_discharges_24h','total_referrals_24h','total_deaths_24h')


# Building table ----------------------------------------------------------

table_df <- daily_data %>% select(uid, date_report, daily_vars) %>% 
  full_join(fac_data,.,by='uid') %>% 
  mutate(total_admissions=as.numeric(activity_info_active_patients_beds_mild_moderate) + as.numeric(activity_info_active_patients_beds_severe) + as.numeric(activity_info_active_patients_beds_critical)) %>% 
  select(uid,health_fac_info_contact_number_referral,mild_mod,severe,critical,paed,preg,sam,total_beds,date_report,
         total_mild_mod=activity_info_active_patients_beds_mild_moderate, 
         total_severe=activity_info_active_patients_beds_severe, 
         total_critical=activity_info_active_patients_beds_critical,
         host_admissions=activity_info_last_24h_new_pat_admit_y_new_pat_host_admit_sum,
         fdmn_admissions=activity_info_last_24h_new_pat_admit_y_new_pat_fdmn_admit_sum,
         total_discharges_24h=activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_sum,
         total_referrals_24h=activity_info_last_24h_new_pat_refer_y_new_pat_refer_sum,
         total_deaths_24h=activity_info_last_24h_new_pat_died_y_new_pat_died_sum) %>% 
  mutate(total_current_admit=as.numeric(total_mild_mod) + as.numeric(total_severe) + as.numeric(total_critical),.after = total_critical) %>% 
  mutate(total_admissions_24h=as.numeric(host_admissions) + as.numeric(fdmn_admissions), .after=fdmn_admissions) %>% 
  group_by(uid) %>% 
  #filter(date_report==max(date_report)) %>% 
  distinct() %>% 
  ungroup() %>% 
  replace_na(list(total_mild_mod= 0, total_severe = 0, total_critical=0, total_current_admit=0,
                  host_admissions=0, fdmn_admissions=0, total_admissions_24h=0, total_discharges_24h=0, total_referrals_24h=0, total_deaths_24h=0)) %>% 
  mutate(uid=as.numeric(uid)) %>% 
  full_join(fac_list,., by='uid') %>% 
  select(name, uid,total_mild_mod, total_severe, total_critical,total_current_admit, total_beds,
         total_admissions_24h,	total_discharges_24h,	total_referrals_24h,total_deaths_24h,date_report, 
         fdmn_admissions, host_admissions,
         referral_contact=health_fac_info_contact_number_referral, mild_mod:sam) %>% 
  mutate(occupancy=total_current_admit/total_beds, .after=total_beds) %>% 
  #mutate(total_deaths_24h=NA) %>% 
  mutate(across(num_vars, as.numeric)) 

cumulative_admissions_df <- table_df %>% select(date_report, fdmn_admissions, host_admissions) %>% 
  pivot_longer(-date_report) %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(name, date_report) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  group_by(name) %>% 
  mutate(cumulative=cumsum(total)) %>% 
  arrange(date_report) %>% 
  pivot_wider(names_from=name, values_from=c('total', 'cumulative'))

#write.csv(x, here('data', 'cumulative_admit.csv'))

# Facility summary --------------------------------------------------------


# #Highest case numbers
# plot_spark1 <- function(data){
#   data %>%
#     #mutate(admit24h=as.numeric(activity_info_last_24h_new_pat_admit_total)) %>%
#     ggplot(aes(x = date, y = occupancy)) +
#     geom_line(size = 15) +
#     theme_void() +
#     theme(legend.position = "none")
# }
# 
# sparknamedata1 <- table_df %>%
#   group_by(name)
#   #filter(!is.na(date_report)) %>%
#   #slice_max(date_report, n=10)
# 
# sparknamedata1$name <- as.character(sparknamedata1$name)
# sparknamedata1$name <- fct_inorder(sparknamedata1$name)
# spark_plots1 <- table_df %>%
#   as.data.frame() %>%
#   #filter(!is.na(date_report)) %>%
#   #filter(name %in% unique(sparknamedata1$name)) %>%
#   select(name, date=date_report, occupancy) %>%
#   mutate(name=factor(name, levels=unique(sparknamedata1$name))) %>%
#   mutate(camp_number=str_extract(name, "[[:digit:]]+")) %>%
#   mutate(camp_number=as.numeric(camp_number)) %>%
#   arrange(camp_number,date) %>%
#   #arrange(name, date) %>%
#   select(-camp_number) %>%
#   nest(occupancy = c(date, occupancy)) %>%
#   mutate(plot = map(occupancy, plot_spark1))
# 


facility_summary <- table_df %>% 
  select(-c('fdmn_admissions', 'host_admissions')) %>% 
  group_by(name) %>% 
  filter(date_report==last(date_report) | is.na(date_report)) %>%
  #summarise_all(last) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(-uid) %>% 
  #mutate(ggplot = NA,.after=occupancy) %>%
  mutate(camp_number=str_extract(name, "[[:digit:]]+")) %>% 
  mutate(camp_number=as.numeric(camp_number)) %>% 
  arrange(camp_number) %>% 
  #arrange(name, date) %>% 
  select(-camp_number) %>% 
  #arrange(name) %>% 
  gt() %>% 
  opt_row_striping(., row_striping = TRUE) %>% 
  # text_transform(
  #   locations = cells_body(vars(ggplot)),
  #   fn = function(x){
  #     map(spark_plots1$plot, ggplot_image, height = px(15), aspect_ratio = 4)
  #   }
  # ) %>%
  tab_spanner(
    label = "Currently accepting?",
    columns = vars(mild_mod,severe,critical)
  ) %>% 
  tab_spanner(
    label = "Accepting special needs patients?",
    columns = vars(paed, sam,preg)
  ) %>% 
  tab_spanner(
    label = "Patient status",
    columns = vars(total_mild_mod,	total_severe,	total_critical)
  ) %>% 
  tab_spanner(
    label = "Beds",
    columns = vars(total_current_admit,	total_beds,	occupancy)
  ) %>% 
  tab_spanner(
    label = "Activity in last 24 hours",
    columns = vars(total_admissions_24h,	total_discharges_24h,	total_referrals_24h,	total_deaths_24h)
  ) %>% 
  fmt_percent(
    columns = vars(occupancy),
    decimals = 0
  ) %>% 
  fmt_missing(
    columns = 1:19,
    missing_text = ""
  ) %>% 
  cols_label(
    name = "Facility name",
    referral_contact = "24/7 Referral hotline",
    date_report = "Reporting date",
    mild_mod = "Mild/Moderate",
    severe = "Severe",
    critical = "Critical",
    paed = "Paediatric",
    sam = "SAM",
    preg = "Pregnant/PP",
    total_beds = "Functional",
    total_current_admit = "Occupied", 
    total_mild_mod = "Mild/Moderate",
    total_severe = "Severe",
    total_critical = "Critical",
    occupancy = "Occupancy (%)", 
    total_admissions_24h = "Admissions", 
    total_discharges_24h = "Discharges",
    total_referrals_24h = "Referrals",
    total_deaths_24h = "Deaths",
    #ggplot="Trend"
  ) %>%
  summary_rows(fns = list(Total = ~ sum(., na.rm=TRUE)), columns = vars(total_mild_mod,total_severe,total_critical,total_current_admit,total_beds,                                                        total_admissions_24h,total_discharges_24h,total_referrals_24h,total_deaths_24h ),
               formatter = fmt_number,
               decimals = 0,
               use_seps = TRUE) %>%
  (function(x) {
    res <- function() x$`_data` %>%
      dplyr::summarize(occupancy = sum(as.numeric(total_current_admit),na.rm=TRUE) / 
                         sum(as.numeric(total_beds),na.rm=TRUE)) %>%
      dplyr::pull(.data$occupancy)
    summary_rows(x, fns = list(Total = ~ res()), columns = vars(occupancy),
                 formatter = fmt_percent,
                 decimals = 0,
                 use_seps = TRUE)
  }) %>%
  data_color(
    columns = vars(occupancy),
    colors = scales::col_numeric(
      "Reds",
      domain = c(0, 1.5), na.color = "grey89")
  )  %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = gt::everything()
  ) 



tonumeric <- c('activity_info_last_24h_new_pat_admit_y_new_pat_admit_fdmn_m',
               'activity_info_last_24h_new_pat_admit_y_new_pat_admit_fdmn_f',
               'activity_info_last_24h_new_pat_admit_y_new_pat_fdmn_admit_sum',
               'activity_info_last_24h_new_pat_admit_y_new_pat_host_admit_sum',
               'activity_info_last_24h_new_pat_admit_y_new_pat_admit_host_m',
               'activity_info_last_24h_new_pat_admit_y_new_pat_admit_host_f',
               'activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_m',
               'activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_f',
               'activity_info_last_24h_new_pat_refer_y_new_pat_refer_m',
               'activity_info_last_24h_new_pat_refer_y_new_pat_refer_f', 
               'activity_info_last_24h_new_pat_died_y_new_pat_died_m',
               'activity_info_last_24h_new_pat_died_y_new_pat_died_f')

demo_df <- new_data %>% 
  filter(activity_type_select==1) %>% 
  select(uid, date_report, contains('fdmn'), contains('host'), 
         contains('_m'), contains('_f')) %>% 
  #remove_empty() %>% 
  mutate(across(tonumeric, as.numeric)) 


historic_data_long <- readRDS(here('data', 'historic_data_long.rds' )) %>% ungroup()

historic_admissions <- historic_data_long %>% 
  filter(name=='admissions') %>% 
  select(name=fac_name, date_report,total)
new_admissions <- table_df %>% 
  select(name, date_report,total=total_admissions_24h) 

all_admissions <- historic_admissions %>% 
  bind_rows(new_admissions) %>% 
  filter(!is.na(date_report)) %>% 
  distinct() %>% 
  group_by(name) %>% 
  arrange(date_report) %>% 
  mutate(cumulative=cumsum(total)) %>% 
  ungroup() %>% 
  dplyr::arrange(name,date_report)

cumulative_admissions <- all_admissions %>% 
  group_by(date_report) %>% 
  summarise(total=sum(total, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(cumulative=cumsum(total))


###ADMISSIONS DATA

historic_adm_dis <- historic_data_long %>% 
  filter(name %in% c('admissions','discharges')) %>% 
  #select(name, date_report,total) %>% 
  group_by(name, date_report) %>% 
  summarise(value=sum(total, na.rm=TRUE)) %>% 
  select(date_report,value) 

new_admissions <- table_df %>% 
  group_by(date_report) %>% 
  summarise(admissions=sum(total_admissions_24h, na.rm=TRUE),
            discharges=sum(total_discharges_24h, na.rm=TRUE)) %>% 
  select(date_report,admissions, discharges) %>% 
  pivot_longer(-date_report)

all_adm_dis <- historic_adm_dis %>% 
  bind_rows(new_admissions) %>% 
  ungroup() %>% 
  arrange(date_report) %>% 
  group_by(name, date_report) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  group_by(name) %>% 
  mutate(cumulative=cumsum(total)) 


adm_dis_gph <-  all_adm_dis %>% 
  ggplot(., aes(x=date_report, y=cumulative, color=name)) +
  geom_line() +
  theme_minimal() + 
  labs(x='Date', y='Cumulative', color='Indicator') +
  scale_y_continuous(breaks=pretty_breaks(n=8))

rm(historic_adm_dis, new_admissions, all_adm_dis)

# require(openxlsx)
# list_of_datasets <- list("all_data" = all_admissions, "cumulative_data" = cumulative_admissions)
# write.xlsx(list_of_datasets, file = here('data', 'admissions_data.xlsx'))

### Admissions in last 24 hours by nationality

historic_admissions_nationality <- 
  historic_data_long %>% 
  filter(name %in% c('admissions_fdmn', 'admissions_host')) %>% 
  group_by(date_report,name) %>% 
  summarise(total=sum(total,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(date_report,name,total)
  
nationality_df <- demo_df %>% 
  select(uid,date_report,
         admissions_fdmn=activity_info_last_24h_new_pat_admit_y_new_pat_fdmn_admit_sum,
         admissions_host=activity_info_last_24h_new_pat_admit_y_new_pat_host_admit_sum) %>% 
  pivot_longer(-c('uid', 'date_report')) %>% 
  group_by(date_report,name) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  bind_rows(historic_admissions_nationality) %>% 
  mutate(name=case_when(grepl('host',name) ~ 'Host', 
                        TRUE ~ 'FDMN')) %>% 
  arrange(name,date_report) %>% 
  group_by(name) %>% 
  mutate(cumluative=cumsum(total))

nationality_gph <- nationality_df %>% 
  ggplot(., aes(x=date_report,y=cumluative,color=name)) +
  geom_line() +
  #geom_col() +
  theme_minimal() +
  labs(x='Report date', y='Count', color='Nationality', 
       title='')

# nat_breakdown_table <- nationality_df %>% 
#   arrange(name, date_report) %>% 
#   complete(nesting(date_report,name), fill=list(total=0)) %>% 
#   group_by(name) %>% 
#   mutate(cumulative=cumsum(total)) %>% 
#   #filter(cumulative>0) %>% 
#   filter(!is.na(date_report))
# 
# write.csv(nat_breakdown_table, here('data', 'nat_breakdown.csv'))
# x <- nat_breakdown_table %>% 
#   select(-total) %>% 
#   ungroup() %>% 
#   distinct() %>% 
#   pivot_wider(., names_from=name, values_from=cumulative, values_fn = length)
# 
# x <- nat_breakdown_table %>% 
#   #select(-total) %>% 
#   group_by(date_report) %>% 
#   mutate(prop=cumulative/sum(cumulative)) %>% 
#   ungroup() %>% 
#   complete(nesting(date_report,name), fill=list(cumulative=0))
# 
# x %>% 
#   complete(name, date_report, fill = list(cumulative = 0)) %>% 
#   
# 
# write.csv(x, here('data', 'nat_breakdown.csv'))
# 
# x %>% 
#   select(-prop) %>% 
#   ungroup() %>% 
#   distinct() %>% 
#   #mutate(cumulative=as.character(cumulative)) %>% 
#   #mutate(row = row_number()) %>%
#   pivot_wider(id_cols=date_report, names_from=name, values_from=cumulative)
# 
# x %>% 
#   ggplot(., aes(x=date_report, y=cumulative, color=name)) +
#   geom_line()
# 
# x %>% 
#   pivot_longer(-c('date_report', 'name'))
# 
# 
# nationality_df %>% 
#   arrange(date_report) %>%  
#   group_by(name) %>% 
#   mutate(cumulative=cumsum(total)) %>% 
#   ggplot(., aes(x=date_report, y=cumulative, color=name)) +
#   geom_line()
#   



### Admissions in last 24 hours by gender

historic_admissions_gender <- 
  historic_data_long %>% 
  filter(name %in% c('admissions_f', 'admissions_m', 
                     'discharges_f', 'discharges_m',
                     'referrals_f', 'referrals_m')) %>% 
  group_by(date_report,name) %>% 
  summarise(total=sum(total,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(date_report,name,total)

gender_df <- demo_df %>% 
  #mutate(across(tonumeric, as.numeric)) %>% 
  mutate(admissions_m=activity_info_last_24h_new_pat_admit_y_new_pat_admit_fdmn_m + 
           activity_info_last_24h_new_pat_admit_y_new_pat_admit_host_m) %>% 
  mutate(admissions_f=activity_info_last_24h_new_pat_admit_y_new_pat_admit_fdmn_f + 
           activity_info_last_24h_new_pat_admit_y_new_pat_admit_host_f) %>% 
  select(uid,date_report,
         admissions_m,
         admissions_f,
         discharges_m=activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_m,
         discharges_f=activity_info_last_24h_new_pat_discharged_y_new_pat_discharged_f,
         referrals_m=activity_info_last_24h_new_pat_refer_y_new_pat_refer_m,
         referrals_f=activity_info_last_24h_new_pat_refer_y_new_pat_refer_f,
         deaths_m=activity_info_last_24h_new_pat_died_y_new_pat_died_m,
         deaths_f=activity_info_last_24h_new_pat_died_y_new_pat_died_f) %>% 
  pivot_longer(-c('uid', 'date_report')) %>% 
  group_by(date_report,name) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  bind_rows(historic_admissions_gender) %>% 
  mutate(indicator=case_when(grepl('admissions', name) ~ 'Admissions',
                             grepl('discharges', name) ~ 'Discharges',
                             grepl('refer', name) ~ 'Referrals',
                             grepl('deaths', name) ~ 'Deaths')) %>% 
  mutate(gender=case_when(grepl('_m', name) ~ 'Male',
                          grepl('_f', name) ~ 'Female')) %>% 
  complete(indicator, gender, date_report,fill=list(total=0)) %>% 
  arrange(indicator, gender, date_report) %>% 
  group_by(indicator, gender) %>% 
  mutate(cumulative=cumsum(total)) 
  

gender_gph <- gender_df %>% 
  mutate(indicator=factor(indicator, levels=c('Admissions', 'Discharges', 'Referrals', 'Deaths'))) %>% 
  ggplot(., aes(x=date_report, y=cumulative, color=gender)) +
  geom_line() +
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") +
  labs(x='Report date', y='Count', color='Sex', 
       title='') +
  #scale_y_continuous(breaks=c(0,3,6,9,12,15)) %>% 
  facet_wrap(~indicator, ncol=2, scales="free_y")

###cumulative indicators

cumulative_indicator_df <- gender_df %>% 
  group_by(date_report,indicator) %>% 
  summarise(n=sum(total,na.rm=TRUE)) %>% 
  complete(date_report,indicator,fill=list(n=0)) %>% 
  group_by(indicator) %>% 
  mutate(cumulative=cumsum(n)) %>% 
  pivot_wider(names_from=indicator,values_from=c(n,cumulative))
  
#write.csv(cumulative_indicator_df, here('data', 'cumulative_indicators.csv'))
### Admissions by severity

historic_severity <- readRDS(here('data', 'severity_data.rds'))
  
severity_df <- table_df %>% 
  select(uid, date_report, total_mild_mod, total_severe, total_critical) %>% 
  filter(!is.na(date_report)) %>% 
  pivot_longer(-c('uid', 'date_report')) %>% 
  group_by(date_report, name) %>% 
  summarise(total=sum(value,na.rm=TRUE)) %>% 
  mutate(name=case_when(grepl('critical', name) ~ 'Critical',
                        grepl('severe', name) ~ 'Severe',
                        TRUE ~ 'Mild/Moderate')) %>% 
  bind_rows(historic_severity) %>% 
  mutate(name=factor(name, levels=c('Mild/Moderate', 'Severe', 'Critical'))) %>% 
  filter(!is.na(date_report)) %>% 
  group_by(date_report) %>% 
  mutate(total_admitted=sum(total, na.rm=TRUE)) 
  #ungroup() %>% 
  #arrange(date_report) %>% 
  #mutate(roll_admitted=zoo::rollmean(total_admitted,3,align='right', fill=NA)) %>% 
  #ungroup()

admissions_trend_df <- severity_df %>% 
  group_by(date_report) %>% 
  summarise(total_admitted=sum(total, na.rm=TRUE)) %>% 
  mutate(roll_admitted=zoo::rollmean(total_admitted,3,align='right', fill=NA))

severity_gph <- 
  ggplot(data=severity_df, aes(x=date_report, y=total)) +
  geom_col(aes(fill=name)) +
  geom_line(data=admissions_trend_df, aes(x=date_report, y=roll_admitted)) +
  labs(x='Report date', y='Total patients currently admitted', fill='Severity', 
       title='') +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 3))


###Occupancy
historic_occupancy <- readRDS(here('data', 'historic_occupancy.rds')) %>% 
  rename(name=fac_name) %>% 
  #select(-c('occupancy')) %>% 
  filter(date_report<ymd('2020-10-13')) %>% 
  group_by(date_report,name) %>% 
  summarise(total_beds=sum(total_beds,na.rm=TRUE),
            total_current_admit=sum(total_current_admit,na.rm=TRUE))
  #mutate(dataset='old tool')

occupancy_df <- table_df %>%
  # select(date_report,uid, total_beds, total_current_admit) %>%
  # filter(!is.na(date_report)) %>%
  # group_by(uid) %>%
  # complete(date_report=seq.Date(min(date_report), max(date_report,na.rm=TRUE), by='day')) %>%
  # fill(total_beds)
  # filter(total_beds>0)
  # #group_by(uid)
  # group_by(date_report) %>%
  # summarise(total_beds=sum(total_beds,na.rm=TRUE),
  #           total_current_admit=sum(total_current_admit,na.rm=TRUE))
  select(date_report,name, total_beds,total_current_admit) %>%
  complete(date_report, name) %>% 
  group_by(name) %>% 
  fill(., total_beds, .direction='down') %>% 
  mutate(total_beds=case_when(grepl('OCA- Kutupalong',name) ~ 41, 
         TRUE ~ total_beds)) %>% 
  group_by(name,date_report) %>% 
  filter(total_beds>0) %>% 
  filter(!is.na(date_report)) %>% 
  summarise(total_beds=sum(total_beds,na.rm=TRUE),
            total_current_admit=sum(total_current_admit,na.rm=TRUE)) %>% 
  bind_rows(historic_occupancy) %>% 
  distinct() %>% 
  group_by(name,date_report) %>% 
  summarise(total_beds=sum(total_beds,na.rm=TRUE),
            total_current_admit=sum(total_current_admit,na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(name)) %>% 
  mutate(occupancy=total_current_admit/total_beds)

write.csv(occupancy_df, here('data', 'occupancy_data.csv'))

# occupancy_df %>% 
#   ggplot(., aes(x=date_report, y=occupancy)) +
#   geom_line() +
#   theme_minimal() +
#   labs(x='Date', y='Occupancy', title='CXB: Health Facility Occupancy') +
#   scale_y_continuous(labels=scales::percent) 
  

  #summarise(total_beds=dplyr::first(total_beds))
  #filter(!is.na(total_beds)) %>%
  # ungroup() %>%
  # summarise(total_beds=sum(total_beds))
  # mutate(dataset='current') %>%
  # bind_rows(historic_occupancy)
  # filter(!is.na(date_report)) %>%
  # mutate(occupancy=total_current_admit/total_beds)

first_date <- min(table_df$date_report,na.rm=TRUE)
last_date <- today()-1

fac_data2 <- new_data %>% 
  dplyr::filter(activity_type_select==2) %>% 
  mutate(total_beds=as.numeric(health_fac_info_sari_beds) + 
           as.numeric(health_fac_info_non_sari_beds)) %>% 
  select(uid,date_time_submission,total_beds)  %>% 
  group_by(uid) %>% 
  filter(date_time_submission==max(date_time_submission)) %>% 
  mutate(date_report=date(date_time_submission)) %>% 
  ungroup() %>% 
  complete(date_report=seq.Date(first_date, last_date, by='day')) %>% 
  complete(date_report,uid) %>% 
  group_by(uid) %>% 
  fill(total_beds, .direction='down') %>% 
  group_by(date_report) %>% 
  summarise(total_beds=sum(total_beds,na.rm=TRUE)) %>% 
  mutate(dataset='new tool') %>% 
  bind_rows(historic_occupancy)
  
total_beds_gph <-  fac_data2 %>% 
  #filter(total_beds>0) %>% 
  ggplot(., aes(x=date_report, y=total_beds, color=dataset)) +
  #geom_line() +
  geom_col(position='dodge') +
  #scale_y_continuous(labels=scales::percent) +
  labs(x='Date', y='Total beds', fill='') +
  theme_minimal()
#facet_wrap(~dataset, scales='free_x')


###Valueboxes for landing page

###FDMN Admissions in last 24 hours 

fdmn_admit_24h <- table_df %>%  
  group_by(date_report) %>% 
  mutate(fdmn_admissions=as.numeric(fdmn_admissions)) %>% 
  summarise(fdmn_admit_24h=sum(fdmn_admissions,na.rm=TRUE)) %>% 
  filter(date_report==max(date_report, na.rm=TRUE)) %>% 
  pull(fdmn_admit_24h)

fdmn_admit_vb <- valueBox(
  fdmn_admit_24h,
  icon= "fas fa-clinic-medical",
  color= "#4472C4"
)

###Host Admissions in last 24 hours 

host_admit_24h <- table_df %>%  
  group_by(date_report) %>% 
  mutate(host_admissions=as.numeric(host_admissions)) %>% 
  summarise(host_admit_24h=sum(host_admissions,na.rm=TRUE)) %>% 
  filter(date_report==max(date_report, na.rm=TRUE)) %>% 
  pull(host_admit_24h)

host_admit_vb <- valueBox(
  host_admit_24h,
  icon= "fas fa-clinic-medical",
  color= "#ED7D31"
)

