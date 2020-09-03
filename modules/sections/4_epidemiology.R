
# GODATA WRANGLING --------------------------------------------------------

godata_wide <- cases %>%
  clean_names() %>% 
  filter(deleted == FALSE) %>%
  unnest(c(
    #addresses,
    date_ranges,
    questionnaire_answers_health_facility,
    questionnaire_answers_status_at_detection,
    questionnaire_answers_outcome,
    questionnaire_answers_outcome_2,
    questionnaire_answers_msf_outcome,
    contains('nationality'),
    contains('result'),
    contains('symptom')
  ),
  keep_empty = TRUE, names_sep = "_") %>% 
  remove_empty(c("rows", "cols")) 


##Get GoData ID from FDMN sheet 

godata_case_id <- fdmn_raw %>% clean_names() %>% filter(!is.na(go_data_case_id)) %>%  pull(go_data_case_id)


###Clean up GoData

##Define age groups for GoData
age_labs <- c(paste(seq(0, 50, by = 10), seq(9, 59, by = 10),
                    sep = "-"), paste(60, "+", sep = ""))

godata_clean <- godata_wide %>% 
  #Ethnicity
  mutate(population_group=coalesce(questionnaire_answers_nationality_value,questionnaire_answers_nationality_2_value,questionnaire_answers_cif_nationality_value)) %>% 
  mutate(lab_result=coalesce(questionnaire_answers_lab_result_value,questionnaire_answers_result_2_value)) %>% 
  mutate(fdmn_positive=case_when(visual_id %in% godata_case_id ~ 1,
                                 (population_group=='FDMN' & lab_result=='Positive') ~ 1)) %>% 
  filter(fdmn_positive==1) %>% 
  #filter(visual_id %in% godata_case_id) %>% 
  #filter(population_group=='FDMN')  %>% 
  #Test result
  #Some records don't have lab data in GoData. These are dropped when filter==positive
  # mutate(lab_result=case_when(visual_id %in% c("CXB3310283", "CXB3310414", 
  #                                              "CXB5310154", "CXB4950010",
  #                                              "CXB3310590", "CXB5310243",
  #                                              "CXB2120064","CXB3310451", 
  #                                              "CXB3310542", "CXB3310603", 
  #                                              "CXB3830005","CXB2008001") ~ 'Positive',
  #                             TRUE ~ lab_result)) %>% 
  # filter(lab_result=='Positive') %>% 
  #Dates
  mutate(date_of_reporting = guess_dates(date_of_reporting),
         #date_of_data_entry = guess_dates(createdat),
         date_of_onset = guess_dates(date_of_onset),
         date_of_outcome = guess_dates(date_of_outcome),
         date_of_last_contact = guess_dates(date_of_last_contact),
         date_become_case = guess_dates(date_become_case)) %>% 
  #Age
  mutate(age_years = case_when(
    is.na(age_years) && !is.na(age_months) ~  as.integer(age_months / 12),
    #is.na(age_years) && is.na(age_months) ~  NA_integer_,
    TRUE ~ as.integer(age_years))) %>%
  #Age group
  mutate(age_group=cut(age_years,breaks = c(seq(0, 60, by = 10), Inf), labels = age_labs, right = FALSE)) %>%
  mutate(age_group=fct_explicit_na(age_group, na_level = "(outcome missing)")) %>% 
  #Classification
  filter(classification != "lng_reference_data_category_case_classification_not_a_case_discarded") %>%
  mutate(classification = case_when(
    grepl('confirmed', classification, ignore.case=TRUE) ~ "confirmed",
    grepl('suspect', classification, ignore.case=TRUE) ~ "suspect",
    grepl('probable', classification, ignore.case=TRUE) ~ "probable"
  )) %>%
  #Sex
  mutate(sex = case_when(
    grepl('_male', gender, ignore.case=TRUE) ~ "Male",
    grepl('_female', gender, ignore.case=TRUE) ~ "Female", 
    visual_id=="CXB3310202" ~ 'Female',
  )) %>% 
  mutate(sex=factor(sex, levels=c('Male', 'Female'))) %>% 
  #Updated this classification to match survey
  #updated to "outcome"
  mutate(outcome = case_when(
    grepl('alive', outcome_id, ignore.case=TRUE) ~ "Alive",
    grepl('deceased', outcome_id, ignore.case=TRUE) ~ "Dead",
    grepl('recovered', outcome_id, ignore.case=TRUE) ~ "Recovered",
    #grepl('death', questionnaire_answers_outcome, ignore.case=TRUE) ~ "dead",
    grepl('healthy', questionnaire_answers_outcome_2_value, ignore.case=TRUE) ~ "Alive",
    grepl('not recovered', questionnaire_answers_outcome_2_value, ignore.case=TRUE) ~ "Alive",
    grepl('$recovered', questionnaire_answers_outcome_2_value, ignore.case=TRUE) ~ "Recovered",
    questionnaire_answers_msf_outcome_value=='Recovered' ~ 'Recovered',
    questionnaire_answers_outcome_2_value=='Recovered' ~ 'Recovered',
    grepl('death', questionnaire_answers_outcome_2_value, ignore.case=TRUE) ~ "Dead", 
    visual_id %in% c("CXB3310414","CXB3310590","CXB2120064") ~ 'Alive',
  )) %>%
  #     mutate(outcome=fct_explicit_na(outcome, na_level = "(Outcome missing)"))  %>% 
  #Status at detection
  mutate(detection_status = questionnaire_answers_status_at_detection_value) %>%
  #Risk level
  mutate(risk_level = case_when(
    grepl('low', risk_level, ignore.case=TRUE) ~ "low",
    grepl('medium', risk_level, ignore.case=TRUE) ~ "medium",
    grepl('high', risk_level, ignore.case=TRUE) ~ "high"
  )) %>%
  #Updated available options
  mutate(status = case_when(
    grepl('hospitalization', date_ranges_typeId, ignore.case=TRUE) ~ "hospitalization",
    grepl('other', date_ranges_typeId, ignore.case=TRUE) ~ "other",
    grepl('isolation', date_ranges_typeId, ignore.case=TRUE) ~ "isolation"
  )) %>%
  mutate(pregnant = case_when(grepl('trimester', pregnancy_status, ignore.case=TRUE) ~ "Yes",
                              grepl('yes',questionnaire_answers_pregnant,ignore.case=TRUE)~ "Yes",
                              (sex=='Female' & age_years>=15) ~ 'No',
                              sex=='Male' ~ 'Not applicable',
                              age_years<15 ~ 'Not applicable',
                              age_years>50 ~ 'Not applicable')) %>% 
  #Symptoms
  #Cough
  mutate(cough=case_when(grepl('cough',questionnaire_answers_respiratory_symptoms_value,ignore.case=TRUE)~1,
                         grepl('yes', questionnaire_answers_cough,ignore.case=TRUE)~1)) %>% 
  #Rapid breathing 
  mutate(rapid_breathing=case_when(grepl('rapid breathing',questionnaire_answers_respiratory_symptoms_value,ignore.case=TRUE)~1,
                                   grepl('yes', questionnaire_answers_rapid_breathing,ignore.case=TRUE)~1)) %>%
  #Respiratory distress
  mutate(resp_distress=case_when(grepl('severe respiratory distress',questionnaire_answers_respiratory_symptoms_value,ignore.case=TRUE)~1,
                                 grepl('yes', questionnaire_answers_acute_respiratory_distress_syndrome_ards,ignore.case=TRUE)~1)) %>% 
  #Runny nose
  mutate(runny_nose=case_when(grepl('Runny nose',questionnaire_answers_respiratory_symptoms_value,ignore.case=TRUE)~1,
                              grepl('yes', questionnaire_answers_runny_nose,ignore.case=TRUE)~1)) %>% 
  #Fever
  mutate(fever=case_when(grepl('Fever',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                         grepl('yes', questionnaire_answers_fever,ignore.case=TRUE)~1)) %>% 
  #Loss of smell 
  mutate(loss_smell=case_when(grepl('Loss of smell',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                              grepl('yes', questionnaire_answers_loss_of_smell,ignore.case=TRUE)~1)) %>% 
  #Fatigue
  mutate(fatigue=case_when(grepl('Fatigue',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                           grepl('yes', questionnaire_answers_fatigue,ignore.case=TRUE)~1)) %>% 
  #Loss of appetite
  mutate(loss_appetite=case_when(grepl('Loss of appetite',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                                 grepl('yes', questionnaire_answers_loss_of_appetite,ignore.case=TRUE)~1)) %>% 
  #Sore throat
  mutate(sore_throat=case_when(grepl('Sore throat',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                               grepl('yes', questionnaire_answers_sore_throat,ignore.case=TRUE)~1)) %>% 
  #Diarrhoea
  mutate(diarrhoea=case_when(grepl('Diarrhoea',questionnaire_answers_symptoms_value,ignore.case=TRUE)~1,
                             grepl('yes', questionnaire_answers_diarrhoea,ignore.case=TRUE)~1)) %>% 
  #Pre-existing conditions
  #Hypertension
  mutate(htn=case_when(grepl('hypertension',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                       grepl('yes', questionnaire_answers_hypertension,ignore.case=TRUE)~1)) %>% 
  #Cardiovascular
  mutate(cardiovascular=case_when(grepl('cardiovascular',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                                  grepl('yes', questionnaire_answers_cardiovascular_disease,ignore.case=TRUE)~1)) %>% 
  #Diabetes
  mutate(diabetes=case_when(grepl('diabetes',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                            grepl('yes', questionnaire_answers_diabetes,ignore.case=TRUE)~1)) %>% 
  #Lung disease
  mutate(lung_disease=case_when(grepl('chronic lung',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                                grepl('yes', questionnaire_answers_chronic_lung_disease,ignore.case=TRUE)~1)) %>% 
  #Kidney disease
  mutate(kidney_disease=case_when(grepl('kidney',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                                  grepl('yes', questionnaire_answers_kidney_failure,ignore.case=TRUE)~1)) %>% 
  #Neurological disease
  mutate(neuro_disease=case_when(grepl('neuro',questionnaire_answers_select_pre_existing_medical_condition,ignore.case=TRUE)~1,
                                 grepl('yes', questionnaire_answers_chronic_neuro_condition,ignore.case=TRUE)~1)) %>% 
  mutate(any_comorb=case_when(htn==1 ~ 'Yes',
                              cardiovascular==1 ~ 'Yes',
                              diabetes==1 ~ 'Yes',
                              lung_disease==1 ~ 'Yes', 
                              kidney_disease==1 ~ 'Yes',
                              neuro_disease ==1~ 'Yes', 
                              TRUE ~ 'No')) %>% 
  mutate(any_comorb=factor(any_comorb, levels=c('Yes','No'))) %>% 
  #Time intervals
  unnest(questionnaire_answers_date_sample_collected,   keep_empty = TRUE, names_sep = "_") %>% 
  #Date of onset
  mutate(date_symptom_onset_final=ymd(date_of_onset)) %>% 
  #Date sample collected
  mutate(date_sample_collected=ymd_hms(questionnaire_answers_date_sample_collected_value)) %>% 
  mutate(date_sample_collected=as.Date(date_sample_collected)) %>% 
  mutate(onset_sample=as.integer(date_sample_collected-date_symptom_onset_final)) %>% 
  #Clean health facility variable
  mutate(health_facility_name=case_when(grepl('KTP', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'MSF / KTP Hospital',
                                        grepl('Leda', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'IOM Leda',
                                        grepl('Hope', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'Hope Foundation / Camp 4',
                                        grepl('BKL', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'MSF OCA BKL',
                                        grepl('5', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'UNHCR / Camp 5',
                                        grepl('8W', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'MSF / Camp 8 W',
                                        questionnaire_answers_health_facility_value=='MR ITC' ~ 'Main Road SARI ITC', 
                                        questionnaire_answers_health_facility_value=='MT ITC' ~ 'Main Road SARI ITC', 
                                        grepl('Main Road SARI ITC', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'Main Road SARI ITC',
                                        grepl('FH-MTI SARI ITC', questionnaire_answers_health_facility_value, ignore.case = T) ~ 'FH-MTI SARI ITC',
                                        TRUE ~ questionnaire_answers_health_facility_value)) %>% 
  #Contact with someone else
  mutate(contact_cov19=case_when(grepl('yes', questionnaire_answers_have_you_had_contact_with_an_anyone_with_suspected_or_confirmed_covid_19_infection, ignore.case=T) ~'Yes',
                                 grepl('no', questionnaire_answers_have_you_had_contact_with_an_anyone_with_suspected_or_confirmed_covid_19_infection, ignore.case=T)  ~'No',
                                 grepl('yes', questionnaire_answers_contact_with_patient, ignore.case=T) ~'Yes',
                                 grepl('no', questionnaire_answers_contact_with_patient, ignore.case=T)  ~'No')) %>% 
  mutate(contact_cov19=factor(contact_cov19, levels=c('Yes', 'No'))) %>% 
  #Smoking history
  mutate(social_event=case_when(grepl('yes', questionnaire_answers_did_the_patient_go_to_any_social_event_party_mass_gathering_religious_services, ignore.case=T) ~'Yes',
                                grepl('no', questionnaire_answers_did_the_patient_go_to_any_social_event_party_mass_gathering_religious_services, ignore.case=T)  ~'No',
                                grepl('yes', questionnaire_answers_attended_occasion, ignore.case=T) ~'Yes',
                                grepl('no', questionnaire_answers_attended_occasion, ignore.case=T)  ~'No')) %>% 
  mutate(social_event=factor(social_event, levels=c('Yes', 'No'))) 


# EPICURVE-GENDER -------------------------------------------------------------------

godata_epi_curve <- godata_clean %>% 
  mutate(week=isoweek(date_symptom_onset_final)) %>% 
  count(week, sex) %>% 
  ggplot(.) +
  geom_col(aes(x = week, y = n, fill=sex)) +
  # scale_x_date(date_breaks = '14 day', date_minor_breaks = '3 day',
  #              date_labels = '%d-%m') +
  theme_minimal() +
  scale_fill_brewer(palette="Dark2", na.value="blue") +
  labs(x = "Week of symptom onset",
       y = "Number of cases", fill='')


# EPICURVE-AGE ------------------------------------------------------------

case_age <- godata_clean %>% 
  count(age_group, .drop = FALSE) %>% 
  mutate(indicator='Cases')

max_cases <- max(case_age$n)

death_age <- godata_clean %>% 
  filter(outcome=='Dead') %>% 
  count(age_group, .drop = FALSE) %>% 
  mutate(indicator='Deaths')

case_death_age_gph <-  case_age %>% 
  bind_rows(death_age) %>% 
  ggplot(.,
         aes(x = age_group,
             y=n,
             #y = ifelse(sex == 'male', n, -n),
             fill = indicator)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Age group", y = "Count", fill = '') +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0,max_cases,2)) +
  scale_fill_brewer(palette="Dark2") +
  geom_text(
    aes(label = ifelse(n>0,n,''), y = n + 0.05),
    position = position_dodge(0.7),
    vjust = 0
  ) 



# TABLE-1 -----------------------------------------------------------------

label(godata_clean$age_years) <- "Age"
label(godata_clean$sex) <- "Sex"
label(godata_clean$outcome) <- "Outcome"
label(godata_clean$questionnaire_answers_travel_outside_camps) <- "Travel outside camp (last 14 days)"
label(godata_clean$status) <- "Status"
#label(godata_clean$risk_level) <- "Risk level"
label(godata_clean$onset_sample) <- "Symptom onest to sample taken (days)"
label(godata_clean$health_facility_name) <- "Health facility"
label(godata_clean$outcome) <- "Patient outcome"
label(godata_clean$contact_cov19) <- "Did the case have contact with a suspected or confirmed COVID-19 case?"
label(godata_clean$any_comorb) <- "Did the case report a pre-existing condition?"
label(godata_clean$detection_status) <- "Status at detection"
label(godata_clean$social_event) <- "Did the case attend a social event/mass gathering before developing symptoms?"
label(godata_clean$any_comorb) <- "Did the case report at least 1 co-morbidity?"


godata_table <- table1(~ age_years  + sex  + 
         detection_status +
         outcome + any_comorb + contact_cov19 + social_event
       , data=godata_clean, topclass="Rtable1-zebra",footnote="Data source: GoData") 



# SYMPTOMS ----------------------------------------------------------------

symptom_gph <- godata_clean %>% 
  #filter(population_group=='Rohingya refugee/FDMN') %>% 
  #filter(test_result=='Positive') %>% 
  select(id, cough, rapid_breathing, resp_distress, runny_nose, fever,loss_smell,fatigue,loss_appetite,sore_throat,diarrhoea) %>% 
  pivot_longer(-id) %>% 
  filter(value==1) %>% 
  arrange(id, name) %>% 
  mutate(name=case_when(name=='runny_nose' ~ 'Runny nose',
                        name=='rapid_breathing' ~ 'Rapid breathing',
                        name=='resp-distress' ~ 'Respiratory distress',
                        name=='fever' ~ 'Fever', 
                        name=='cough' ~ 'Cough', 
                        name=='fatigue' ~ 'Fatigue',
                        name=='loss_appetite' ~ 'Loss of appetite',
                        name=='sore_throat' ~ 'Sore throat', 
                        name=='diarrhoea' ~ 'Diarrhoea')) %>% 
  select(-value) %>% 
  group_by(id) %>%
  summarize(symptom_list = list(name)) %>% 
  ggplot(aes(x=symptom_list)) +
  geom_bar() +
  theme_bw() +
  labs(x = "",
       y = "Number of cases",
       title="Distribution of symptoms at presentation",
       caption="Data source: GoData") +
  scale_x_upset(n_intersections = 5) +
  scale_y_continuous(breaks= pretty_breaks())
