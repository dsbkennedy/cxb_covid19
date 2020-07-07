

# Google data -------------------------------------------------------------


#sheet_names <- c('host_fdmn_append', 'ari_ili')
#sheet_names <- c('host_fdmn_append',  'testing')
#sheet_names <- c('host_fdmn_append',  'testing', 'dru', 'quarantine')
sheet_names <- c('Confirm_FDMN')
gdrive_link <- "12qtWMq-tTkYO7y9h-dqt3cFDyMawsgEaCSNQAQb5fHs"

#plan(multiprocess)
library(purrr)
library(googlesheets4)
library(here)

gs4_deauth()
gsheet_data <- map(sheet_names, ~read_sheet(gdrive_link, sheet=.)) %>%  set_names(sheet_names)


# GoData ------------------------------------------------------------------


library(lubridate)
library(data.table)
library(dplyr)
library(tibble)
library(tidyr)
library(httr)
library(jsonlite)
library(magrittr)
library(tibble)
url <- "https://godata-r4.who.int/"                   # <--------------------- insert instance url here, don't forget the slash at end !
username <- "david.kennedy@lshtm.ac.uk"                           # <--------------------- insert your username for signing into Go.Data webapp here
password <- "tEssAmAIsIE150!"                           # <--------------------- insert your password for signing into Go.Data webapp here
outbreak_id <- "9116fb0c-2b2e-424f-9252-accaf574e785"   # <--------------------- insert your outbreak ID here

#get access token
url_request <- paste0(url,"api/oauth/token?access_token=123")

response <- POST(url=url_request,
                 body = list(
                   username = username,
                   password = password),
                 encode = "json")

content <-
  content(response, as = "text") %>%
  fromJSON(flatten = TRUE)

access_token <- content$response$access_token                 ## this is your access token !!! that allows API calls

#specify date ranges, for follow up filters
date_now <- format(Sys.time(), "%Y-%m-%dT23:59:59.999Z")
date_21d_ago <- format((Sys.Date()-21), "%Y-%m-%dT23:59:59.999Z")

# import outbreak Cases
response_cases <- GET(paste0(url,"api/outbreaks/",outbreak_id,"/cases"),
                      add_headers(Authorization = paste("Bearer", access_token, sep = " "))
)
json_cases <- content(response_cases, as = "text")


cases <- as_tibble(fromJSON(json_cases, flatten = TRUE)) 
  #select(-c(firstName,middleName,lastName, addresses))

age_labs <- c(paste(seq(0, 60, by = 10), seq(9, 69, by = 10),
                    sep = "-"), paste(70, "+", sep = ""))

godata_wide <- cases %>%
  clean_names() %>% 
  filter(deleted == FALSE) %>%
  unnest(c(
    #addresses,
    date_ranges,
    #questionnaire_answers_nationality,
    #questionnaire_answers_nationality_2,
    #questionnaire_answers_cif_nationality,
    contains('nationality'),
    contains('result'),
    contains('symptom'),
    # questionnaireAnswers.numeroIDcasIndex,
    # questionnaireanswers_liendeparente
  ),
  keep_empty = TRUE, names_sep = "_") %>% 
  remove_empty(c("rows", "cols")) 


godata_clean <- godata_wide %>% 
  #Ethnicity
  mutate(population_group=coalesce(questionnaire_answers_nationality_value,questionnaire_answers_nationality_2_value,questionnaire_answers_cif_nationality_value)) %>% 
  filter(population_group=='FDMN')  %>% 
  #Test result
  mutate(lab_result=coalesce(questionnaire_answers_lab_result_value,questionnaire_answers_result_2_value)) %>% 
  filter(lab_result=='Positive') %>% 
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
  mutate(age_group=cut(age_years,breaks = c(seq(0, 70, by = 10), Inf), labels = age_labs, right = FALSE)) %>%
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
    grepl('_male', gender, ignore.case=TRUE) ~ "male",
    grepl('_female', gender, ignore.case=TRUE) ~ "female"
  )) %>%
  #Updated this classification to match survey
  #updated to "outcome"
  mutate(outcome = case_when(
    grepl('alive', outcome_id, ignore.case=TRUE) ~ "alive",
    grepl('deceased', outcome_id, ignore.case=TRUE) ~ "dead",
    grepl('recovered', outcome_id, ignore.case=TRUE) ~ "recovered",
    grepl('death', questionnaire_answers_outcome, ignore.case=TRUE) ~ "dead"
  )) %>%
  mutate(outcome=fct_explicit_na(outcome, na_level = "(Outcome missing)"))  %>% 
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
  mutate(pregnant = case_when(grepl('trimester', date_ranges_typeId, ignore.case=TRUE) ~ "pregnant"
  )) %>%
  mutate(pregnant=case_when(grepl('yes',questionnaire_answers_pregnant,ignore.case=TRUE)~1)) %>% 
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
  #Time intervals
  unnest(questionnaire_answers_date_sample_collected,   keep_empty = TRUE, names_sep = "_") %>% 
  #Date of onset
  mutate(date_symptom_onset_final=ymd(date_of_onset)) %>% 
  #Date sample collected
  mutate(date_sample_collected=ymd_hms(questionnaire_answers_date_sample_collected_value)) %>% 
  mutate(date_sample_collected=as.Date(date_sample_collected)) %>% 
  mutate(onset_sample=as.integer(date_sample_collected-date_symptom_onset_final))





# Building data -----------------------------------------------------------

#Want to combine GoData and Google Sheet data 

#42 records in GOdata
fdmn_linelist <- gsheet_data$Confirm_FDMN %>% 
  filter(Nationality=='FDMN') %>% 
  clean_data() %>% 
  setNames(paste0('gsheet.', names(.))) %>% 
  mutate(join_name=gsheet.name) %>% 
  #Clean names
  mutate(join_name=case_when(join_name=='mokaroma' ~ 'mokarrama', 
                             join_name=='sumi_akter' ~ 'sumi',
                             join_name=='monir_ahmed' ~ 'monir',
                             join_name=='toyoba_begum' ~ 'toyoba',
                             join_name=='jubayer' ~ 'md_jubair',
                             join_name=='kamal_hossain' ~ 'kamal_husson',
                             join_name=='ziaur_rahman' ~ 'jiaur_rahaman',
                             join_name=='abu_siddik' ~ 'abu_siddique',
                             join_name=='md_rafiq' ~ 'md_rafiqul_islam',
                             join_name=='noshima' ~ 'nasima',
                             join_name=='md_alam' ~ 'mohammad_alom',
                               TRUE ~ join_name))
  
  
  mokaroma


#What can we use to join data? 

names(fdmn_linelist)

name_match <- godata_clean %>% 
  clean_data() %>% 
  setNames(paste0('godata.', names(.))) %>% 
  inner_join(fdmn_linelist, by=c('godata.first_name'='gsheet.name')) %>% 
  select(godata.visual_id, godata.first_name, gsheet.date_of_case_detection, godata.date_become_case, godata.date_of_reporting, godata.age_years,gsheet.age_in_years, godata.sex, gsheet.sex) %>% 
  mutate(age_match=case_when(godata.age_years==gsheet.age_in_years~'Y', 
                             TRUE ~ 'N')) %>% 
  mutate(sex_match=case_when(godata.sex=='male' & gsheet.sex=='m' ~ 'Y', 
                             godata.sex=='female' & gsheet.sex=='f' ~ 'Y', 
                             TRUE ~ 'N'))


# library(statar)
# name_match %>%  tab(age_match, sex_match)
# 
# 
# name_antimatch <- godata_clean %>% 
#   clean_data() %>% 
#   setNames(paste0('godata.', names(.))) %>% 
#   anti_join(fdmn_linelist, by=c('godata.first_name'='gsheet.name')) 
# 
#   select(godata.visual_id, godata.first_name,  godata.age_years,gsheet.age_in_years, godata.sex, gsheet.sex) 
#   

  library(fuzzyjoin)
 fuzzy_match <- godata_clean %>% 
    clean_data() %>% 
    setNames(paste0('godata.', names(.))) %>% 
    mutate(join_name=godata.first_name) %>% 
   #stringdist_anti_join(fdmn_linelist, by = 'join_name', max_dist=1) 
   stringdist_full_join(fdmn_linelist, by = 'join_name', max_dist=1) %>% 
    select(godata.visual_id,gsheet.si_no, godata.first_name,gsheet.name, gsheet.date_of_case_detection, godata.date_become_case, godata.date_of_reporting, godata.age_years,gsheet.age_in_years, godata.sex, gsheet.sex) %>% 
   mutate(age_match=case_when(godata.age_years==gsheet.age_in_years~'Y', 
                              TRUE ~ 'N')) %>% 
   mutate(sex_match=case_when(godata.sex=='male' & gsheet.sex=='m' ~ 'Y', 
                              godata.sex=='female' & gsheet.sex=='f' ~ 'Y', 
                              TRUE ~ 'N'))
    
  
 match_df <- fuzzy_match %>% 
   filter(age_match=='Y' & sex_match=='Y') %>% 
   select(godata.visual_id, gsheet.si_no) %>% 
   rename(godata_caseID=godata.visual_id, 
          si_no=gsheet.si_no) %>% 
   arrange(si_no)
 
 write.csv(match_df, here('data', 'match_rec.csv'))
 
 
 fuzzy_match %>% 
   select()
 
  
  
    #fuzzy_inner_join(fdmn_linelist, by=c('godata.first_name'='gsheet.name')) 
    fuzzy_join(x, y, exact = "a", fuzzy = "b")  





