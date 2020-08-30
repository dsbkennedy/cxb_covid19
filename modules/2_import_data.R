## ---- google-sheets --------

##Import data from Google Sheets

gs4_deauth()
# #plan(multiprocess)
gsheet_data <- map(sheet_names, ~read_sheet(gdrive_link, sheet=.)) %>%
  set_names(sheet_names)

##Import FDMN data 
fdmn_raw <- gsheet_data$fdmn %>% 
  clean_names() %>% 
  janitor::remove_empty() %>% 
  mutate(date_of_death=janitor::excel_numeric_to_date(date_of_death)) %>% 
  #select(-date_of_death) %>% 
  mutate(camp_of_residence=as.character(camp_of_residence)) %>%
  mutate(nationality='FDMN')

##Import host data 
host_raw <- gsheet_data$host %>% 
  clean_names() %>% 
  janitor::remove_empty() %>% 
  mutate(nationality='Host')

##Bind FDMN and host data
all_cases_raw <- fdmn_raw %>% 
  bind_rows(host_raw)

#Find all date columns
all_date_cols <- grep("(date|created_at| updated_on)", names(all_cases_raw))

#Process linelist data
all_cases_linelist <- all_cases_raw %>% clean_dates( force_Date  = all_date_cols,
                                                     guess_dates = all_date_cols) %>% 
  mutate(population_group=case_when(grepl('host', nationality, ignore.case=T)~ 'Host community',
                                    grepl('fdmn', nationality, ignore.case=T) ~ 'Rohingya refugee/FDMN')) %>% 
  mutate(upazilla=case_when(upazilla=='CXB Sadar' ~ 'Sadar', 
                            TRUE~upazilla)) %>% 
  mutate(date_of_case_detection=coalesce(date_of_case_detection, date_of_lab_result_received))
  #filter(!(is.na(population_group))) 


## Testing data
tests_data <- gsheet_data$testing %>% 
  clean_names() 

test_nationality <- tests_data %>% 
  filter(!date %in% c('NULL', 'Total')) %>% 
  mutate(date_format=excel_numeric_to_date(as.numeric(date))) %>% 
  select(-date) %>% 
  pivot_longer(-date_format) %>% 
  mutate(population_group=case_when(grepl('fdmn',name) ~ 'Rohingya refugee/FDMN',
                                    grepl('host',name) ~ 'Host community')) 


##DRU data
dru_raw <- gsheet_data$dru %>% clean_names() %>%   mutate(facility_name=gsub(":([[:alpha:]])", ": \\1", facility_name)) 
quarantine_raw <- gsheet_data$quarantine %>% clean_names() %>% filter(!is.na(facility_name))


### Camp population file
population <- read.csv(here('data', 'population.csv')) 

## ---- godata --------

### GoData

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


#Save populations as objects for processing later on 
host_population <-  2805491
fdmn_population <-  859205 

