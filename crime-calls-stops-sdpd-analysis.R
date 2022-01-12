# Libraries
library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(rgdal)
library(readxl)

# Set working directory
setwd("~/Desktop/crime-calls-stops-sdpd")

###################################################################
###################### IMPORT RIPA DATA ###########################
###################################################################

library(readr)
master_2019 <- read_csv("master-ripa-2019.csv", 
                        col_types = cols(...1 = col_skip(), ldmk = col_character(), 
                                         hw_exit = col_character()))

master_2020 <- read_csv("master-ripa-2020.csv", 
                        col_types = cols(...1 = col_skip(), ldmk = col_character(), 
                                         hw_exit = col_character()))

master_2021 <- read_csv("master-ripa-2021.csv", 
                        col_types = cols(...1 = col_skip(), ldmk = col_character(), 
                                         hw_exit = col_character()))

# Append to make final master
master <- rbind(master_2019, master_2020, master_2021)

remove(master_2019, master_2020, master_2021)

###################################################################
###################### IMPORT BEAT SHAPE ##########################
###################################################################

# Import beat shapes into Global Environment
library(rgdal)
beat_shape <- readOGR(dsn = "pd_beats_datasd",
                      layer = "pd_beats_datasd")

#Change the projection so the CRS comes from the polygon objects
beat_shape <- spTransform(beat_shape, CRS("+proj=longlat +ellps=sphere +no_defs"),
                          use_ob_tran=TRUE)

###################################################################
###################### IMPORT 911 DATA ############################
###################################################################

# 2019 call data
calls19 <- read_csv("pd_calls_for_service_2019_datasd.csv", 
                    col_types = cols(address_dir_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character()))

# 2020 call data
calls20 <- read_csv("pd_calls_for_service_2020_datasd.csv", 
                    col_types = cols(address_dir_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character()))

# 2021 call data
calls21 <- read_csv("pd_calls_for_service_2021_datasd.csv", 
                    col_types = cols(address_dir_intersecting = col_character(), 
                                     address_sfx_intersecting = col_character()))

# Merge
calls <- rbind(calls19, calls20, calls21)
## New row count == 1,601,810

remove(calls19, calls20, calls21)

# Split date and time column
## Create date column
calls$date <- as.Date(calls$date_time)

# Create time column
calls$time <- format(calls$date_time,"%H:%M:%S")

# See how many calls occurred during desired date
## To be filtered below
calls %>% 
  filter(date >= "2019-01-01" & date <= "2021-06-30") %>% 
  count(n())
# 1,447,625

# Filter to time frame of RIPA data
## 1/1/2019 through 6/30/2021
calls <- calls[calls$date >= "2019-01-01" & calls$date <= "2021-06-30",]
## New row count == 1,447,625

# Import disposition dictionary
disp <- read_csv("pd_dispo_codes_datasd.csv")

# Rename columns for merge
names(disp) <- c("disposition", "disp_desc")

# Left join with calls
calls <- left_join(calls, disp, by = "disposition")

remove(disp)

###################################################################
##################### IMPORT CRIME DATA ###########################
###################################################################

# Import crime data
library(readxl)
crime <- read_excel("PRA 21-4922_2019-2021_Crimes__UCR Revised.xlsx", 
                    col_types = c("text", "text", "date", 
                                  "numeric", "text", "text", "text", 
                                  "text", "text", "text", "text", "text", 
                                  "text", "text", "text", "numeric"))
# Row count == 144,458

# Separate out date and time
crime$date <- as.Date(crime$activityDate, "%m/%d/%Y")
crime$time <- format(crime$activityDate, format = "%H:%M:%S")

# Remove original activity date
crime <- crime %>% 
  select(-activityDate)

# Check for duplicates
n_distinct(crime$activityNumber)
# 144,458
## No duplicates

# See how many crimes occurred during desired date
## To be filtered below
crime %>% 
  filter(date >= "2019-01-01" & date <= "2021-06-30") %>% 
  count(n())
# 131,983

# Filter to time frame of RIPA data
## 1/1/2019 through 6/30/2021
crime <- crime[crime$date >= "2019-01-01" & crime$date <= "2021-06-30",]
## New row count == 131,983

###################################################################
####################### CLEAN CRIME ###############################
###################################################################

# Remove leading and trailing whitespace
library(stringr)
crime <- crime %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
crime <- crime %>% 
  mutate_if(is.character, str_squish)

# Make all letters in violationSection uppercase
library(base)
crime$violationSection <- toupper(crime$violationSection)

# Import in crime_desc with part 1 or 2 classifications
## Created by SDUT manually, using CA legislature description of part 1 crimes:
## https://leginfo.legislature.ca.gov/faces/codes_displaySection.xhtml?sectionNum=667.5.&lawCode=PEN
desc <- read_csv("crime_desc_part1_cats.csv", 
                 col_types = cols(codeUcr = col_character(), 
                                  count = col_skip()))

# Create column in crime to merge with desc
crime$comb <- paste0(crime$chargeDescription, " - ", crime$codeUcr)

# Left join with desc
crime <- left_join(crime, desc, by = "comb")

# Remove columns we don't need
names(crime)
crime <- crime %>% 
  select(-chargeDescription.y, -codeUcr.y)

# Rename columns
crime <- rename(crime, chargeDescription = chargeDescription.x)
crime <- rename(crime, codeUcr = codeUcr.x)

remove(desc)

###################################################################
####################### CLEAN CALLS ###############################
###################################################################

# Remove leading and trailing whitespace
calls <- calls %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
calls <- calls %>% 
  mutate_if(is.character, str_squish)

# Remove date_time
calls <- calls %>% 
  select(-date_time)

###################################################################
##################### CLEAN RIPA ##################################
###################################################################

# Remove leading and trailing whitespace
master <- master %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
master <- master %>% 
  mutate_if(is.character, str_squish)

# Remove floating commas
master$reason_exp <- gsub(" , ", ", ", master$reason_exp)
master$search_basis_exp <- gsub(" , ", ", ", master$search_basis_exp)

## Count number of characters in time string
library(stringi)
master$count <- nchar(stri_escape_unicode(master$time))

table(master$count)
#      8     19 
#  407592    92

# View rows with 19 characters in time
test <- master %>% 
  filter(count == 19)

# Some times have incorrect / computer generated dates attached
## gsub out the dates
master$time <- gsub("1900-01-01 ", "", master$time)
master$time <- gsub("1899-12-30 ", "", master$time)

# Create second time column that's in time format
library(chron)
master$time2 <- times(master$time)

# Unload chron, to avoid masking with lubridate
detach("package:chron", unload=TRUE)

# Remove count column
master <- master %>% 
  select(-count)

remove(test)

###################################################################
####################### CLEAN RACE ################################
###################################################################

# Create simplified column for race, written out
master <- master %>% 
  mutate(race_simp = str_replace_all(race, "White", "white") %>% 
           str_replace_all("Pacific Islander", "pi") %>% 
           str_replace_all("Native American", "nam") %>% 
           str_replace_all("Middle Eastern or South Asian", "me_sa") %>% 
           str_replace_all("Hispanic/Latino/a", "hisp") %>% 
           str_replace_all("Black/African American", "black") %>% 
           str_replace_all("Asian", "asian"))

# Create new race category
## Hispanic + other race == "hisp"
## More than one race (but not hisp) == "mixed"
master <- master %>% 
  mutate(race_condensed = case_when(str_detect(race_simp, "hisp") ~ "hisp", # if contains "hisp", add to hispanic category
                                    str_detect(race_simp, "\\|") ~ "mixed", # if contains "|", create mixed category
                                    TRUE ~ race_simp)) # if neither above is true, paste original from race_words

# Remove original race column
master <- master %>% 
  select(-race)

# Create race column descriptions for easy calculations
master$asian <- ifelse(grepl("asian", master$race_condensed), 1, 0)
master$black <- ifelse(grepl("black", master$race_condensed), 1, 0)
master$hisp <- ifelse(grepl("hisp", master$race_condensed), 1, 0)
master$me_sa <- ifelse(grepl("me_sa", master$race_condensed), 1, 0)
master$mixed <- ifelse(grepl("mixed", master$race_condensed), 1, 0)
master$nam <- ifelse(grepl("nam", master$race_condensed), 1, 0)
master$pi <- ifelse(grepl("pi", master$race_condensed), 1, 0)
master$white <- ifelse(grepl("white", master$race_condensed), 1, 0)

###################################################################
###################### CLEAN STOP REASON ##########################
###################################################################

# Create new column for reason of stop simplified
## Only one reason is listed in this column, no multiple entries
master <- master %>% 
  mutate(reason_simp = str_replace_all(reason_words, "Determine whether the student violated school policy", "rs_school") %>% 
           str_replace_all("Possible conduct warranting discipline under Education Code sections 48900, 48900.2, 48900.3, 48900.4 and 48900.7", "rs_ed") %>%
           str_replace_all("Consensual Encounter resulting in a search", "rs_consent") %>%
           str_replace_all("Investigation to determine whether the person was truant", "rs_truant") %>%
           str_replace_all("Knowledge of outstanding arrest warrant/wanted person", "rs_warrant") %>%
           str_replace_all("Known to be on Parole / Probation / PRCS / Mandatory Supervision", "rs_parole") %>% 
           str_replace_all("Reasonable Suspicion", "rs_susp") %>% 
           str_replace_all("Traffic Violation", "rs_traff"))

# Create final "grouped" reason id column
master$reason_condensed <- ifelse(master$reason_simp == "rs_traff", "TRAFFIC",
                                  ifelse(master$reason_simp == "rs_susp", "SUSP",
                                         ifelse(master$reason_simp == "rs_school", "OTHER",
                                                ifelse(master$reason_simp == "rs_ed", "OTHER",
                                                       ifelse(master$reason_simp == "rs_consent", "OTHER",
                                                              ifelse(master$reason_simp == "rs_truant", "OTHER",
                                                                     ifelse(master$reason_simp == "rs_warrant", "OTHER",
                                                                            ifelse(master$reason_simp == "rs_parole", "OTHER", "CHECK"))))))))

###################################################################
###################### CLEAN SEARCH BASIS #########################
###################################################################

# Change character string of NA in search_basis to real NA
master$search_basis[master$search_basis == "NA"] = NA

# Create is_searched column check
master$is_searched <- ifelse(!is.na(master$search_basis),1,0)

# Create new column for search basis type, simplified
master <- master %>% 
  mutate(search_basis_simp = str_replace_all(search_basis, "Suspected violation of school policy", "sch_school") %>% 
           str_replace_all("Vehicle inventory", "sch_inventory") %>%
           str_replace_all("Exigent circumstances/emergency", "sch_emerg") %>%
           str_replace_all("Incident to arrest", "sch_arrest") %>%
           str_replace_all("Consent given", "sch_consent") %>%
           str_replace_all("Officer Safety/safety of others", "sch_safety") %>% 
           str_replace_all("Search Warrant", "sch_warrant") %>% 
           str_replace_all("Condition of parole / probation/ PRCS / mandatory supervision", "sch_parole") %>% 
           str_replace_all("Suspected weapons", "sch_susp_weapons") %>% 
           str_replace_all("Visible contraband", "sch_vis_cont") %>% 
           str_replace_all("Odor of contraband", "sch_od_cont") %>% 
           str_replace_all("Canine detection", "sch_k9") %>% 
           str_replace_all("Evidence of crime", "sch_crime"))

# Create columns for search basis descriptions
master$sch_consent <- ifelse(grepl("sch_consent", master$search_basis_simp), 1, 0)
master$sch_safety <- ifelse(grepl("sch_safety", master$search_basis_simp), 1, 0)
master$sch_warrant <- ifelse(grepl("sch_warrant", master$search_basis_simp), 1, 0)
master$sch_parole <- ifelse(grepl("sch_parole", master$search_basis_simp), 1, 0)
master$sch_susp_weapons <- ifelse(grepl("sch_susp_weapons", master$search_basis_simp), 1, 0)
master$sch_vis_cont <- ifelse(grepl("sch_vis_cont", master$search_basis_simp), 1, 0)
master$sch_od_cont <- ifelse(grepl("sch_od_cont", master$search_basis_simp), 1, 0)
master$sch_k9 <- ifelse(grepl("sch_k9", master$search_basis_simp), 1, 0)
master$sch_crime <- ifelse(grepl("sch_crime", master$search_basis_simp), 1, 0)
master$sch_arrest <- ifelse(grepl("sch_arrest", master$search_basis_simp), 1, 0)
master$sch_emerg <- ifelse(grepl("sch_emerg", master$search_basis_simp), 1, 0)
master$sch_inventory <- ifelse(grepl("sch_inventory", master$search_basis_simp), 1, 0)
master$sch_school <- ifelse(grepl("sch_school", master$search_basis_simp), 1, 0)

###################################################################
######################## DISCRETION ###############################
###################################################################

# RIPA categorized search basis into higher discretion and lower discretion
## Experts suggested (in interviews) that the U-T categorize search bases 
## into "discretionary" and "non-discretionary"

# Create discretionary column
## Since there can be more than one "search basis", 
## create check if any of these reasons were selected
master$discretionary <- ifelse(master$sch_consent == 1, 1,
                               ifelse(master$sch_safety == 1, 1,
                                      ifelse(master$sch_parole == 1, 1,
                                             ifelse(master$sch_susp_weapons == 1, 1,
                                                    ifelse(master$sch_vis_cont == 1, 1,
                                                           ifelse(master$sch_od_cont == 1, 1,
                                                                  ifelse(master$sch_k9 == 1, 1,
                                                                         ifelse(master$sch_crime == 1, 1,
                                                                                ifelse(master$sch_emerg == 1, 1,
                                                                                       ifelse(master$sch_school == 1, 1, 0))))))))))

# Create non_discretionary column
## Since there can be more than one "search basis",
## create check if any of these reasons were selected
master$non_discretionary <- ifelse(master$sch_warrant == 1, 1,
                                   ifelse(master$sch_arrest == 1, 1,
                                          ifelse(master$sch_inventory == 1, 1, 0)))

# There are some that are both discretionary and non_discretionary, due to multiple search bases
## Remove 1's from discretionary if there's also a non_discretionary reason for the search
### Since non_discretionary searches will always take place, regardless of other circumstances
master$discretionary <- ifelse(master$discretionary == 1 & master$non_discretionary == 1, 0, 
                               master$discretionary)

###################################################################
###################### CLEAN CONTRABAND ###########################
###################################################################

# Create new column for contraband type found during search, simplified
master <- master %>% 
  mutate(cont_simp = str_replace_all(cont, "Other Contraband or evidence", "cont_other") %>% 
           str_replace_all("Cell phone\\(s\\) or electronic device\\(s\\)", "cont_cell") %>%
           str_replace_all("Suspected Stolen property", "cont_stolen_prop") %>%
           str_replace_all("Drug Paraphernalia", "cont_para") %>%
           str_replace_all("Money", "cont_money") %>%
           str_replace_all("Alcohol", "cont_alcohol") %>% 
           str_replace_all("Drugs/narcotics", "cont_drugs") %>% 
           str_replace_all("Weapon\\(s\\) other than a firearm", "cont_weapons") %>% 
           str_replace_all("Ammunition", "cont_ammu") %>% 
           str_replace_all("Firearm\\(s\\)", "cont_firearm") %>% 
           str_replace_all("None", "cont_none"))

# Create columns for contraband descriptions
master$cont_other <- ifelse(grepl("cont_other", master$cont_simp), 1, 0)
master$cont_cell <- ifelse(grepl("cont_cell", master$cont_simp), 1, 0)
master$cont_stolen_prop <- ifelse(grepl("cont_stolen_prop", master$cont_simp), 1, 0)
master$cont_para <- ifelse(grepl("cont_para", master$cont_simp), 1, 0)
master$cont_money <- ifelse(grepl("cont_money", master$cont_simp), 1, 0)
master$cont_alcohol <- ifelse(grepl("cont_alcohol", master$cont_simp), 1, 0)
master$cont_drugs <- ifelse(grepl("cont_drugs", master$cont_simp), 1, 0)
master$cont_weapons <- ifelse(grepl("cont_weapons", master$cont_simp), 1, 0)
master$cont_ammu <- ifelse(grepl("cont_ammu", master$cont_simp), 1, 0)
master$cont_firearm <- ifelse(grepl("cont_firearm", master$cont_simp), 1, 0)
master$cont_none <- ifelse(grepl("cont_none", master$cont_simp), 1, 0)

# Create column that aggregates drugs, weapons and ammunition
master$drugs_weapons <- ifelse(master$cont_drugs == 1, 1, 
                               ifelse(master$cont_weapons == 1, 1,
                                      ifelse(master$cont_firearm == 1, 1,
                                             ifelse(master$cont_ammu == 1, 1, 0))))

###################################################################
######################### CLEAN ACTIONS ###########################
###################################################################

# Create new column for actions, simplified
master <- master %>% 
  mutate(act_simp = str_replace_all(actions, "None", "act_none") %>% 
           str_replace_all("Admission or written statement obtained from student", "act_student") %>%
           str_replace_all("Vehicle impounded", "act_vi") %>%
           str_replace_all("Property was seized", "act_prop_seiz") %>%
           str_replace_all("Search of property was conducted", "act_sch_prop") %>%
           str_replace_all("Asked for consent to search property", "act_req_sch_prop") %>% 
           str_replace_all("Search of person was conducted", "act_sch_pers") %>% 
           str_replace_all("Asked for consent to search person", "act_req_sch_pers") %>% 
           str_replace_all("Person photographed", "act_photo") %>%
           str_replace_all("Physical or Vehicle contact", "act_physical") %>%
           str_replace_all("Chemical spray used", "act_chem") %>%
           str_replace_all("Baton or other impact weapon used", "act_baton") %>%
           str_replace_all("Canine bit or held person", "act_k9_bit") %>%
           str_replace_all("Impact projectile discharged or used", "act_ip") %>%
           str_replace_all("Electronic control device used", "act_elect") %>%
           str_replace_all("Firearm discharged or used", "act_fad") %>%
           str_replace_all("Firearm pointed at person", "act_fp") %>%
           str_replace_all("Canine removed from vehicle or used to search", "act_k9_rem") %>%
           str_replace_all("Patrol car detention", "act_car_det") %>%
           str_replace_all("Handcuffed or flex cuffed", "act_hc") %>%
           str_replace_all("Curbside detention", "act_curb") %>%
           str_replace_all("Field sobriety test conducted", "act_sober") %>%
           str_replace_all("Person removed from vehicle by physical contact", "act_rem_cont") %>%
           str_replace_all("Person removed from vehicle by order", "act_rem_order"))

# Create separate column for action detained category
master$act_detained <- ifelse(grepl("act_car_det", master$act_simp), 1, 
                              ifelse(grepl("act_hc", master$act_simp), 1,
                                     ifelse(grepl("act_curb", master$act_simp), 1, 0)))

# Create separate column for action force category
master$act_force <- ifelse(grepl("act_chem", master$act_simp), 1, 
                           ifelse(grepl("act_baton", master$act_simp), 1,
                                  ifelse(grepl("act_k9_bit", master$act_simp), 1,
                                         ifelse(grepl("act_ip", master$act_simp), 1, 
                                                ifelse(grepl("act_elect", master$act_simp), 1,
                                                       ifelse(grepl("act_fad", master$act_simp), 1,
                                                              ifelse(grepl("act_fp", master$act_simp), 1,
                                                                     ifelse(grepl("act_rem_cont", master$act_simp), 1,
                                                                            ifelse(grepl("act_physical", master$act_simp), 1,0)))))))))

# Create separate column for action none category
master$act_none <- ifelse(grepl("act_none", master$act_simp), 1, 0)

# Create separate column for action other category
master$act_other <- ifelse(grepl("act_k9_rem", master$act_simp), 1, 
                           ifelse(grepl("act_stud", master$act_simp), 1,
                                  ifelse(grepl("act_photo", master$act_simp), 1,
                                         ifelse(grepl("act_sober", master$act_simp), 1,
                                                ifelse(grepl("act_rem_order", master$act_simp), 1, 0)))))

# Create separate column for action search requested category
master$act_req_search <- ifelse(grepl("act_req_sch_prop", master$act_simp), 1, 
                                ifelse(grepl("act_req_sch_pers", master$act_simp), 1, 0))

# Create separate column for action search conducted category
master$act_search <- ifelse(grepl("act_sch_prop", master$act_simp), 1, 
                            ifelse(grepl("act_sch_pers", master$act_simp), 1, 0))

# Create separate column for action seize category
master$act_seize <- ifelse(grepl("act_vi", master$act_simp), 1, 
                           ifelse(grepl("act_prop_seize", master$act_simp), 1, 0))

###################################################################
######################## POPULATION ###############################
###################################################################

# Import population/demographics of beat
pop <- read_csv("beat_demographics_2019_acs.csv")

# Create other category since it's not in original data
pop <- pop %>% 
  mutate(other = total - (white + black + aian + asian + nhopi + hisp))

# Aggregate new other with aian and nhopi
pop <- pop %>% 
  mutate(other2 = aian + nhopi + other)

# Remove columns we don't need
pop <- pop %>% 
  select(-aian, -nhopi, -other)

# Rename
pop <- rename(pop, other = other2)

# Calculate percent of each race
pop <- pop %>%
  mutate(white_per = round((white / total)*100,1),
         black_per = round((black / total)*100,1),
         asian_per = round((asian / total)*100,1),
         hisp_per = round((hisp / total)*100,1),
         other_per = round((other / total)*100,1))

# Remove raw count columns
pop <- pop %>% 
  select(-white, -black, -asian, -hisp, -other)

# Rename total for merge later on
pop <- rename(pop, total_pop = total)

###################################################################
####################### CLEANING BEATS ############################
###################################################################

# calls, master and crime data all have rows with incorrect beats
## Remove rows in master that have a beat that's NOT in the shapefile
master2 <- subset(master, (beat %in% beat_shape@data$beat))

# Do the same for crime data
crime2 <- subset(crime, (beat %in% beat_shape@data$beat))

# Do the same for calls
calls2 <- subset(calls, (beat %in% beat_shape@data$beat))

###################################################################
################ CALC PERCENTS OF STOPS X RACE ####################
###################################################################

stop_race <- master2 %>% 
  group_by(beat, race_condensed) %>% 
  count(race_condensed)

# Spread
library(tidyr)
stop_race <- stop_race %>%
  spread(key = race_condensed, value = n, fill = 0)

# Calculate total people stopped
stop_race$total_ppl_stopped <- rowSums(stop_race[, c(2:9)])

# Calculate percents
stop_race <- stop_race %>%
  mutate(per_asian_stopped = round((asian / total_ppl_stopped)*100,1),
         per_black_stopped = round((black / total_ppl_stopped)*100,1),
         per_hisp_stopped = round((hisp / total_ppl_stopped)*100,1),
         per_mesa_stopped = round((me_sa / total_ppl_stopped)*100,1),
         per_mixed_stopped = round((mixed / total_ppl_stopped)*100,1),
         per_nam_stopped = round((nam / total_ppl_stopped)*100,1),
         per_pi_stopped = round((pi / total_ppl_stopped)*100,1),
         per_white_stopped = round((white / total_ppl_stopped)*100,1))

# Remove raw number columns
stop_race <- stop_race %>% 
  select(-asian, -black, -hisp, -me_sa, -mixed, -nam, -pi, -white)

###################################################################
###################### MERGING TOGETHER ###########################
###################################################################

# Count of calls by beat
calls_beat <- calls2 %>% 
  group_by(beat) %>% 
  summarise(calls_total = n()) %>% 
  mutate(calls_percent = round((calls_total / sum(calls_total))*100,1))

# Merge with shapefile
library(tigris)
final <- geo_join(beat_shape, calls_beat, "beat", "beat")

# Crimes by beat
crimes_beat <- crime2 %>% 
  group_by(beat) %>% 
  summarise(all_crime_total = n()) %>% 
  mutate(all_crime_percent = round((all_crime_total / sum(all_crime_total))*100,1))

# Merge with shapefile
final <- geo_join(final, crimes_beat, "beat", "beat")

# Part1 crimes by beat
p1_beat <- crime2 %>% 
  filter(category == 1) %>% 
  group_by(beat) %>% 
  summarise(p1_total = n()) %>% 
  mutate(p1_percent = round((p1_total / sum(p1_total))*100,1))

# Merge with shapefile
final <- geo_join(final, p1_beat, "beat", "beat")

# Non violent crimes by beat
p2_beat <- crime2 %>% 
  filter(category == 0) %>% 
  group_by(beat) %>% 
  summarise(p2_total = n()) %>% 
  mutate(p2_percent = round((p2_total / sum(p2_total))*100,1))

# Merge with shapefile
final <- geo_join(final, p2_beat, "beat", "beat")

# Number of people stopped by beat
people_beat <- master2 %>% 
  group_by(beat) %>% 
  summarise(total_ppl_stopped = n()) %>% 
  mutate(ppl_percent = round((total_ppl_stopped / sum(total_ppl_stopped))*100,1))

# Merge with shapefile
final <- geo_join(final, people_beat, "beat", "beat")

# Number of people stopped by beat
## But weren't from a service call
people_non_serv_beat <- master2 %>% 
  group_by(beat) %>% 
  summarise(total_ppl_stopped_non_service = n()) %>% 
  mutate(ppl_non_service_percent = round((total_ppl_stopped_non_service / sum(total_ppl_stopped_non_service))*100,1))

# Merge with shapefile
final <- geo_join(final, people_non_serv_beat, "beat", "beat")

# Create unduplicated version of master to count stops
stops <- master2[!duplicated(master2$stop_id),]

# Stops by beat
stop_beat <- stops %>% 
  group_by(beat) %>% 
  summarise(total_stops = n()) %>% 
  mutate(stops_percent = round((total_stops / sum(total_stops))*100,1))

# Merge with shapefile
final <- geo_join(final, stop_beat, "beat", "beat")

# Stops by beat non service call
stop_nonserv_beat <- stops %>% 
  filter(is_serv == 0) %>% 
  group_by(beat) %>% 
  summarise(total_stops_nonservice = n()) %>% 
  mutate(stops_nonservice_percent = round((total_stops_nonservice / sum(total_stops_nonservice))*100,1))

# Merge with shapefile
final <- geo_join(final, stop_nonserv_beat, "beat", "beat")

# Stops by beat that were due to service call
stop_service_beat <- stops %>% 
  filter(is_serv == 1) %>% 
  group_by(beat) %>% 
  summarise(total_stops_service = n()) %>% 
  mutate(stops_service_percent = round((total_stops_service / sum(total_stops_service))*100,1))

# Merge with shapefile
final <- geo_join(final, stop_service_beat, "beat", "beat")

# Merge final with pop
final <- geo_join(final, pop, "beat", "beat")

# Merge final with stop_race
final <- geo_join(final, stop_race, "beat", "beat")

# Remove columns we don't need
names(final@data)
final@data <- final@data %>% 
  select(-beat.1, -beat.2, -beat.3, -beat.4,
         -beat.5, -beat.6, -beat.7, -beat.8,
         -beat.9, -beat.10, -beat.11)

# Create ratios
final@data <- final@data %>% 
  mutate(crimes_per_stop = round(all_crime_total / total_stops,1),
         stops_per_crime = round(total_stops / all_crime_total,1),
         ppl_per_crime = round(total_ppl_stopped / total_stops,1),
         stops_per_p1crime = round(total_stops / p1_total,1))

# Create file for further analysis
write.csv(final@data, "stops-crime-calls-by-beat.csv")

###################################################################
########################## ANALYSIS ###############################
###################################################################

n_distinct(final$beat)

# How many stops total?
n_distinct(master$stop_id)
# 353547

# How many people stopped?
n_distinct(master$id)
# 407684

# How many stops occurred in proper police beats?
n_distinct(master2$stop_id)
# 344419

# How many people were stopped in proper police beats?
n_distinct(master2$id)
# 398082

# Calculate proportion of race of all people stopped in legitimate beats
master2 %>% 
  count(race_condensed) %>% 
  arrange(desc(n))

# race_condensed  count
#          white 165208
#           hisp 118100
#          black  79854
#          asian  19072
#          me_sa  10657
#             pi   3021
#          mixed   1370
#            nam    800

# Calculate percentages
master2 %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed  count               percent
#          white 165208              41.5009973
#           hisp 118100              29.6672545
#          black  79854              20.0596862
#          asian  19072               4.7909727
#          me_sa  10657               2.6770866
#             pi   3021               0.7588889
#          mixed   1370               0.3441502
#            nam    800               0.2009636

# How many crimes were part 1?
crime2 %>% 
  count(category) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# category     n `(prop = n/sum(n)) * 100`
#        1 77419                      59.1
#        0 53473                      40.9

# Calls by disposition
call_disp <- calls2 %>% 
  group_by(disposition) %>% 
  summarise(total = n()) %>% 
  mutate(percent = round((total / sum(total))*100,1))

# Left join to get disposition descriptions
call_disp <- left_join(call_disp, disp, by = "disposition")

# Calls by type
call_types <- calls2 %>% 
  group_by(call_type) %>% 
  summarise(total = n()) %>% 
  mutate(percent = round((total / sum(total))*100,1))

# Most common call type by beat
call_types <- calls2 %>% 
  group_by(beat, call_type) %>% 
  summarise(total = n())

# Spread
call_types <- call_types %>% 
  spread(key = call_type, value = total, fill = 0)

###################################################################

## Calculating overall city data for graphics
# Total people stopped for traffic violations
reasons <- master2 %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

test <- master2 %>% 
  filter(is.na(reason_condensed))

remove(test)

master2$reason_condensed <- ifelse(is.na(master2$reason_condensed), "OTHER", master2$reason_condensed)

traffic <- master2 %>% 
  filter(reason_words == "Traffic Violation") %>% 
  count(reason_detail) %>% 
  arrange(desc(n))

sum(traffic$n)

searches <- master2 %>% 
  filter(is_searched == 1)

searches %>% 
  count(discretionary)

# discretionary     n
#             0 47655
#             1 37675

###################################################################

## Calculating graphics for neighborhoods

# Extract stops in 124 beat -- La Jolla
lajolla <- master2 %>% 
  filter(beat == 124)

# race of people stopped
lajolla %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
lajolla %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
lajolla %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
lajolla %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
lajolla %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################

# Extract stops in 712 beat -- San Ysidro
sy <- master2 %>% 
  filter(beat == 712)

# race of people stopped
sy %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
sy %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
sy %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
sy %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
sy %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################

# Extract stops in 825 beat -- Kensington
kens <- master2 %>% 
  filter(beat == 825)

# race of people stopped
kens %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
kens %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
kens %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
kens %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
kens %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################

# Extract stops in 626 beat -- Mission Hills
mh <- master2 %>% 
  filter(beat == 626)

# race of people stopped
mh %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
mh %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
mh %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
mh %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
mh %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################

# Extract stops in 724 beat -- Palm City
pc <- master2 %>% 
  filter(beat == 724)

# race of people stopped
pc %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
pc %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
pc %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
pc %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
pc %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################

# Extract stops in 832 beat -- Teralta West
tw <- master2 %>% 
  filter(beat == 832)

# race of people stopped
tw %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# reasons people were stopped
tw %>% 
  count(reason_condensed) %>% 
  arrange(desc(n))

# reason details for traffic violations
tw %>% 
  filter(reason_condensed == "TRAFFIC") %>% 
  count(reason_detail)

# People searched
tw %>% 
  count(is_searched)

# People searched discretionary vs non-discretionary
tw %>% 
  filter(is_searched == 1) %>% 
  count(discretionary)

###################################################################
## Populations of neighborhoods

pop_graphics <- pop %>% 
  filter(beat == 712 |
           beat == 832 |
           beat == 724 |
           beat == 626 |
           beat == 124 |
           beat == 825)

###################################################################
######################### EXTRACTIONS #############################
###################################################################

# Extract stops in 435 beat -- Broadway Heights
beat435_stops <- master2 %>% 
  filter(beat == 435)
write.csv(beat435_stops, "beat435_stops.csv")

# Extract crime in 435
beat435_crime <- crime2 %>% 
  filter(beat == 435)
write.csv(beat435_crime, "beat435_crime.csv")

# Extract calls in 435
beat435_calls <- calls2 %>% 
  filter(beat == 435)
write.csv(beat435_calls, "beat435_calls.csv")
table(crime2$violationType)
###################################################################

# Extract stops in 316 beat -- Qualcomm
beat316_stops <- master2 %>% 
  filter(beat == 316)
write.csv(beat316_stops, "beat316_stops.csv")

# Extract crime in 316
beat316_crime <- crime2 %>% 
  filter(beat == 316)
write.csv(beat316_crime, "beat316_crime.csv")

# Extract calls in 316
beat316_calls <- calls2 %>% 
  filter(beat == 316)
write.csv(beat316_calls, "beat316_calls.csv")

###################################################################

# Extract stops in 714 beat -- Border
beat714_stops <- master2 %>% 
  filter(beat == 714)
write.csv(beat714_stops, "beat714_stops.csv")

# Extract crime in 714
beat714_crime <- crime2 %>% 
  filter(beat == 714)
write.csv(beat714_crime, "beat714_crime.csv")

# Extract calls in 714
beat714_calls <- calls2 %>% 
  filter(beat == 714)
write.csv(beat714_calls, "beat714_calls.csv")

###################################################################

# Extract stops in 514 beat -- Sherman Heights
beat514_stops <- master2 %>% 
  filter(beat == 514)
write.csv(beat514_stops, "beat514_stops.csv")

# Extract crime in 514
beat514_crime <- crime2 %>% 
  filter(beat == 514)
write.csv(beat514_crime, "beat514_crime.csv")

# Extract calls in 514
beat514_calls <- calls2 %>% 
  filter(beat == 514)
write.csv(beat514_calls, "beat514_calls.csv")

###################################################################

# Extract stops in 932 beat -- Torrey Preserve
beat932_stops <- master2 %>% 
  filter(beat == 932)
write.csv(beat932_stops, "beat932_stops.csv")

# Extract crime in 932
beat932_crime <- crime2 %>% 
  filter(beat == 932)
write.csv(beat932_crime, "beat932_crime.csv")

# Extract calls in 932
beat932_calls <- calls2 %>% 
  filter(beat == 932)
write.csv(beat932_calls, "beat932_calls.csv")

###################################################################

# Extract stops in 511 beat -- Barrio Logan
beat511_stops <- master2 %>% 
  filter(beat == 511)
write.csv(beat511_stops, "beat511_stops.csv")

# Extract crime in 511
beat511_crime <- crime2 %>% 
  filter(beat == 511)
write.csv(beat511_crime, "beat511_crime.csv")

# Extract calls in 511
beat511_calls <- calls2 %>% 
  filter(beat == 511)
write.csv(beat511_calls, "beat511_calls.csv")

###################################################################

# Extract stops in 933 beat -- Del Mar Heights
beat933_stops <- master2 %>% 
  filter(beat == 933)
write.csv(beat933_stops, "beat933_stops.csv")

# Extract crime in 933
beat933_crime <- crime2 %>% 
  filter(beat == 933)
write.csv(beat933_crime, "beat933_crime.csv")

# Extract calls in 933
beat933_calls <- calls2 %>% 
  filter(beat == 933)
write.csv(beat933_calls, "beat933_calls.csv")

###################################################################

# Extract stops in 832 beat -- Teralta West
beat832_stops <- master2 %>% 
  filter(beat == 832)
write.csv(beat832_stops, "beat832_stops.csv")

# Extract crime in 832
beat832_crime <- crime2 %>% 
  filter(beat == 832)
write.csv(beat832_crime, "beat832_crime.csv")

# Extract calls in 832
beat832_calls <- calls2 %>% 
  filter(beat == 832)
write.csv(beat832_calls, "beat832_calls.csv")

###################################################################

# Extract stops in 831 beat -- Teralta East
beat831_stops <- master2 %>% 
  filter(beat == 831)
write.csv(beat831_stops, "beat831_stops.csv")

# Extract crime in 831
beat831_crime <- crime2 %>% 
  filter(beat == 831)
write.csv(beat831_crime, "beat831_crime.csv")

# Extract calls in 831
beat831_calls <- calls2 %>% 
  filter(beat == 831)
write.csv(beat831_calls, "beat831_calls.csv")

###################################################################

# Extract stops in 724 beat -- Palm City
beat724_stops <- master2 %>% 
  filter(beat == 724)
write.csv(beat724_stops, "beat724_stops.csv")

# Extract crime in 724
beat724_crime <- crime2 %>% 
  filter(beat == 724)
write.csv(beat724_crime, "beat724_crime.csv")

# Extract calls in 724
beat724_calls <- calls2 %>% 
  filter(beat == 724)
write.csv(beat724_calls, "beat724_calls.csv")

###################################################################

# Extract stops in 626 beat -- Mission Hills
beat626_stops <- master2 %>% 
  filter(beat == 626)
write.csv(beat626_stops, "beat626_stops.csv")

# Extract crime in 626
beat626_crime <- crime2 %>% 
  filter(beat == 626)
write.csv(beat626_crime, "beat626_crime.csv")

# Extract calls in 626
beat626_calls <- calls2 %>% 
  filter(beat == 626)
write.csv(beat626_calls, "beat626_calls.csv")

###################################################################

# Extract stops in 124 beat -- La Jolla
beat124_stops <- master2 %>% 
  filter(beat == 124)
write.csv(beat124_stops, "beat124_stops.csv")

# Extract crime in 124
beat124_crime <- crime2 %>% 
  filter(beat == 124)
write.csv(beat124_crime, "beat124_crime.csv")

# Extract calls in 124
beat124_calls <- calls2 %>% 
  filter(beat == 124)
write.csv(beat124_calls, "beat124_calls.csv")

###################################################################

# Extract stops in 825 beat -- Kensington
beat825_stops <- master2 %>% 
  filter(beat == 825)
write.csv(beat825_stops, "beat825_stops.csv")

# Extract crime in 825
beat825_crime <- crime2 %>% 
  filter(beat == 825)
write.csv(beat825_crime, "beat825_crime.csv")

# Extract calls in 825
beat825_calls <- calls2 %>% 
  filter(beat == 825)
write.csv(beat825_calls, "beat825_calls.csv")

###################################################################

# Extract stops in 712 beat -- San Ysidro
beat712_stops <- master2 %>% 
  filter(beat == 712)
write.csv(beat712_stops, "beat712_stops.csv")

# Extract crime in 712
beat712_crime <- crime2 %>% 
  filter(beat == 712)
write.csv(beat712_crime, "beat712_crime.csv")

# Extract calls in 712
beat712_calls <- calls2 %>% 
  filter(beat == 712)
write.csv(beat712_calls, "beat712_calls.csv")
