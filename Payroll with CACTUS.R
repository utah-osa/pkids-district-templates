require(tidyverse)
require(magrittr)
require(stringdist)
require(fuzzyjoin)

# function to find the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

########################
##### GET LEA INFO #####

curr_lea <- "[lea code]"
payroll_file_name <- "[payroll file].csv"

lea_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv", col_types = cols())
lea_ids %<>% mutate(district_location = str_pad(district_location, 3, "left", "0"))
lea_ids %<>% filter(district_code == curr_lea)

setwd(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files"))

#######################
##### IMPORT DATA #####

### course data

# import course data
course <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/sm_", lea_ids$district_code, ".RDS")) 

# exclude classes that should not receive an allocation of payroll money (online, release time, etc.) or that do not have an assigned teacher
course %<>% filter(INEX_CLASS == 1 & !is.na(TEACHER_ID))

# only keep the unique combinations of teacher and year to identify which teachers appear in the course data each year
course %<>% mutate(TEACHER_ID = as.character(TEACHER_ID)) %>% distinct(SCHOOL_YEAR, DISTRICT_ID, TEACHER_ID)

### cactus data (USBE information on teachers)

# import cactus data
cactus <- readRDS("H:/Economists/Ed/KIDS/All LEAs/USBE Data/cactus.RDS")

# only keep observations for this school district
cactus %<>% filter(district_id == lea_ids$district_id)

### payroll data

# import payroll data
payroll <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/", payroll_file_name)) 
payroll %<>% mutate(CACTUS_ID = as.character(CACTUS_ID))

# create a variable with the district ID
payroll %<>% mutate(district_id = lea_ids$district_id)

# create a list of payroll names (to merge files and make selections in the code)
payroll_names <- names(payroll)
# identify the first and last years that we have payroll data for (for loops in the code)
min_year <- min(payroll$Fiscal_Year)
max_year <- max(payroll$Fiscal_Year)

# ensure there is only one name associated with each employee ID (for fuzzy matching)
payroll %<>% group_by(Employee_ID) %>% mutate(Name = getmode(toupper(Name))) %>% ungroup()

# ensure payroll file is aggregated to the correct level (for joining with other files)
payroll %<>% group_by_at(payroll_names[which(payroll_names != "Amount")]) %>% summarise(Amount = sum(Amount)) %>% ungroup()

### standard object codes with corresponding types (Benefits, Salary, Other)

# import paycodes data
paycodes <- read_csv("H:/Economists/Ed/Data/STABLE/PayrollObjectCode.csv") 

# clean paycodes data
paycodes %<>% select(code = u_object, subcode = o_subcode, type)

#############################################################################
##### MERGE PAYROLL WITH COURSE/CACTUS INFORMATION TO IDENTIFY TEACHERS #####

### prepare course/cactus information to fuzzy match teacher names to payroll names

# course data contains cactus (state) id
# cactus data contains cactus (state) id and name info
# payroll data contains employee (internal) id and name info

# to merge data sources, we must
# (1) match course and cactus data on cactus id
# (2) match the course/cactus data with payroll on teacher name

# merge cactus teacher information to teachers in the course data
course_cactus <- course %>% left_join(cactus %>% distinct(person_id, first_name, last_name, middle_name, maiden_name), by = c("TEACHER_ID" = "person_id"))

# create 13 variations of each teacher's name (to prepare for fuzzy matching to different payroll name structures)
course_cactus %<>% 
  mutate(NM1 = str_c(last_name, first_name, sep = ", ")) %>% 
  mutate(NM2 = str_c(last_name, ", ",first_name, " ",middle_name)) %>% 
  mutate(NM3 = str_c(last_name, ", ",first_name, " ", str_sub(middle_name, 1, 1))) %>% 
  mutate(NM4 = if_else(maiden_name !="", str_c(maiden_name, first_name, sep = ", "),"")) %>%
  mutate(NM5 = if_else(maiden_name !="", str_c(maiden_name, ", ",first_name, " ",middle_name),"")) %>%
  mutate(NM6 = if_else(maiden_name !="", str_c(maiden_name, ", ",first_name, " ", str_sub(middle_name, 1, 1)),"")) %>%
  mutate(NM7 = if_else(maiden_name !="",str_c(last_name, "-", maiden_name, ", ", first_name),"")) %>%
  mutate(NM8 = if_else(maiden_name !="",str_c(maiden_name, "-", last_name, ", ", first_name),"")) %>%
  mutate(NM9 = str_c(first_name, last_name, sep = " ")) %>%
  mutate(NM10 = if_else(middle_name !="", str_c(first_name, middle_name, last_name, sep = " "),"")) %>%
  mutate(NM11 = if_else(middle_name !="", str_c(first_name, str_sub(middle_name, 1, 1), last_name, sep = " "),"")) %>%
  mutate(NM12 = if_else(middle_name !="", str_c(last_name, middle_name, sep = ", "),"")) %>%
  mutate(NM13 = str_c(last_name, str_sub(first_name, 1, 5), sep = ", "))

# remove any NA's in the name columns for fuzzy matching function
course_cactus %<>% mutate_if(is.character, replace_na, replace = "")

### prepare payroll data for fuzzy matching

# create a list of all employees for each fiscal year (if cactus ID is missing)
payroll_namesbyyear <- payroll %>% distinct(Fiscal_Year, CACTUS_ID, Employee_ID, Name)

### match course/cactus names to payroll names, by year

match <- list()
nomatch <- list()

for(year in min_year:max_year){ 
  
  # filter both dataframes to same year
  course_cactus_year <- course_cactus %>% filter(SCHOOL_YEAR == year & !(TEACHER_ID %in% (payroll %>% filter(!is.na(CACTUS_ID)) %>% distinct(CACTUS_ID) %>% pull()))) 
  payroll_names_year <- payroll_namesbyyear %>% filter(Fiscal_Year == year & is.na(CACTUS_ID))
  
  # 1. exact match payroll names to course/cactus names (max distance = 0)
  match[[year]] <- stringdist_inner_join(course_cactus_year, payroll_names_year, by = c("NM1" = "Name"), max_dist = 0)
  nomatch[[year]] <- stringdist_anti_join(course_cactus_year, payroll_names_year, by = c("NM1" = "Name"), max_dist = 0)
  
  for(i in 2:13){
    by.val <- c("Name")
    names(by.val) <- paste0("NM", i)
    match[[year]] %<>% rbind(stringdist_inner_join(nomatch[[year]], payroll_names_year %>% filter(!(Employee_ID %in% match$Employee_ID)), by = by.val, max_dist = 0))
    nomatch[[year]] %<>% stringdist_anti_join(payroll_names_year, by = by.val, max_dist = 0)
  }
  
  # 2. fuzzy match remaining payroll names to course/cactus names (max distance = 1)
  for(i in 1:13){
    by.val <- c("Name")
    names(by.val) <- paste0("NM", i)
    match[[year]] %<>% rbind(stringdist_inner_join(nomatch[[year]], payroll_names_year %>% filter(!(Employee_ID %in% match$Employee_ID)), by = by.val, max_dist = 1))
    nomatch[[year]] %<>% stringdist_anti_join(payroll_names_year, by = by.val, max_dist = 1)
  }
}

rm(course_cactus_year, payroll_names_year, by.val, year, i)

### unlist and bind rows for all years of the match and nomatch files

match %<>% bind_rows()
nomatch %<>% bind_rows()

# _________________________________________________________________________________________
# DISTRICT-SPECIFIC ADJUSTMENTS - MANUAL DUPLICATE MATCH CORRECTIONS & MANUAL NAME MATCHING 

### check for duplicate matches (multiple payroll employees matched to a single cactus ID or multiple cactus IDs matched to a single payroll employee)

# compare the following files to the payroll and payroll_namesbyyear files to figure out which match is correct if duplicated
# look through descriptions for pay and years in the data
# match %>% group_by(TEACHER_ID) %>% mutate(n = length(unique(Employee_ID))) %>% filter(n > 1) %>% View() 
# match %>% group_by(Employee_ID) %>% mutate(n = length(unique(TEACHER_ID))) %>% filter(n > 1) %>% View() 

### make adjustments to remove duplicate matches (then recheck files above to ensures all duplicates have been removed)
# match %<>% filter(!((TEACHER_ID == "" & Employee_ID == "") | (TEACHER_ID == "" & Employee_ID == "")))

### merge matches back to payroll employee list by fiscal year, to use for additional manual matches

payroll_namesbyyear %<>% left_join(match %>% distinct(SCHOOL_YEAR, Employee_ID, TEACHER_ID), by = c("Fiscal_Year" = "SCHOOL_YEAR", "Employee_ID"))

## move all original cactus IDs to the matched cactus IDs column

payroll_namesbyyear %<>% mutate(TEACHER_ID = if_else(is.na(TEACHER_ID), CACTUS_ID, TEACHER_ID))

### check for manual matches

# check which names are new in the latest year
# nomatch %>% group_by(TEACHER_ID) %>% mutate(count = n()) %>% ungroup() %>% filter(SCHOOL_YEAR == max_year & count == 1) %>% View()

# compare the nomatch file to the payroll_namesbyyear file to make additional matches by year
# payroll_namesbyyear %<>%
#   mutate(TEACHER_ID = if_else(Name=="","",TEACHER_ID),
#          TEACHER_ID = if_else(Employee_ID == "", "", TEACHER_ID))

# check for remaining unmatched teachers after fuzzy & manual matching
nomatch %<>% anti_join(payroll_namesbyyear %>% subset(!is.na(TEACHER_ID) | TEACHER_ID == ""), by = c("SCHOOL_YEAR" = "Fiscal_Year", "TEACHER_ID"))
# _________________________________________________________________________________________

### create and export full crosswalk between cactus & employee ids

id_crosswalk <- payroll_namesbyyear %>% left_join(course_cactus %>% distinct(TEACHER_ID, NM1), by = "TEACHER_ID") %>% 
  filter(!is.na(TEACHER_ID)) %>% distinct(TEACHER_ID, Employee_ID, Name, NM1) %>% 
  select(cactus_id = TEACHER_ID, cactus_name = NM1, payroll_id = Employee_ID, payroll_name = Name)
saveRDS(id_crosswalk, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/id_crosswalk.RDS"))

### create a file with all information for each teacher

# join payroll names & IDs to course/cactus information by cactus ID and year 
# this file now includes every individual in payroll who matched to the course/cactus information for each year
teachers_only <- left_join(course_cactus, payroll_namesbyyear, by = c("TEACHER_ID", "SCHOOL_YEAR" = "Fiscal_Year")) %>%
  select(SCHOOL_YEAR, TEACHER_ID, Cactus_Name = NM1, Employee_ID)

# join course/cactus information back to full payroll data
# this file now includes all compensation information ONLY for teachers who were matched to course/cactus information for each year
teachers_only <- left_join(payroll, teachers_only, by = c("Employee_ID", "Fiscal_Year" = "SCHOOL_YEAR")) %>%
  filter(!(is.na(TEACHER_ID)) & TEACHER_ID != "")

####################################################################
##### ASSIGN TEACHER PAY TO INSTRUCTIONAL OR NON-INSTRUCTIONAL #####

### clean teachers_only file for assignment of individual transactions

# pull out individual components of the account number
if(str_count(teachers_only$Account_No[1], "-") == 5){
  teachers_only %<>% separate(Account_No, c("FUND", "LOCATION", "YEAR", "PROGRAM", "FUNCTION", "OBJECT"), sep = "-", remove = FALSE)
} else if(str_count(teachers_only$Account_No[1], "-") == 4){
  teachers_only %<>% separate(Account_No, c("FUND", "LOCATION", "PROGRAM", "FUNCTION", "OBJECT"), sep = "-", remove = FALSE)
}

teachers_only %<>% mutate(OBJECT = as.numeric(OBJECT), FUNCTION = as.numeric(FUNCTION))

### make default instructional/non-instructional assignments based on object codes

# join teacher payroll information with default assignments based on object codes
teachers_only %<>% left_join(paycodes, by = c("OBJECT"="code")) %>%
  select(all_of(payroll_names),"TEACHER_ID", "OBJECT", "FUNCTION", "PROGRAM", "type") 

# assign all benefits to instructional
teachers_only %<>% mutate(type = if_else(str_detect(as.character(OBJECT), "^2"), "B", type))

# __________________________________________________________________________________
# DISTRICT-SPECIFIC ADJUSTMENTS - MANUAL INSTRUCTIONAL/NON-INSTRUCTIONAL ASSIGNMENTS

# identify which object codes were not automatically assigned: 
# teachers_only %>% filter(is.na(type)) %>% arrange(OBJECT) %>% View()

# identify exceptions to default assignments based on object & description: 
# teachers_only %>% distinct(OBJECT, `CDH Description`, `Key Description`) %>% arrange(OBJECT) %>% View()

# idenfity exceptions to default assignments based on function & description: 
# teachers_only %>% distinct(FUNCTION, `CDH Description`, `Key Description`) %>% arrange(FUNCTION) %>% View()

## Which descriptions are new each year? What object codes are new? Functions?

# check new descriptions in latest year
# teachers_only %>% distinct(`CDH Description`, `Key Description`, Fiscal_Year, type) %>% group_by(`CDH Description`, `Key Description`) %>% mutate(count = length(unique(Fiscal_Year))) %>% ungroup() %>%
#   filter(Fiscal_Year == max_year & count == 1) %>% select(-count) %>% View()

# check new objects/functions by latest year
# teachers_only %>% distinct(OBJECT, Fiscal_Year, type) %>% group_by(OBJECT) %>% mutate(count = length(unique(Fiscal_Year))) %>% ungroup() %>%
#   filter(Fiscal_Year == max_year & count == 1) %>% select(-count) %>% View()
# teachers_only %>% distinct(FUNCTION, Fiscal_Year, type) %>% group_by(FUNCTION) %>% mutate(count = length(unique(Fiscal_Year))) %>% ungroup() %>%
#   filter(Fiscal_Year == max_year & count == 1) %>% select(-count) %>% View()

# check combination of function/object/description by latest year
# teachers_only %>% distinct(FUNCTION, OBJECT, `CDH Description`, `Key Description`, Fiscal_Year, type) %>% group_by(FUNCTION, OBJECT, `CDH Description`, `Key Description`) %>% mutate(count = length(unique(Fiscal_Year))) %>% ungroup() %>%
#   filter(Fiscal_Year == max_year & count == 1) %>% select(-count) %>% View()

# change default assignments for pay based on descriptions & function codes
# ...

# If all pay was non-instructional for a teacher in a year, make all benefits non-instructional that weren't captured with the previous code
# teachers_only %<>% group_by(TEACHER_ID, Fiscal_Year) %>% 
#   mutate(prop_S = sum(Amount[type == "S"]) /sum(Amount[type %in% c("S","O")])) %>% ungroup() %>%
#   mutate(type = if_else(prop_S == 0 & !is.na(prop_S) & type == "B", "NB", type))

# make adjustments for specific teachers in the data
# ...

# make sure that all transactions have been assigned
sum(is.na(teachers_only$type))
# __________________________________________________________________________________

### create a variable to identify teachers' instructional compensation

teachers_only %<>% mutate(incourse = if_else(type %in% c("S","B"), 1, 0))

########################################################################
##### CREATE FILE FOR TEACHER INSTRUCTIONAL ALLOCATION TO STUDENTS #####

### create file with aggregate compensation calculations for each teacher

aggpay_teachers <- teachers_only %>%
  group_by(Fiscal_Year, TEACHER_ID, Employee_ID, Name) %>%
  summarise(inst_pay = sum(Amount[type == "S"]),
            inst_benefits = sum(Amount[type == "B"]),
            noninst_pay = sum(Amount[type == "O"]),
            noninst_benefits = sum(Amount[type == "NB"]),
            total_instcomp = sum(Amount[type %in% c("S","B")]),
            total_comp = sum(Amount)) %>% ungroup()

### join aggregate compensation calculations to course/cactus data

Teacher_Payroll <- left_join(course_cactus, aggpay_teachers, by=c("TEACHER_ID", "SCHOOL_YEAR"="Fiscal_Year"))
Teacher_Payroll %<>% select(-c(NM2:NM13)) %>% rename(Cactus_Name = NM1)  

### join cactus summary information with payroll data

Teacher_Payroll %<>% left_join(cactus %>% select(person_id, school_year, license_level, degree_summary, nonintern_yrs, intern_yrs = intern_years, intern_flag),
                               by=c("TEACHER_ID"="person_id","SCHOOL_YEAR"="school_year"))

################################################################################
##### CREATE FILE TO SUPPLEMENT TRANSPARENT UTAH EXPENSE COMPENSATION DATA #####

### join teacher compensation assignments to full payroll data

Payroll_With_Incourse <- payroll %>% left_join(teachers_only, by = c(payroll_names))

### change incourse flag to 0 for all employees without a cactus ID

Payroll_With_Incourse %<>% mutate(incourse = if_else(is.na(TEACHER_ID), 0, incourse))

### pull out individual components of the account number

if(str_count(Payroll_With_Incourse$Account_No[1], "-") == 5){
  Payroll_With_Incourse %<>% separate(Account_No, c("u_fund", "u_location", "year", "u_program", "u_function", "u_object"), sep = "-", remove = FALSE) %>% select(-year)
} else if(str_count(Payroll_With_Incourse$Account_No[1], "-") == 4){
  Payroll_With_Incourse %<>% separate(Account_No, c("u_fund", "u_location", "u_program", "u_function", "u_object"), sep = "-", remove = FALSE)
}

# __________________________________________________________________________
# DISTRICT-SPECIFIC ADJUSTMENTS - ADJUST LOCATIONS TO MATCH THE EXPENSE FILE

# make locations more specific
# ...
# __________________________________________________________________________

###############################
##### PAYROLL DATA CHECKS #####

### check payroll totals

# check for teachers with high or low compensation (make adjustments as necessary)
Teacher_Payroll %>% subset(total_instcomp != 0 & (total_instcomp < 5000 | total_instcomp > 100000)) %>% View()
# check that payroll totals didn't change in the cleaning process
sum(payroll$Amount) == sum(Payroll_With_Incourse$Amount)
# check that incourse totals are equal between the final allocation & expense files
sum(Payroll_With_Incourse$Amount[Payroll_With_Incourse$incourse == 1]) == sum(Teacher_Payroll$total_instcomp, na.rm = TRUE)
# check that incourse totals are fairly consistent across years
Payroll_With_Incourse %>% filter(incourse == 1) %>% group_by(Fiscal_Year) %>% summarise(sum(Amount))

### prepare and export a file to check payroll vs. expense compensation totals

# get payroll compensation totals
payroll_total <- Payroll_With_Incourse %>% filter(u_object >= 100 & u_object < 300) %>% 
  mutate(type = ifelse(u_object >= 100 & u_object < 200, "pay", "benefit")) %>% rename(fiscal_year = Fiscal_Year) %>%
  group_by(fiscal_year, type) %>% summarize(total = sum(Amount))

# get expense compensation totals
expense_total <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Expenditures/Raw Data/EXP_", lea_ids$district_code, ".rds"))
expense_total %<>% filter(u_object >= 100 & u_object < 300) %>% mutate(type = ifelse(u_object >= 100 & u_object < 200, "pay", "benefit")) %>%
  group_by(fiscal_year, type) %>% summarize(total = sum(total))

# calculate differences and format
payroll_check <- left_join(payroll_total, expense_total, by = c("fiscal_year", "type"), suffix = c(".payroll", ".expense"))
payroll_check %<>% mutate(diff = total.expense - total.payroll, percent_diff = round(((total.payroll - total.expense)/total.expense)*100, 2))
payroll_check %<>% pivot_longer(cols = starts_with("total"), names_to = "source", names_prefix = "total.", values_to = "total") %>% 
  mutate(district_short = lea_ids$match) %>% select(district_short, fiscal_year, source, type, total, percent_diff, diff)

# export payroll check to common file
all_district_payroll <- read_csv("H:/Economists/Ed/KIDS/All LEAs/Data Quality Checks/District Payroll Check.csv")
all_district_payroll %<>% filter(district_short != lea_ids$match) # remove payroll data for current district if already there
all_district_payroll <- full_join(all_district_payroll, payroll_check) # join updated district payroll data
write_csv(all_district_payroll, "H:/Economists/Ed/KIDS/All LEAs/Data Quality Checks/District Payroll Check.csv") # export updated payroll data

# export fund check to district folder - in case common file becomes corrupted, recreate with combining these files
write_csv(payroll_check, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/", lea_ids$district_code, "_PAYROLL.csv"))

rm(payroll_total, expense_total, payroll_check, all_district_payroll)

#################################################################
##### EXPORT FINAL FILES FOR EXPENSE AND STUDENT ALLOCATION #####

saveRDS(Teacher_Payroll, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/Teacher_Payroll_", lea_ids$district_code, ".RDS"))
saveRDS(Payroll_With_Incourse,paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/", lea_ids$district_code, "_Payroll_With_Incourse.RDS"))
