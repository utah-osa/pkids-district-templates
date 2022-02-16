library(tidyverse)
library(magrittr)
library(readxl)
library(beepr)
library(data.table)

##### GET LEA INFO #####

curr_lea <- "XXSD"

lea_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv", col_types = cols())
lea_ids %<>% mutate(district_location = str_pad(district_location, 3, "left", "0"))
lea_ids %<>% filter(district_code == curr_lea)

setwd(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Expenditures/Raw Data"))



##### IMPORT DATA #####

# load in expense data
expense <- readRDS(paste0("EXP_", lea_ids$district_code, ".rds"))

# load in payroll data
payroll <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/", lea_ids$district_code, "_Payroll_With_Incourse.rds"))

payroll %<>% mutate(u_fund = as.numeric(u_fund),
                    u_location = str_pad(as.character(u_location), 3, "left", "0"),
                    u_program = str_pad(as.character(u_program), 4, "left", "0"),
                    u_function = as.numeric(u_function), 
                    u_object = as.numeric(u_object))

# swap employee compensation in payroll for expense
expense %<>% filter(u_object < 100 | u_object >= 300)

expense %<>% full_join(payroll, by = c("fiscal_year", "u_fund", "u_location", "u_program", "u_function", "u_object", "name", "description" = "account_description", "total"))

# load in subcode information for programs, functions, and objects
funds <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/2021/PKIDS COA Mapping.xlsx", sheet = "Funds")
programs <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/2021/PKIDS COA Mapping.xlsx", sheet = "Programs")
functions <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/2021/PKIDS COA Mapping.xlsx", sheet = "Functions")
objects <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/2021/PKIDS COA Mapping.xlsx", sheet = "Objects")
program_updates <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/2021/PKIDS COA Mapping.xlsx", sheet = "Program Updates")

programs %<>% mutate(u_program = str_pad(as.character(u_program), 4, "left", "0"))



#### CLEAN EXPENSE FILE ####

# merge subcode information with expense data
expense %<>% left_join(funds, by = c("u_fund"))
expense %<>% left_join(programs, by = c("u_program"))
expense %<>% left_join(functions, by = c("u_function"))
expense %<>% left_join(objects, by = c("u_object"))

# overwrite programs with updated meanings
for(i in 1:nrow(program_updates)){
  expense %<>% mutate(p_desc = ifelse(u_program == program_updates$u_program[i] & fiscal_year >= program_updates$p_start[i], program_updates$p_desc[i], p_desc),
                      p_subcode = ifelse(u_program == program_updates$u_program[i] & fiscal_year >= program_updates$p_start[i], program_updates$p_subcode[i], p_subcode))
}


# assign subcodes based first on programs, objects, then functions

expense %<>% mutate(subcode = ifelse(!is.na(p_subcode), p_subcode, NA_character_),
                    aspect = ifelse(!is.na(p_subcode), "program", NA_character_))

expense %<>% mutate(aspect = ifelse(is.na(subcode) & !is.na(o_subcode), "object", aspect),
                    subcode = ifelse(is.na(subcode) & !is.na(o_subcode), o_subcode, subcode))

expense %<>% mutate(aspect = ifelse(is.na(subcode) & !is.na(f_subcode), "function", aspect),
                    subcode = ifelse(is.na(subcode) & !is.na(f_subcode), f_subcode, subcode))

# clean up expense file
expense <- as_tibble(expense)

expense %<>% mutate(default_sc = subcode, default_aspect = aspect) %>% # store default assignments
  select(-c(id, posting_date, batch_id, type, aspect, ends_with("_start"), ends_with("_end"))) # remove variables from Transparency & UCOA

expense %<>% select(fiscal_year, total, description, u_fund, u_location, u_program, u_function, u_object, name, subcode, # organize by account info
                    default_aspect, default_sc, # default assignments
                    p_desc, p_subcode, f_desc, f_subcode, o_desc, o_subcode, # standard descriptions and assignments
                    everything()) # Transparency descriptions and other flags


# ***********************************************************************************
# DISTRICT-SPECIFIC ADJUSTMENTS - LOCATION FIXES OR OTHER


# ***********************************************************************************



#### LOCATION COMPARISON ####

## read in all files and check locations are consistent for latest year
# check for new expense locations
expense %>% distinct(u_location, fiscal_year, org2) %>% group_by(u_location) %>% mutate(n = n()) %>% filter(fiscal_year == max(expense$fiscal_year) & n == 1)

### then record any new locations in the Location Crosswalk & workpapers

# merge locations with expense
locations <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name,"/Other Data and Resources/", lea_ids$district_code, "_Loc_Cross.csv"))
locations %<>% mutate(location = str_pad(as.character(location), 3, "left", "0")) %>% replace(is.na(.), "")
expense %<>% left_join(locations, by = c("u_location"= "location"))

# check that there is a location description for each expense location
expense %>% anti_join(locations, by = c("u_location" = "location")) %>% distinct(u_location)


# ___________________________________________________________________________________

#### STANDARD ALLOCATION SCRIPT ####

### 1. Account for program ranges in standard COA

expense %<>% mutate(subcode = case_when(as.numeric(u_program) >= 2200 & as.numeric(u_program) < 3600 ~ "F99",
                                        as.numeric(u_program) >= 3600 & as.numeric(u_program) < 3700 ~ "E18",
                                        TRUE ~ subcode))

# check for ranges being used for CTE courses
expense %>% filter(as.numeric(u_program) >= 6000 & as.numeric(u_program) <= 7000) %>% group_by(u_program, p_desc, subcode) %>% 
  summarize(total = sum(total)) %>% arrange(u_program)

# assign programs in CTE ranges to applicable subcode if necessary
expense %<>% mutate(subcode = case_when(!str_detect(subcode, "C") & str_detect(u_program, "^60") ~ "C09",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^61") ~ "C10",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^62") & fiscal_year < 2020 ~ "C13",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^62") & fiscal_year >= 2020 ~ "C12",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^63") ~ "C11",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^64") & fiscal_year < 2020 ~ "C12",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^64") & fiscal_year >= 2020 ~ "C15",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^65") ~ "C13",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^66") & fiscal_year < 2020 ~ "C14",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^66") & fiscal_year >= 2020 ~ "C16",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^67") ~ "C15",
                                        !str_detect(subcode, "C") & str_detect(u_program, "^68") ~ "C16",
                                        TRUE ~ subcode))


### 2. Assign subcodes based on internal COA programs, if applicable

# internal COA map format:
# SUBCODE column is blank if (1) used for the same purpose as the standard description or (2) is too vague to assign by program

# read in internal COA that's been mapped to PKIDS subcodes
try(internal_coa <- read_excel(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Other Data and Resources/", lea_ids$district_code,"_Program_Subcodes.xlsx")), silent = T)

if(exists("internal_coa")){
  
  internal_coa %<>% mutate(PROGRAM = str_pad(as.character(PROGRAM), 4, "left", "0"), DESCRIPTION = str_to_title(DESCRIPTION))
  
  # pull out programs that match to standard COA
  standard_programs <- expense %>% filter(!is.na(subcode) & default_aspect == "program") %>% distinct(u_program) %>% arrange(u_program) %>% pull(u_program)
  
  # manually check all programs that are in both charts of accounts and have different subcode assignments
  check_programs <- internal_coa %>% select(PROGRAM, DESCRIPTION, IN_EXPENSE, SUBCODE) %>% filter(IN_EXPENSE == 1) %>%
    left_join(programs, by = c("PROGRAM" = "u_program")) %>% filter(!is.na(p_subcode) & p_subcode != SUBCODE)
  
  internal_programs <- internal_coa %>% filter(!PROGRAM %in% standard_programs | PROGRAM %in% c("")) # keep all programs where internal programs should be used for mapping
  vague_programs <- internal_programs %>% filter(is.na(SUBCODE)) %>% pull(PROGRAM) # store vague programs to overwrite description only
  internal_programs %<>% filter(!is.na(SUBCODE))
  
  # assign custom subcodes and update descriptions for all programs with a different internal meaning
  for(i in 1:nrow(internal_programs)){
    expense %<>% mutate(subcode = ifelse(u_program == internal_programs$PROGRAM[i], internal_programs$SUBCODE[i], subcode),
                        p_desc = ifelse(u_program == internal_programs$PROGRAM[i], internal_programs$DESCRIPTION[i], p_desc))
  }
  
  # update descriptions only for vague programs
  for(i in 1:length(vague_programs)){
    expense %<>% mutate(p_desc = ifelse(u_program == vague_programs[i], internal_coa$DESCRIPTION[which(vague_programs[i] == internal_coa$PROGRAM)], p_desc))
  }
  
}


### 3. Assign subcodes for expenses in the no_subcodes file based on account codes and descriptions

# expense %<>% mutate(subcode = case_when(is.na(subcode) & str_detect(u_function, "^26") ~ "O99",
#                                         is.na(subcode) & u_object == 131 ~ "F99",
#                                         TRUE ~ subcode))


### 4. Assign only Superintendent, BA, and Board Members' pay and benefits to A99

expense %<>% mutate(subcode = case_when(subcode == "A99" ~ "A08", 
                                        TRUE ~ subcode))

expense %<>% group_by(name, fiscal_year, u_location) %>% 
  mutate(subcode = case_when(any(u_object %in% c(111, 112, 114)) ~ "A99", 
                             TRUE ~ subcode)) %>% ungroup()


### 5. Assign all principal and principal assistant pay and benefits to Q99

expense %<>% group_by(name, fiscal_year, u_location, u_program, u_function) %>% 
  mutate(subcode = case_when(any(u_object %in% c(121, 122)) ~ "Q99", 
                             TRUE ~ subcode)) %>% ungroup()


### 6. Use location codes to assign subcodes more specifically

expense %<>% mutate(subcode = case_when(str_detect(School, "Transportation") & subcode %in% c("U01","U03","U04","U22") ~ "T11", # SPED Transportation
                                        str_detect(School, "Transportation") & subcode != "M99" ~ "T01", # General Transportation
                                        str_detect(School, "Special Ed") & !str_detect(subcode, "^U") ~ "U01", # Special Education
                                        str_detect(School, "Maintenance") & subcode != "M99" ~ "O99", # Operational Expense
                                        str_detect(Level, "Preschool") ~ "X99", # Non K-12
                                        TRUE ~ subcode))

### 7. Assign all expenses with a Transportation function to T01/T11 (unless already in another T subcode)

expense %<>% mutate(subcode = case_when(str_detect(u_function, "^27") & !str_detect(subcode, "^U|^T") ~ "T01", # General Transportation
                                        str_detect(u_function, "^27") & subcode %in% c("U01","U03","U04","U22") ~ "T11", # SPED Transportation
                                        TRUE ~ subcode))


### 8. Assign all expenses with a Nutrition function to N01 (unless already in N99)

expense %<>% mutate(subcode = case_when(str_detect(u_function, "^31") & subcode != "N99" ~ "N01",
                                        TRUE ~ subcode))


### 9. Assign all expenses in the Student Activities Fund to E18 (unless already in another E subcode)

expense %<>% mutate(subcode = case_when(u_fund == 21 & !str_detect(subcode, "^E") ~ "E18",
                                        TRUE ~ subcode))


### 10. Create district-specific subcodes (Z-codes)

# expense %<>% mutate(subcode = case_when(u_program == "1234" ~ "Z01",
#                                         u_location == "800" ~ "Z02",
#                                         TRUE ~ subcode))


### 11. Reassign F99 based on function codes

expense %<>% mutate(subcode = case_when(subcode == "F99" & str_detect(u_function, "^23|^25") ~ "A08",
                                        subcode == "F99" & str_detect(u_function, "^21") ~ "B99",
                                        subcode == "F99" & str_detect(u_function, "^24") ~ "Q99",
                                        subcode == "F99" & str_detect(u_function, "^26") ~ "O99",
                                        subcode == "F99" & str_detect(u_function, "^4") ~ "M99",
                                        TRUE ~ subcode))


### 12. Reassign F99 based on object codes

# reassign salaries and associated benefits
expense %<>% group_by(name, fiscal_year, u_location, u_program, u_function) %>% 
  mutate(subcode = case_when(any(subcode == "F99" & u_object %in% c(100,110,113,115,150,151,152)) ~ "A08",
                             any(subcode == "F99" & u_object %in% c(144)) ~ "B05",
                             any(subcode == "F99" & u_object %in% c(141,142,143)) ~ "B99",
                             any(subcode == "F99" & u_object %in% c(195)) ~ "E01",
                             any(subcode == "F99" & u_object %in% c(191)) ~ "N01",
                             any(subcode == "F99" & u_object %in% c(180,181,182)) ~ "O99",
                             any(subcode == "F99" & u_object %in% c(120)) ~ "Q99",
                             any(subcode == "F99" & u_object %in% c(170,171,172,173,174,175)) ~ "T01",
                             TRUE ~ subcode)) %>% ungroup()

# reassign other expenses
expense %<>% mutate(subcode = case_when(subcode == "F99" & u_object %in% c(345) ~ "A07",
                                        subcode == "F99" & u_object %in% c(310,343,344,550,349,520,521,522,530,540) ~ "A09",
                                        subcode == "F99" & u_object %in% c(600,610,640,641,642,644,650,670,734,736) ~ "L99",
                                        subcode == "F99" & u_object %in% c(450,700,710,720,730,733,740,750,790) ~ "M99",
                                        subcode == "F99" & u_object %in% c(570,630) ~ "N01",
                                        subcode == "F99" & (u_object %in% c(500,620,621,622,623,624,625,626,629,680,731,739) | str_detect(u_object, "^4")) ~ "O99",
                                        subcode == "F99" & u_object %in% c(510,511,512,513,514,515,516,517,518,681,682,683,684,732,735) ~ "T01",
                                        TRUE ~ subcode))

### 13. Pull supplies from F02-F05

expense %<>% mutate(subcode = case_when(subcode == "F02" & u_object %in% c(600,610,640,641,642,644,650,670,734,736) ~ "L01", # elementary instructional supplies
                                        subcode == "F03" & u_object %in% c(600,610,640,641,642,644,650,670,734,736) ~ "L02", # middle school instructional supplies
                                        subcode == "F04" & u_object %in% c(600,610,640,641,642,644,650,670,734,736) ~ "L03", # high school instructional supplies
                                        subcode == "F05" & u_object %in% c(600,610,640,641,642,644,650,670,734,736) ~ "L04", # secondary instructional supplies
                                        TRUE ~ subcode))


### 14. Reassign B99 to F99 based on function codes if necessary

# View(expense %>% filter(subcode == "B99" & str_detect(u_function, "^1|^22")))
expense %<>% mutate(subcode = case_when(subcode == "B99" & str_detect(u_function, "^1|^22") ~ "F99",
                                        TRUE ~ subcode))


### 15. Move all A08 expenses that are not compensation to A09, and move compensation in A09 to A08

expense %<>% mutate(subcode = case_when(subcode == "A08" & (u_object < 100 | u_object >= 300) ~ "A09",
                                        subcode == "A09" & (u_object >= 100 & u_object < 300) ~ "A08",
                                        TRUE ~ subcode))


### 16. Reassign A08 and A09 expenses based on function codes

# View(expense %>% filter(subcode %in% c("A08", "A09") & str_detect(u_function, "^1|^21|^22")) %>% arrange(u_function))
expense %<>% mutate(subcode = case_when(#subcode %in% c("A08","A09") & str_detect(u_function, "^1|^22") ~ "F99",
                                        #subcode %in% c("A08","A09") & str_detect(u_function, "^21") ~ "B99",
                                        subcode %in% c("A08","A09") & str_detect(u_function, "^24") ~ "Q99",
                                        subcode %in% c("A08","A09") & str_detect(u_function, "^26") ~ "O99",
                                        subcode %in% c("A08","A09") & str_detect(u_function, "^4") ~ "M99",
                                        TRUE ~ subcode))


### 17. Reassign O99 at district-wide locations to O05

# check district locations to move to O05
expense %>% filter(subcode == "O99" & Level == "District" & !str_detect(School, "Maintenance")) %>% group_by(u_location, School) %>% summarize(total = sum(total))

expense %<>% mutate(subcode = ifelse(subcode == "O99" & Level == "District" & str_detect(School, "District"), "O05", subcode))


### 18. Move all interest payments on debt to D99 and all capital costs not depreciated to M99

expense %<>% mutate(subcode = case_when(u_object == 830  ~ "D99",
                                        u_object >= 700 & u_object < 800 | u_function >= 4000 & u_function < 5000 ~ "M99",
                                        TRUE ~ subcode))


### 19. Assign all expenses in the Non K12 Fund or with a Non K12 Function or for preschool to X99

expense %<>% mutate(subcode = case_when((u_fund == 23 | u_fund == 26 | str_detect(u_function, "^33")) | subcode %in% c("G14","U02","U12","U15","U23") ~ "X99", 
                                        TRUE ~ subcode))


### 20. Assign charter-related programs to most applicable subcode

expense %>% filter(u_program %in% c("5551", "5619", "5842", "5845", "5846", "5863", "5910", "7625", "7626")) %>%
  group_by(u_program, p_desc, subcode) %>% summarize(total = sum(total))


### 21. Implement other district-specific adjustments




# ___________________________________________________________________________________

## final checks for new allocation assignments

# check allocations for new programs in expense
expense %>% group_by(u_program, fiscal_year, p_desc, program2, subcode) %>% summarize(total = sum(total)) %>% ungroup() %>% 
  group_by(u_program) %>% mutate(n = n()) %>% filter(fiscal_year == max(expense$fiscal_year) & n == 1) %>% 
  arrange(desc(total))

# check allocations for new objects in expense
expense %>% group_by(u_object, fiscal_year, o_desc, cat2, subcode) %>% summarize(total = sum(total)) %>% ungroup() %>% 
  group_by(u_object) %>% mutate(n = n()) %>% filter(fiscal_year == max(expense$fiscal_year) & n == 1) %>% 
  arrange(desc(total))

# check allocations for new functions in expense
expense %>% group_by(u_function, fiscal_year, f_desc, function2, subcode) %>% summarize(total = sum(total)) %>% ungroup() %>% 
  group_by(u_function) %>% mutate(n = n()) %>% filter(fiscal_year == max(expense$fiscal_year) & n == 1) %>% 
  arrange(desc(total))


# then assign all teacher pay to P99
expense %<>% mutate(subcode = if_else(!is.na(incourse) & incourse == 1, "P99", subcode))



#### ADD DEBT AND CAPITAL ####

source("H:/Economists/Ed/KIDS/All LEAs/Global Scripts/Allocate Debt and Capital.R")

cafr_detail <- read_excel("H:/Economists/Ed/KIDS/All LEAs/OSA, CAFR Digitization 3.0.xlsx", sheet = "fs")
depreciation <- cafr_detail %>% filter(LCODE == lea_ids$lcode & FY >= min(expense$fiscal_year)) %>% filter(is.na(DEP))

# depreciate expense
expense <- depreciate_expense(expense, depreciation, lea_ids)



##### SUPPLEMENT MISSING MINOR FUNDS WITH CAFR VALUES #####

cafr <- read_csv("H:/Economists/Ed/KIDS/All LEAs/Data Quality Checks/District Fund Check.csv", col_types = cols())
cafr %<>% filter(str_detect(district_name, paste0(lea_ids$district_code))) %>% rename(fund_desc = fund_name)

cafr %<>% filter(fund_desc %in% c("Student Activities Fund", "Student Activity Fund", "Tax Increment Financing Fund", "Non K-12 Programs", "School Food Services Fund", "Debt Services Fund")) # filter on standard minor funds
cafr %<>% filter(abs(percent_diff) > 50) %>% select(fiscal_year, u_fund, fund_desc, total = difference) # filter on funds that have more than a 50% difference between Transparency and cafr totals


# ***********************************************************************************
# DISTRICT-SPECIFIC ADJUSTMENTS - IMPUTE DEBT SERVICES FUND, IF APPLICABLE

# manually impute debt services fund since this fund doesn't have a one-to-one relationship with a single subcode
# see statement of revenues, expenditures, and changes in fund balance in CAFR for D50/D99 breakdown of debt services
# where principal retirement -> D50, interest and fiscal charges -> D99

cafr %>% filter(fund_desc == "Debt Services Fund") # check if debt services fund needs to be imputed

try(debt <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Expenditures/Working Files/", lea_ids$district_code, "_DEBT.csv"), col_types = cols()), silent = T)

if(exists("debt")){
  exp_debt <- expense %>% filter(fiscal_year %in% cafr$fiscal_year[which(cafr$fund_desc == "Debt Services Fund")] & u_fund == 31) %>% 
    group_by(fiscal_year, subcode) %>% summarize(total = sum(total))
  
  debt %<>% left_join(exp_debt, by = c("fiscal_year", "subcode"), suffix = c(".cafr", ".exp")) %>% mutate(total.exp = replace_na(total.exp, 0), diff = total.cafr - total.exp)
  debt %<>% select(-c(total.cafr, total.exp, description))
  
  cafr %<>% left_join(debt, by = c("fiscal_year", "u_fund")) %>% mutate(total = ifelse(u_fund == 31, diff, total)) %>% select(-diff)
  cafr %<>% mutate(fund_desc = ifelse(fund_desc == "Debt Services Fund", "Debt Service Fund", fund_desc))
  
} else { cafr %<>% mutate(subcode = NA_character_) }

# ***********************************************************************************

# assign all other missing fund values to subcodes

cafr %<>% mutate(description = "CAFR ADJUSTMENT", u_program = "0000", u_function = 0, u_object = 0, name = "Office of the State Auditor", u_location = lea_ids$district_location) %>% 
  mutate(subcode = case_when(fund_desc %in% c("Tax Increment Financing Fund", "Non K-12 Programs") ~ "X99", # assign funds to related subcode
                             fund_desc == "School Food Services Fund" ~ "N01",
                             fund_desc %in% c("Student Activities Fund", "Student Activity Fund") ~ "E18",
                             TRUE ~ subcode))

expense %<>% full_join(cafr) # join CAFR imputations to expense

# fill in account code descriptions for data imputations
expense %<>% 
  group_by(u_fund) %>% fill(fund_desc, .direction = "downup") %>% ungroup() %>% 
  group_by(u_program) %>% fill(p_desc, .direction = "downup") %>% ungroup() %>% 
  group_by(u_function) %>% fill(f_desc, .direction = "downup") %>% ungroup() %>% 
  group_by(u_object) %>% fill(o_desc, .direction = "downup") %>% ungroup()

# check no duplicate descriptions by account segment
desc_check <- bind_rows(
  expense %>% distinct(u_fund, fund_desc) %>% group_by(u_fund) %>% mutate(n = n()) %>% 
    filter(n > 1) %>% rename(Code = u_fund, Desc = fund_desc) %>% mutate(Segment = "Fund"),
  
  expense %>% distinct(u_program, p_desc) %>% group_by(u_program) %>% mutate(n = n(), u_program = as.numeric(u_program)) %>% 
    filter(n > 1 & !u_program %in% program_updates$u_program) %>% rename(Code = u_program, Desc = p_desc) %>% mutate(Segment = "Program"),
  
  expense %>% distinct(u_function, f_desc) %>% group_by(u_function) %>% mutate(n = n()) %>% 
    filter(n > 1) %>% rename(Code = u_function, Desc = f_desc) %>% mutate(Segment = "Function"),
  
  expense %>% distinct(u_object, o_desc) %>% group_by(u_object) %>% mutate(n = n()) %>% 
    filter(n > 1) %>% rename(Code = u_object, Desc = o_desc) %>% mutate(Segment = "Object")
)

desc_check %>% select(Segment, Code, Desc) # make sure null before proceeding



#### FINAL JOINS ####

# check for transactions with no subcodes
no_subcodes <- expense %>% filter(is.na(subcode)) 
no_subcodes %<>% group_by(description, u_program, u_function, u_object) %>% summarize(total = sum(total), n = n())

# anything with still no subcode appears to be truly miscellaneous 
expense %<>% mutate(subcode = ifelse(is.na(subcode), "Z99", subcode))


## break out transportation spending
# the july 15th data details the proportion of transportation money spent on different purposes
july15 <- read_excel("H:/Economists/Ed/KIDS/All LEAs/July15_C_DB_2014-21.xlsx")
july15 %<>% filter(District == lea_ids$transportation_id) %>% select(fiscal_year, T01, T08, T09, T10, X99)
july15 %<>% gather(T01, T08, T09, T10, X99, key = "subcode", value = "J15_percent")

transportation <- expense %>% filter(subcode == "T01")
transportation %<>% group_by(fiscal_year, u_location) %>% summarize(total = sum(total)) %>% ungroup()
transportation %<>% full_join(july15)
transportation %<>% mutate(total = round(total*J15_percent, 2)) %>% select(-J15_percent)

# create spending matrix for allocation
spending <- expense %>% filter(!subcode %in% c("P99", "T01")) %>% group_by(fiscal_year, u_location, subcode) %>% summarize(total = sum(total, na.rm = T))
spending <- full_join(spending, transportation)

spending %<>% group_by(fiscal_year, u_location, subcode) %>% summarize(total = round(sum(total), 2)) %>% ungroup()

subcodes <- read_excel("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/Subcodes and Expense Categories 2021.xlsx")
subcodes %<>% select(SC, Expense_Category, PC, PC_NAME)
expense %<>% left_join(subcodes, by = c("subcode" = "SC"))


# ***********************************************************************************
# DISTRICT-SPECIFIC ADJUSTMENTS - MODIFY Z CODE DESCRIPTIONS

expense %>% filter(str_detect(subcode, "Z") & subcode != "Z99") %>% distinct(subcode) # check for Z codes

expense %<>% # fill in Z code labels if necessary
  mutate(Expense_Category = case_when(subcode == "P99" ~ "Teacher Pay",
                                      subcode == "Z01" ~ "[Z01 Label]",
                                      TRUE ~ Expense_Category),
         PC_NAME = case_when(subcode == "P99" ~ "Instructional Related Expenses",
                             subcode %in% c("Z01") ~ "Instructional Related Expenses",
                             TRUE ~ PC_NAME),
         PC = case_when(subcode == "P99" ~ "F",
                        subcode %in% c("Z01") ~ "F",
                        TRUE ~ PC))

# flag expenses to not be allocated to students - add any Z codes if necessary
expense %<>% mutate(FLAG_ALLOC_EX = if_else(subcode %in% c("D50", "X99"), 1, 0)) 

# ***********************************************************************************



#### EXPORT FILES ####

## finalize expense file
# read in schema table
schema <- read_csv("H:/Economists/Ed/KIDS/All LEAs/Database/database_schema/exports_v3/expense_schema.csv")
fields <- schema %>% pull(field)

setdiff(names(expense), fields) # check for any useful payroll descriptions being dropped

# add identifier variables
expense %<>% mutate(lea_name = lea_ids$entity_name, lea_id = lea_ids$district_id, lea_code = lea_ids$district_code)
expense %<>% select(fields) %>% relocate(fields) # order variables using schema table

ncol(expense) == 40 # check correct number of columns

expense %>% filter(is.na(u_location) | is.na(fiscal_year) | is.na(subcode)) # check no null fields


# check new subcodes added
explainer <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Allocation_Explanation_", lea_ids$district_code, ".csv"), col_types = cols())

expense %>% filter(!subcode %in% explainer$subcode & subcode != "P99") %>% distinct(subcode, Expense_Category) %>% arrange(subcode)


## export files
# write out spending matrix for allocation
write_csv(spending, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Allocation_Matrix_", lea_ids$district_code, ".csv"))

# write out expense file for database upload
fwrite(expense, paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/expense/", tolower(lea_ids$district_code), "_expense.txt"), 
       sep = "|", col.names = TRUE, na = "", quote = FALSE)

beep(1) # alert once files have exported
