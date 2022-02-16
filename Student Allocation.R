library(tidyverse)
library(magrittr)
library(readxl)
library(data.table)

##### GET LEA INFO #####

district <- "XXXX"

lea_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv", col_types = cols())
lea_ids %<>% filter(district_code == district)

setwd(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation"))
options(scipen = 999)


# read in statewide merge data with teacher pay

all2 <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/smd_TeacherPay.rds"))
min_year <- min(all2$SCHOOL_YEAR)
max_year <- max(all2$SCHOOL_YEAR)

all2 %<>%
  mutate(TEACHER_ID = as.character(TEACHER_ID),
         CORE_CODE = as.character(CORE_CODE),
         SSID = as.character(SSID),
         SS_ID = as.character(SS_ID)) %>% 
  mutate(STUDENT_ID = ifelse(is.na(STUDENT_ID), "", STUDENT_ID))



#### JOIN DISTRICT-SPECIFIC DATA ####

# merge in transportation
transportation <- readRDS(paste0("H:/Economists/Ed/KIDS/All LEAs/Transportation/Working Files/Student Level Transportation/Student Level Transportation ", lea_ids$district_code, ".rds"))
transportation %<>% select(SCHOOL_YEAR, SCHOOL_NUMBER, STUDENT_ID, REAL_DATA, BUS_ELIGIBILE = eligibility) %>%
  mutate(SCHOOL_NUMBER = as.character(SCHOOL_NUMBER), STUDENT_ID = as.character(STUDENT_ID))

all2 %<>% left_join(transportation, by = c("SCHOOL_NUMBER", "SCHOOL_YEAR", "STUDENT_ID"))

# merge in nutrition
nutrition <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Nutrition/Student_Meals_", lea_ids$district_code, ".rds"))
nutrition %<>% select(SCHOOL_YEAR, SCHOOL_NUMBER, STUDENT_ID, REAL_MEALS, MEAL_COUNT) %>% 
  mutate(SCHOOL_NUMBER = as.character(SCHOOL_NUMBER), STUDENT_ID = as.character(STUDENT_ID))

all2 %<>% left_join(nutrition, by = c("SCHOOL_NUMBER", "SCHOOL_YEAR", "STUDENT_ID"))

# merge in grades
try(grades <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Grades/Course Grades ", lea_ids$district_code, ".csv"),
                   col_types = cols(STUDENT_ID = col_character(), TEACHER_ID = col_character(), CORE_CODE = col_character())), silent = T)

if(exists("grades")){
  
  grades %<>%
    filter(SCHOOL_YEAR >= min_year & SCHOOL_YEAR <= max_year) %>%
    group_by(STUDENT_ID, CORE_CODE, TEACHER_ID, SCHOOL_YEAR) %>%
    summarize(GPA_POINTS = mean(GPA_Points, na.rm = T)) %>%
    ungroup() %>%
    mutate(CORE_CODE = if_else(nchar(CORE_CODE) == 10, paste0("0", CORE_CODE), CORE_CODE))
  
  all2 %<>% left_join(grades, by = c("SS_ID" = "STUDENT_ID", "CORE_CODE", "SCHOOL_YEAR", "TEACHER_ID"))
  
  # all2 %>% count(GPA_POINTS) # check to make sure correct student ID field is used for grades merge
  
  all2 %<>% mutate(GPA_POINTS = if_else(is.na(GPA_POINTS) & !is.na(GPA), GPA, GPA_POINTS))
  
} else { all2 %<>% mutate(GPA_POINTS = GPA) }




#### ALLOCATE EXPENSES AND NON-INSTRUCTIONAL COMPENSATION ####

# district-specific allocation rules

all3 <- all2 %>%
  mutate(
    WGT = if_else(GRADE_LEVEL == -1, 1, WGT),
    
    # # District Wide Expenses
    # A01 = if_else(GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # A02 = if_else(GRADE_LEVEL %in% c(7, 8), WGT, NULL),
    # A03 = if_else(GRADE_LEVEL %in% c(9, 10, 11, 12), WGT, NULL),
    # A04 = if_else(GRADE_LEVEL %in% c(7, 8, 9, 10, 11, 12), WGT, NULL),
    # A06 = if_else(STUDENT_ID != "", WGT, NULL),
    # A07 = if_else(STUDENT_ID != "", WGT, NULL),
    # A08 = if_else(STUDENT_ID != "", WGT, NULL),
    # A09 = if_else(STUDENT_ID != "", WGT, NULL),
    # A99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Student Support Services
    # B01 = if_else(GRADE_LEVEL %in% c(1, 2, 3, 4, 5, 6), WGT, NULL),
    # B02 = if_else(DISABILITY_CODE_1 %in% c('CD', 'HI') | DISABILITY_CODE_2 %in% c('CD', 'HI') | DISABILITY_CODE_3 %in% c('CD', 'HI'), WGT, NULL), #Speech Audiology
    # B03 = if_else(DISABILITY_CODE_1 %in% c('DB', 'VI') | DISABILITY_CODE_2 %in% c('DB', 'VI') | DISABILITY_CODE_3 %in% c('DB', 'VI'), WGT, NULL), #Vision Services
    # B04 = if_else(DISABILITY_CODE_1 == 'OI' | DISABILITY_CODE_2 == 'OI' | DISABILITY_CODE_3 == 'OI' , WGT, NULL),           #Physical Therapy
    # B05 = if_else(STUDENT_ID != "", WGT, NULL),                        #Psychological Services
    # B99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Course Related Expenses
    # C01 = if_else(GRADE_LEVEL %in% c(10, 11), WGT, NULL),                      #Drivers Ed
    # C02 = if_else(CORE_CODE %in% c('25020000020'), WGT, NULL),        #Student Council
    # C03 = if_else(AP == 1, WGT, NULL),
    # C04 = if_else(CE == 1, WGT, NULL),
    # C05 = if_else(INSTITUTIONAL_SETTING == "ON", WGT, NULL),             #Online Ed
    # C06 = if_else(TEACHER_ID == '218972', WGT, NULL),                              #MATC
    # C07 = if_else(IB == 1, WGT, NULL),
    # C08 = if_else(Subject %in% c('Science', 'Math'), WGT, NULL),                 #STEM Center
    # C09 = if_else(Subject == 'CTE', WGT, NULL),                                              #CTE
    # C10 = if_else(Subject2 == 'Agriculture', WGT, NULL),                               #Agriculture
    # C11 = if_else(Subject2 == 'Health Science' , WGT, NULL),
    # C12 = if_else(Subject2 == 'FACS', WGT, NULL),
    # C13 = if_else(Subject2 == 'Business', WGT, NULL),
    # C14 = if_else(Subject2 == 'Trade & Technical', WGT, NULL),
    # C15 = if_else(Subject2 == 'Technology', WGT, NULL),
    # C16 = if_else(Subject2 == 'Engineering', WGT, NULL),
    # C17 = if_else(CORE_CODE == '39010000001', WGT, NULL),                     #College Career Awareness
    # C18 = if_else(Subject2 == "Work Based Learning", 1, NULL),                  #Work Based Learning
    # C19 = if_else(Subject == 'Foreign Language', WGT, NULL),                      #NSEP
    # C20 = if_else(CORE_CODE == '15000000060' | (str_detect(SCHOOL_COURSE_TITLE, "ROTC")), WGT, NULL),  #ROTC
    # C21 = if_else(GRADE_LEVEL == 0, WGT, NULL),
    # C22 = if_else(CORE_CODE == '25020000050', WGT, NULL),                    #Service Learning
    # C23 = if_else(Subject %in% c('Science', 'Math'), WGT, NULL),                 #USTAR
    # C99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Debt and Interest
    # D50 = if_else(STUDENT_ID != "", WGT, NULL),
    # D99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Extracurricular
    # E01 = if_else(str_detect(SCHOOL_COURSE_TITLE, "CONDI") |(str_detect(SCHOOL_COURSE_TITLE, "LA-COMP")), WGT, NULL), #Athletics
    # E02 = if_else(str_detect(SCHOOL_COURSE_TITLE, "LA-COND/FTBALL"), WGT, NULL),         #Football
    # E03 = if_else(SCHOOL_COURSE_TITLE == "LA-COND/BSOCCER" , WGT, NULL),                     #Soccer
    # E04 = if_else(str_detect(SCHOOL_COURSE_TITLE, "TENNIS"), WGT, NULL),                           #Tennis
    # E05 = if_else(str_detect(SCHOOL_COURSE_TITLE, "TRACK"), WGT, NULL),                            #Track
    # E06 = if_else(str_detect(SCHOOL_COURSE_TITLE, "CHEER"), WGT, NULL),                            #Cheer
    # E07 = if_else(str_detect(SCHOOL_COURSE_TITLE, "VOLLEYBALL"), WGT, NULL),                  #Volleyball
    # E09 = if_else(str_detect(SCHOOL_COURSE_TITLE, "WREST") , WGT, NULL),                          #Wrestling
    # E10 = if_else(str_detect(SCHOOL_COURSE_TITLE, "SWIM"), WGT, NULL),                             #Swimming
    # E11 = if_else(str_detect(SCHOOL_COURSE_TITLE, "GOLF"), WGT, NULL),                              #Golf
    # E12 = if_else(str_detect(SCHOOL_COURSE_TITLE, "BASEBALL"), WGT, NULL),                      #Baseball
    # E13 = if_else(str_detect(SCHOOL_COURSE_TITLE, "SOFTBALL"), WGT, NULL),                      #Softball
    # E14 = if_else(str_detect(SCHOOL_COURSE_TITLE, "BASKETBALL"), WGT, NULL),                 #Basketball
    # E15 = if_else(str_detect(SCHOOL_COURSE_TITLE, "DRILL"), WGT, NULL),                             #Drill
    # E16 = if_else(Subject2 == 'Band' , WGT, NULL),                                                                           #Band
    # E17 = if_else(STUDENT_ID != "", WGT, NULL),                                                                              #Other Student Clubs
    # E18 = if_else(STUDENT_ID != "", WGT, NULL),                                                                             #General Student Activities
    # E99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Instructional Support
    # F01 = if_else(TITLE_I_PROGRAM == "SW" | (TITLE_I_PROGRAM == "TA" & (MIGRANT == 1 | HOMELESS >= 1 | CHRONICALLY_ABSENT == 1 | LOW_INCOME == 1)), WGT, NULL),
    # F02 = if_else(GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # F03 = if_else(GRADE_LEVEL %in% c(7, 8), WGT, NULL),
    # F04 = if_else(GRADE_LEVEL %in% c(9, 10, 11, 12), WGT, NULL),
    # F05 = if_else(GRADE_LEVEL %in% c(7, 8, 9, 10, 11, 12), WGT, NULL),
    # F99 = if_else(STUDENT_ID != "", WGT, NULL),
    # G01 = if_else(GRADE_LEVEL == 1, WGT, NULL),
    # G02 = if_else(GRADE_LEVEL == 2, WGT, NULL),
    # G03 = if_else(GRADE_LEVEL == 3, WGT, NULL),
    # G04 = if_else(GRADE_LEVEL == 4, WGT, NULL),
    # G05 = if_else(GRADE_LEVEL == 5, WGT, NULL),
    # G06 = if_else(GRADE_LEVEL == 6, WGT, NULL),
    # G07 = if_else(GRADE_LEVEL == 7, WGT, NULL),
    # G08 = if_else(GRADE_LEVEL == 8, WGT, NULL),
    # G09 = if_else(GRADE_LEVEL == 9, WGT, NULL),
    # G10 = if_else(GRADE_LEVEL == 10, WGT, NULL),
    # G11 = if_else(GRADE_LEVEL == 11, WGT, NULL),
    # G12 = if_else(GRADE_LEVEL == 12, WGT, NULL),
    # G13 = if_else(GRADE_LEVEL == 0, WGT, NULL),
    # G14 = if_else(GRADE_LEVEL == -1, WGT, NULL),
    # G15 = if_else(STUDENT_ID != "", WGT, NULL),                                                           #School Land Trust
    # G99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Supplies
    # L01 = if_else(GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # L02 = if_else(GRADE_LEVEL %in% c(7, 8), WGT, NULL),
    # L03 = if_else(GRADE_LEVEL %in% c(9, 10, 11, 12), WGT, NULL),
    # L04 = if_else(GRADE_LEVEL %in% c(7, 8, 9, 10, 11, 12), WGT, NULL),
    # L99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Facilities Acquisition and Construction
    # M99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Nutrition
    # N01 = if_else(MEAL_COUNT > 0, MEAL_COUNT, NULL),
    # N99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Operations and School Administration
    # O05 = if_else(STUDENT_ID != "", WGT, NULL), #Operational costs of non-school buildings not including buildings related to child nutrition or transportation services.
    # O99 = if_else(STUDENT_ID != "", WGT, NULL),
    # Q99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Subject Related Expenses
    # S01 = if_else(STUDENT_ID != "", WGT, NULL),                                                                            #Aeronautics
    # S02 = if_else(Subject2 == 'Agriculture', WGT, NULL),
    # S03 = if_else(STUDENT_ID != "", WGT, NULL),                                                                            #Arts and Crafts
    # S04 = if_else(Subject == 'Language Arts', WGT, NULL),
    # S05 = if_else(Subject2 == 'Debate', WGT, NULL),
    # S06 = if_else(Subject2 == 'Journalism', WGT, NULL),
    # S07 = if_else(Subject2 == 'Theater' | str_detect(SCHOOL_COURSE_TITLE, "DEBATE") | str_detect(SCHOOL_COURSE_TITLE, "SPEECH"), WGT, NULL),                                                                                                       #Speech Drama
    # S08 = if_else(CORE_CODE == '02030000030', WGT, NULL),                                                     #Musical
    # S09 = if_else(Subject == 'Foreign Language', WGT, NULL),
    # S10 = if_else(Subject2 == 'Spanish', WGT, NULL),
    # S11 = if_else(Subject2 == 'German', WGT, NULL),
    # S12 = if_else(Subject2 == 'French', WGT, NULL),
    # S13 = if_else(Subject2 == 'Japanese', WGT, NULL),
    # S14 = if_else(Subject2 == 'Chinese', WGT, NULL),
    # S15 = if_else(Subject2 == 'Arabic', WGT, NULL),
    # S16 = if_else(Subject2 == 'FACS', WGT, NULL),                                                                            #Homemaking
    # S17 = if_else(str_detect(SCHOOL_COURSE_TITLE, "WOODWORKING") | (str_detect(SCHOOL_COURSE_TITLE, "WELDING")) | (str_detect(SCHOOL_COURSE_TITLE, "FURNITURE")) |
    #                 (str_detect(SCHOOL_COURSE_TITLE, "CARPENTRY")), WGT, NULL), #Industrial Arts
    # S18 = if_else(Subject == 'Mathematics', WGT, NULL),
    # S19 = if_else(Subject2 %in% c("Band", "Orchestra", "Music") | GRADE_LEVEL %in% c(1, 2, 3, 4, 5, 6), WGT, NULL),
    # S20 = if_else(Subject2 %in% c("Band", "Orchestra"), WGT, NULL),
    # S21 = if_else(Subject2 == 'Choir', WGT, NULL),
    # S22 = if_else(GRADE_LEVEL %in% c(1, 2, 3, 4, 5, 6), WGT, NULL),
    # S23 = if_else(Subject2 == "Art" | GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # S24 = if_else(Subject2 == "P.E." | GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # S25 = if_else(Subject2 == "Dance", WGT, NULL),                                                                        #Dance
    # S26 = if_else(Subject2 == "P.E.", WGT, NULL),                                                                             #High Adventure P.E.
    # S27 = if_else(STUDENT_ID != "", WGT, NULL),                                                                            #Reading
    # S28 = if_else(Subject == "Science", WGT, NULL),
    # S29 = if_else(Subject2 == "Biology", WGT, NULL),                                                                     #Biology
    # S30 = if_else(Subject2 == "Chemistry", WGT, NULL),                                                                #Chemistry
    # S32 = if_else(Subject == "Science", WGT, NULL),                                                                       #Physical Science
    # S33 = if_else(Subject == "Social Studies", WGT, NULL),
    # S34 = if_else(Subject2 == "Theater", WGT, NULL),                                                                    #Stage
    # S35 = if_else(Subject2 == "Yearbook", WGT, NULL),
    # S36 = if_else(DLI == 1, WGT, NULL),                                                                                             #Critical Languages
    # S37 = if_else(DLI == 1 , WGT, NULL),
    # S38 = if_else(CORE_CODE %in% c('01000000100', '34010000005'), WGT, NULL),              #Financial Literacy
    # S39 = if_else(GRADE_LEVEL %in% c(4, 5, 6), WGT, NULL),
    # S40 = if_else(Subject %in% c("Science", "Mathematics"), WGT, NULL),
    # S41 = if_else(GRADE_LEVEL %in% c(1, 2, 3, 4, 5, 6), WGT, NULL),
    # 
    # # Transportation
    # T01 = if_else(BUS_ELIGIBLE == 1, WGT, NULL),
    # T08 = if_else(STUDENT_ID != "", WGT, NULL),
    # T09 = if_else(BUS_ELIGIBLE == 1 & GRADE_LEVEL %in% c(7, 8, 9, 10, 11, 12), WGT, NULL),
    # T10 = if_else(BUS_ELIGIBLE == 1 & GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6), WGT, NULL),
    # T11 = if_else(SPECIAL_ED == 'Y', WGT, NULL),
    # 
    # # Student Related Expenses
    # U01 = if_else(SPECIAL_ED == 'Y', WGT, NULL),
    # U02 = if_else(SPECIAL_ED == 'Y' & GRADE_LEVEL == -1, WGT, NULL),
    # U03 = if_else(SPECIAL_ED == 'Y', WGT, NULL),                                                                           #Extended Year Disabled
    # U04 = if_else(SPECIAL_ED == 'Y', WGT, NULL),                                                                           #Extended Year SPED Teachers
    # U05 = if_else(GRADE_LEVEL %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8), WGT, NULL),
    # U06 = if_else(GIFTED == 'Y', WGT, NULL),                                                                                    #Accelerated Students
    # U07 = if_else((GRADE_LEVEL == 12 & HS_COMPLETION_STATUS %in% c('GA', 'GR', 'G3', 'GQ', 'GM') & MEMBERSHIP_DAYS < 135 & CHRONICALLY_ABSENT == 0) | 
    #                 (GRADE_LEVEL == 11 & HS_COMPLETION_STATUS %in% c('GA', 'GR', 'G3', 'GQ', 'GM')), WGT, NULL), #Centennial Scholarship for Early Graduation 
    # # Use 12th graders there for less than 3/4 of the year (can graduate up to Q3), or for any 11th grader with a HS Completion Code
    # U08 = if_else(ELL_STATUS == 'Y' | CHRONICALLY_ABSENT == 1 | GPA < 2.0 | GPA_POINTS < 2.0, WGT, NULL),      #At Risk
    # U09 = if_else(YIC_MEMBERSHIP_DAYS > 0, WGT, NULL),                                                          #Youth in Custody
    # U10 = if_else(INSTITUTIONAL_SETTING == "IS", WGT, NULL),                                                #Home School
    # U11 = if_else(RACE_AMERICAN_INDIAN == 1, WGT, NULL),
    # U12 = if_else(GRADE_LEVEL == -1, WGT, NULL),                                                                       #Early Interventions
    # U13 = if_else((GRADE_LEVEL == 12 & HS_COMPLETION_STATUS %in% c('GA', 'GR', 'G3', 'GQ', 'GM') & MEMBERSHIP_DAYS < 135 & CHRONICALLY_ABSENT == 0) | 
    #                 (GRADE_LEVEL == 11 & HS_COMPLETION_STATUS %in% c('GA', 'GR', 'G3', 'GQ', 'GM')), WGT, NULL), #Early Graduation
    # U14 = if_else(LOW_INCOME == 1, WGT, NULL),                                                                        #Intergenerational Poverty
    # U15 = if_else(GRADE_LEVEL == -1, WGT, NULL),                                                                       #UPSTART
    # U16 = if_else(GRADE_LEVEL %in% c(-1, 0, 1, 2, 3), WGT, NULL),
    # U17 = if_else(GRADE_LEVEL == 4, WGT, NULL),                                                                        #Capitol Field Trips
    # U18 = if_else(GIFTED == 'Y', WGT, NULL),                                                                                  #Gifted and Talented
    # U19 = if_else(RACE_AMERICAN_INDIAN == 1, WGT, NULL),                                                  #Indian Education
    # U20 = if_else(LOW_INCOME == 1, WGT, NULL),                                                                       #TANF
    # U21 = if_else(STUDENT_ID != "", WGT, NULL),                                                                         #Accelerated Readers
    # U22 = if_else(SPECIAL_ED == 'Y', WGT, NULL),
    # U23 = if_else(SPECIAL_ED == 'Y' & GRADE_LEVEL == -1, WGT, NULL),
    # U24 = if_else(DISABILITY_CODE_1 == 'DB' | DISABILITY_CODE_2 == 'DB' | DISABILITY_CODE_3 == 'DB', WGT, NULL),  #Deaf Blind
    # U25 = if_else(ELL_STATUS == 'Y', WGT, NULL),
    # U26 = if_else(ELL_STATUS == 'Y', WGT, NULL),                                                                          #Migrant
    # U27 = if_else(ENVIRONMENT_1 == 'H' | ENVIRONMENT_2 == 'H', WGT, NULL),            #Homebound
    # U28 = if_else(GRADE_LEVEL > 9, WGT, NULL),                                                                         #ACT Test Reimbursement
    # U29 = if_else(HOMELESS >= 1, WGT, NULL),
    # U30 = if_else(LOW_INCOME == 1, WGT, NULL),                                          # Low income
    # U31 = if_else(STUDENT_ID != "", WGT, NULL),
    # U99 = if_else(STUDENT_ID != "", WGT, NULL),
    # 
    # # Non K-12, Miscellaneous, Depreciation, and District Specific
    # X99 = if_else(STUDENT_ID != "", WGT, NULL),
    # Y99 = if_else(STUDENT_ID != "", WGT, NULL),
    # Z01 = if_else(STUDENT_ID != "", WGT, NULL),     #District Specific
    # Z99 = if_else(STUDENT_ID != "", WGT, NULL)
  )


# ___________________________________________________________________________________

#### CHECK STUDENT ALLOCATION ####

## subcode allocation checks
money <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Allocation_Matrix_", lea_ids$district_code, ".csv"))
money %<>% filter(fiscal_year >= min_year & fiscal_year <= max_year) %>% mutate(u_location = as.character(u_location))

subcodes <- setdiff(names(all3), names(all2)) # get list of subcodes with allocation rules above

# check whether there are extra or missing subcodes -- make sure both of these are null before proceeding
sort(setdiff(money$subcode, subcodes)) # subcodes listed in money file but without subcode rules - add rules above
sort(setdiff(subcodes, money$subcode)) # subcodes listed in code above but with no money - delete these or comment out above

# save expense totals to compare later in code
expense_total <- money %>% summarize(total = sum(total)) %>% pull(total)
expense_cd <- money %>% filter(!subcode %in% c("X99", "Y99")) %>% summarize(total = sum(total)) %>% pull(total)
expense_nocd <- money %>% filter(!subcode %in% c("D50", "X99")) %>% summarize(total = sum(total)) %>% pull(total)

# create a wide money file that is more conducive to the student allocation formatting
money_wide <- money %>% arrange(subcode) %>% pivot_wider(names_from = subcode, values_from = total)
money_wide[is.na(money_wide)] <- 0


## allocation explainer checks

explainer <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name,"/Allocation/Allocation_Explanation_", lea_ids$district_code, ".csv"))

# check if there are subcodes present in the expense data that are lacking an allocation explanation in the explainer
money %>% filter(!subcode %in% explainer$subcode & !subcode %in% c("T08", "T09", "T10")) %>% distinct(subcode) %>% arrange(subcode) # make sure this is null before proceeding

# check if any other fields are null
explainer %>% filter(is.na(Expense_Category) | is.na(`Allocated To`) | (is.na(Why) & !str_detect(str_to_title(`Allocated To`), "Not Allocated"))) # make sure this is null before proceeding

# ___________________________________________________________________________________


#### ALLOCATE LOCATION-SPECIFIC EXPENSES ####

# sum up total student weight by location/year/subcode
location_weight <- all3 %>%
  group_by(SCHOOL_NUMBER, SCHOOL_YEAR) %>%
  summarize_at(subcodes, sum, na.rm = T) %>% 
  rename_at(subcodes, funs(paste0("LW_", .)))

# get list of schools present in the course data
school_locations <- all3 %>%
  distinct(SCHOOL_YEAR, SCHOOL_NUMBER) %>%
  mutate(school = 1)

# subset money on school locations that are present in the student course data
location_money <- money_wide %>%
  rename_at(subcodes, funs(paste0("LM_", .))) %>%
  left_join(school_locations, by = c("fiscal_year" = "SCHOOL_YEAR", "u_location" = "SCHOOL_NUMBER")) %>% 
  filter(!is.na(school)) %>%
  select(-school)

# create smaller allocation file subset on its keys: STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID (and student weights by subcode)
allocation <- all3 %>% select(STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID, subcodes)

allocation %<>% left_join(location_weight, by = c("SCHOOL_YEAR", "SCHOOL_NUMBER"))
allocation %<>% left_join(location_money, by = c("SCHOOL_YEAR" = "fiscal_year", "SCHOOL_NUMBER" = "u_location"))

# calculate each student's share of funding based on their student weight and total money in the subcode
for(i in 1:length(subcodes)){
  student_share <- allocation[, subcodes[i]] / allocation[, paste0("LW_", subcodes[i])] # get each student's proportion (share) out of total weights for the location/year/subcode
  student_allocated <- allocation[, paste0("LM_", subcodes[i])] * student_share # get the amount of money allocated to each student
  
  allocation[, paste0("L_", subcodes[i])] <- student_allocated
}

allocation %<>% select(STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID, starts_with("L_"))
allocation[is.na(allocation)] <- 0

all3 %<>% left_join(allocation, by = c("STUDENT_ID", "SCHOOL_YEAR", "SCHOOL_NUMBER", "CLASS_ID"))



#### ALLOCATE DISTRICT-WIDE EXPENSES ####


# ***********************************************************************************
# DISTRICT-SPECIFIC ADJUSTMENTS - ADJUST DISTRICT-SPECIFIC SUBCODES


# ***********************************************************************************

# sum up total student weight by year/subcode only
district_weight <- all3 %>%
  group_by(SCHOOL_YEAR) %>%
  summarize_at(subcodes, sum, na.rm = T) %>% 
  rename_at(subcodes, funs(paste0("DW_", .)))

# subset money on locations not present in the student course data
district_money <- money_wide %>% left_join(school_locations, by = c("fiscal_year" = "SCHOOL_YEAR", "u_location" = "SCHOOL_NUMBER")) %>% 
  filter(is.na(school)) %>% select(-school)

district_money %<>% group_by(fiscal_year) %>% 
  summarize_at(subcodes, sum, na.rm = T) %>% 
  rename_at(subcodes, funs(paste0("DM_", .))) %>% 
  ungroup()

# create smaller allocation file subset on its keys: STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID (and student weights by subcode)
district_allocation <- all3 %>% select(STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID, subcodes)

district_allocation %<>% left_join(district_weight, by = "SCHOOL_YEAR")
district_allocation %<>% left_join(district_money, by = c("SCHOOL_YEAR" = "fiscal_year"))

# calculate each student's share of funding based on their student weight and total money in the subcode
for(i in 1:length(subcodes)){
  student_share <- district_allocation[, subcodes[i]] / district_allocation[, paste0("DW_", subcodes[i])] # get each student's proportion (share) out of total weights
  student_allocated <- district_allocation[, paste0("DM_", subcodes[i])] * student_share # get the amount of money allocated to each student
  
  district_allocation[, paste0("D_", subcodes[i])] <- student_allocated
}

district_allocation %<>% select(STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER, CLASS_ID, starts_with("D_"))
district_allocation[is.na(district_allocation)] <- 0

all3 %<>% left_join(district_allocation, by = c("STUDENT_ID", "SCHOOL_YEAR", "SCHOOL_NUMBER", "CLASS_ID"))

remove(allocation, district_allocation, location_weight, district_weight, money_wide, student_share, student_allocated, i)


# ___________________________________________________________________________________

#### CHECK SUBCODE ALLOCATION ####

# location-specific allocation checks

location_check <- all3 %>% 
  select(SCHOOL_YEAR, SCHOOL_NUMBER, starts_with("L_")) %>% 
  group_by(SCHOOL_YEAR, SCHOOL_NUMBER) %>% 
  summarize_all(sum, na.rm = T) %>% 
  pivot_longer(starts_with("L_"), names_to = "Subcode", values_to = "Allocated") %>% 
  mutate(Subcode = str_sub(Subcode, 3)) %>% 
  ungroup()
  
location_money %<>% 
  pivot_longer(starts_with("LM_"), names_to = "Subcode", values_to = "Money") %>% 
  mutate(Subcode = str_sub(Subcode, 4))

location_check %<>% full_join(location_money, by = c("SCHOOL_YEAR" = "fiscal_year", "SCHOOL_NUMBER" = "u_location", "Subcode")) %>% 
  select(Subcode, SCHOOL_YEAR, SCHOOL_NUMBER, Money, Allocated) %>% 
  arrange(Subcode, SCHOOL_YEAR, SCHOOL_NUMBER) %>%
  mutate(Check = round(Money - Allocated)) %>%
  filter(abs(Check) > 0)

# district-wide allocation checks

district_check <- all3 %>% 
  select(SCHOOL_YEAR, starts_with("D_")) %>% 
  group_by(SCHOOL_YEAR) %>% 
  summarize_all(sum, na.rm = T) %>% 
  pivot_longer(starts_with("D_"), names_to = "Subcode", values_to = "Allocated") %>% 
  mutate(Subcode = str_sub(Subcode, 3)) %>% 
  ungroup()

district_money %<>% 
  pivot_longer(starts_with("DM_"), names_to = "Subcode", values_to = "Money") %>% 
  mutate(Subcode = str_sub(Subcode, 4))

district_check %<>% full_join(district_money, by = c("SCHOOL_YEAR" = "fiscal_year", "Subcode")) %>% 
  select(Subcode, SCHOOL_YEAR, Money, Allocated) %>% 
  arrange(Subcode, SCHOOL_YEAR) %>% 
  mutate(Check = round(Money - Allocated)) %>%
  filter(abs(Check) > 0)

# check whether there is money not being allocated -- make sure both of these are null before proceeding
district_check %>% distinct(Subcode) %>% pull(Subcode)
location_check %>% distinct(Subcode) %>% pull(Subcode)

# write_csv(district_check, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Working Files/LS Check.csv"))
# write_csv(location_check, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Working Files/DW Check.csv"))


# max subcode allocation per student check

# aggregate student spending to subcode level of detail
spending_cat <- read_excel(paste0("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/Subcodes and Expense Categories ", max_year, ".xlsx"))
tableau_cat <- spending_cat %>% distinct(TC_NAME) %>% filter(!is.na(TC_NAME)) %>% pull(TC_NAME) # save full list of tableau categories
spending_cat %<>% filter(SC %in% subcodes)

# add up totals for subcodes, regardless of if they're spent at the school or district level
for (i in 1:length(subcodes)) {
  all3[, spending_cat$Expense_Category[i]] <- all3[, paste0("L_", subcodes[i])] + all3[, paste0("D_", subcodes[i])]
}

all3 %<>% select(-c(starts_with("L_"), starts_with("D_")))

# max subcode allocation
subcode_check <- all3 %>% 
  group_by(STUDENT_ID, SCHOOL_YEAR, SCHOOL_NUMBER) %>% 
  summarize_at(spending_cat$Expense_Category, sum, na.rm = T) %>% 
  ungroup()

subcode_check %<>% 
  pivot_longer(spending_cat$Expense_Category, names_to = "Category", values_to = "Amount") %>% 
  group_by(SCHOOL_YEAR, SCHOOL_NUMBER, Category) %>% 
  summarize(Max = round(max(Amount), 2), Mean = round(mean(Amount), 2), Median = round(median(Amount), 2)) %>% 
  ungroup()

subcode_check %>% arrange(desc(Max))
  
# write_csv(subcode_check, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Working Files/Subcode Check.csv"))

# ___________________________________________________________________________________



#### CATEGORIZE STUDENT EXPENSES ####

all4 <- all3 %>% mutate(Teacher_Pay = A_WGTC)

# ***********************************************************************************
# DISTRICT-SPECIFIC ADJUSTMENTS - RENAME Z-CODES AND ADD TO PARENT CODES

# all4 %<>%
#   rename(`Z01 Label` = `Dependent on District 1`)
# 
# spending_cat %<>%
#   mutate(Expense_Category = case_when(Expense_Category == "Dependent on District 1" ~ "Z01 Label",
#                                       TRUE ~ Expense_Category)) %>% 
#   mutate(TC_NAME = ifelse(SC %in% c("Z01"), "Instructional Related Expenses", TC_NAME))
# leave TC_NAME NA if parent code is non K12

# ***********************************************************************************


## aggregate to parent code level of detail where we feel comfortable making global comparisons

all4[, tableau_cat] <- NA

for(i in 1:length(tableau_cat)){
  pc_cat <- spending_cat$Expense_Category[which(spending_cat$TC_NAME == tableau_cat[i])]
  all4[, tableau_cat[i]] <- ifelse(rep(!is_empty(pc_cat), nrow(all4)), rowSums(all4[, pc_cat], na.rm = T), 0)
}


# create course subject expense category to differentiate in-class spending
course_subject <- spending_cat %>% filter(Course_Subject_Expense == "Y") %>% pull(Expense_Category)
all4$Course_Subject_Expense <- rowSums(all4[, course_subject], na.rm = T)

all4$Total_Expense_C <- rowSums(all4[, c(tableau_cat, "Teacher_Pay")], na.rm = T)


# sum up total expenses

all4 %<>%
  group_by(SCHOOL_YEAR, STUDENT_ID) %>%
  mutate(Total_Expense_S = sum(Total_Expense_C, na.rm = T)) %>%
  ungroup()

all4 %<>%
  group_by(CLASS_ID) %>%
  mutate(Teacher_Pay_C = sum(Teacher_Pay, na.rm = T), 
         Total_WGT = sum(WGT, na.rm = T)) %>%
  ungroup()

all4 %<>%
  mutate(all_nocd = Total_Expense_C,
         all_cd = Total_Expense_C + `Actual Cost` - `Depreciated Expense`)



#### CLEAN TESTING DATA ####

# check if there are testing scores where the RISE/ASPIRE data is considered "not sufficient" by USBE but still have scores included - this will inform toggle option later on
# all4 %>%
#   filter(SIS_SUFF == 0 & !is.na(SPROF) | LIS_SUFF == 0 & !is.na(LPROF) | MIS_SUFF == 0 & !is.na(MPROF))%>%
#   select(SCHOOL_YEAR, SCHOOL_TYPE, SIS_SUFF, SPROF, LIS_SUFF, LPROF, MIS_SUFF, MPROF)

# add proficiency labels

prof <- all4 %>% select(ends_with("PROF")) %>% names() 

all4[, paste0(prof, "_L")] <- NA

for(i in 1:length(prof)){
  all4[, prof[i]] <- round(all4[, prof[i]], 0)
  
  all4[, paste0(prof[i], "_L")] <-  case_when(
    all4[, prof[i]] == 1 ~ "Below Proficient",
    all4[, prof[i]] == 2 ~ "Approaching Proficient",
    all4[, prof[i]] == 3 ~ "Proficient",
    all4[, prof[i]] == 4 ~ "Highly Proficient",
    is.na(all4[, prof[i]]) ~ "No Score"
  )
}

all4 %<>%
  mutate(SGP = case_when(
      subject_of_class == "Language Arts" ~ LSGP,
      subject_of_class == "Mathematics" ~ MSGP,
      subject_of_class == "Science" ~ SSGP,
      is.na(LSGP) & is.na(MSGP) & is.na(SSGP) ~ round(SGP_MEAN, 2)))

all4 %<>%
  mutate(mean_l = if_else(lab_mean_l != "Insufficient Data", mean_l, NULL),
         mean_s = if_else(lab_mean_s != "Insufficient Data", mean_s, NULL),
         mean_m = if_else(lab_mean_m != "Insufficient Data", mean_m, NULL),
         mean_rw = if_else(lab_mean_rw != "Insufficient Data", mean_rw, NULL),
         mean_t = if_else(lab_mean_t != "Insufficient Data", mean_t, NULL),
         mean_e = if_else(lab_mean_e != "Insufficient Data", mean_e, NULL))


test_scores <- all4 %>% select(contains(c("PROF", "PROF_L", "SCORE", "SGP", "YN")), -c(AP_TEST_SCORE)) %>% names()

all4 %<>% 
  group_by(STUDENT_ID, SCHOOL_YEAR, DISTRICT_ID, school_number_mode_s, school_name_mode_s, school_type_mode_s) %>% 
  fill(test_scores, .direction = "downup") %>% 
  ungroup()



#### EXPORT FILES ####

### final checks

# Compare expense totals (no capital/no teacher pay) between original expense matrix and all4 file
c(round(expense_nocd), round(sum(all4$all_nocd) - sum(all4$Teacher_Pay, na.rm = T)))

# Compare expense totals (with capital/no teacher pay) between original expense matrix and all4 file
c(round(expense_cd), round(sum(all4$all_cd, na.rm  = T) - sum(all4$Teacher_Pay, na.rm = T))) 


### finalize allocation file

# recode variables
all4 %<>% 
  mutate(
    ELL_STATUS = if_else(ELL_STATUS == "Y", "ELL", "Not ELL", "Not ELL"),
    SPECIAL_ED = if_else(SPECIAL_ED == "Y", "Special Ed", "Not Special Ed", "Not Special Ed"),
    LOW_INCOME_YN = if_else(LOW_INCOME == 1, "Low Income", "Not Low Income", "Not Low Income"),
    Title_I = if_else(TITLE_I == 1, 1, 0, 0),
    MIGRANT_STATUS = if_else(MIGRANT == 1, "Migrant", "Not Migrant"),
    MOBILE = if_else(MOBILE == 1, "Mobile", "Not Mobile"),
    CHRONICALLY_ABSENT = if_else(CHRONICALLY_ABSENT == 0 | is.na(CHRONICALLY_ABSENT), "Not Chronically Absent", "Chronically Absent"),
    HOMELESS = case_when(
      HOMELESS == 0 ~ "Not homeless",
      HOMELESS == 1 ~ "With another family member because of loss of housing or economic hardship",
      HOMELESS == 2 ~ "In a motel or hotel",
      HOMELESS == 3 ~ "In a shelter (emergency, transitional, or domestic violence)",
      HOMELESS == 4 ~ "In a car, park, campground, or public place",
      HOMELESS == 5 ~ "Somewhere without adequate facilities (running water, heat, electricity)",
      is.na(HOMELESS) ~ "Not Homeless"),
    HOMELESS_YN = if_else(HOMELESS != 0 & !is.na(HOMELESS), "Homeless", "Not Homeless"),
    SCHOOL_YEAR_STRING = as.character(SCHOOL_YEAR),
    Grade_LEVEL_STRING = as.character(GRADE_LEVEL)
  ) %>%
  rename(TEACHER_NAME = NAME) %>% 
  select(-TITLE_I)


## clean up subcode variables
# remove subcodes containing student weights
all4 %<>% select(-spending_cat$SC)

# rename using subcodes, not subcode descriptions
all4 %<>% rename_at(vars(spending_cat$Expense_Category), ~ spending_cat$SC)

# impute all other subcodes
full_cat <- read_excel(paste0("H:/Economists/Ed/KIDS/All LEAs/UCOA and Subcodes/Subcodes and Expense Categories ", max_year, ".xlsx"))

charter_cat <- read_excel(paste0("H:/Economists/Ed/KIDS/All Charter Schools/All/UCOA and Subcodes/Charter Subcodes and Expense Categories ", max_year, ".xlsx"))
charter_cat %<>% filter(!SC %in% full_cat$SC) # get charter-specific subcodes

full_cat %<>% bind_rows(charter_cat) %>% filter(!is.na(SC)) %>% arrange(SC) # join missing district subcodes

missing_cat <- full_cat %>% filter(!SC %in% spending_cat$SC) # remove subcodes that are already present in allocation data

missing_cat %<>% select(SC) %>% mutate(money = 0) %>% spread(SC, money)
all4 %<>% bind_cols(missing_cat)


## finalize allocation file
# read in schema table
schema <- read_csv("H:/Economists/Ed/KIDS/All LEAs/Database/database_schema/exports_v3/class_schema.csv")
fields <- schema %>% pull(field)

# add identifier variables
all4 %<>% mutate(lea_name = lea_ids$entity_name, lea_id = lea_ids$district_id, lea_code = lea_ids$district_code)

all4 %<>% select(fields) %>% relocate(fields) # order variables using schema table

ncol(all4) == 374 # check correct number of columns

# write out allocation file for database upload
fwrite(all4,paste0("H:/Economists/Ed/KIDS/All LEAs/Database/exports_v3/allocation/", tolower(lea_ids$district_code), "_class.txt"), 
       sep = "|", col.names = TRUE, na = "", quote = FALSE) 
