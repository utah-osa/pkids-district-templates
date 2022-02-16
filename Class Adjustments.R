# load in functions for general class assignment
source("H:/Economists/Ed/KIDS/All LEAs/Class Assignment/Class_Assignment_Functions.R")

# load in functions for student, class, and testing labels
source("H:/Economists/Ed/KIDS/All LEAs/Statewide Merge/Statewide_Labels_Functions2014-2019.R")

########################
##### GET LEA INFO #####

curr_lea <- "[lea code]"

lea_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv", col_types = cols())
lea_ids %<>% mutate(district_location = str_pad(district_location, 3, "left", "0"))
lea_ids %<>% filter(district_code == curr_lea)

setwd(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files"))

##############################
##### IMPORT & PREP DATA #####

### class data file

sm <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/sm_", lea_ids$district_code, ".RDS")) 
max_year <- max(sm$SCHOOL_YEAR)

# only keep grades K-12
sm %<>% subset(GRADE_LEVEL != -1 | is.na(GRADE_LEVEL))

# separate into individual files for adjustments
studentbyclass <- sm %>% select(STUDENT_ID:GPA, GRADE_LEVEL, TOTAL_MEMBERSHIP, ATTENDANCE) 
e <- sm %>% select(c(STUDENT_ID, SCHOOL_YEAR, DISTRICT_ID, SCHOOL_ID, SCHOOL_NUMBER, SS_ID:HOMELESS)) %>% distinct(.keep_all = TRUE)
scram <- sm %>% select(STUDENT_ID, SCHOOL_YEAR, DISTRICT_ID, SCHOOL_ID, DISABILITY_CODE_1:ENVIRONMENT_3) %>% distinct(.keep_all = TRUE)
st_testing <- sm %>% select(STUDENT_ID, SCHOOL_YEAR, DISTRICT_ID, SCHOOL_ID, SCHOOL_NUMBER, WHICH_TEST:ACA_ISREADINGGRADELEVEL_EOY) %>% distinct(.keep_all = TRUE)
act_testing <- sm %>% select(DISTRICT_ID, SCHOOL_YEAR, STUDENT_ID, ACT_COMPOSITE:ACT_SCIENCE) %>% distinct(.keep_all = TRUE)
ap_testing <- sm %>% select(STUDENT_ID,DISTRICT_ID,SCHOOL_ID,SCHOOL_NUMBER,SCHOOL_YEAR,CORE_CODE,AP_TEST_NAME:AP_TEST_SCORE) %>% distinct(.keep_all = TRUE)
student_info <- sm %>% select(STUDENT_ID, last_name:SSID) %>% distinct(.keep_all = TRUE)
core_code_all <- sm %>% select(CORE_CODE, CORE_SHORT_DESC:DLI) %>% distinct(.keep_all = TRUE)
school_info <- sm %>% select(DISTRICT_ID, SCHOOL_NUMBER, SCHOOL:SCHOOL_TYPE) %>% distinct(.keep_all = TRUE)
title_I <- sm %>% select(DISTRICT_ID, SCHOOL_ID, SCHOOL_YEAR, TITLE_I, TITLE_I_PROGRAM) %>% distinct(.keep_all = TRUE)

### payroll data files

Teacher_Payroll <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/Teacher_Payroll_", lea_ids$district_code, ".RDS"))
Payroll_With_Incourse <- readRDS(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/", lea_ids$district_code, "_Payroll_With_Incourse.RDS"))

# create a list of payroll names (to make variable selections in the code)
payroll_names <- names(Payroll_With_Incourse)

### teacher exclusions file

exclusions <- read.csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/Working Files/", lea_ids$district_code, "_TEACHER_EXC.csv"))
exclusions %<>% select(TEACHER_ID, YEARS) %>% mutate(YEARS = toupper(YEARS))

### location crosswalk file

Loc_Cross <- read_csv(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Other Data and Resources/", lea_ids$district_code, "_Loc_Cross.csv"), col_types = cols(location = col_character()))
Loc_Cross %<>% select(location, school_type_mode_s = Level)

# ___________________________________
# ___________________________________
# DISTRICT-SPECIFIC CLASS ADJUSTMENTS 

### Class corrections ###
# ...

# calculate the weights and class size
studentbyclass %<>%
  # FIND NUMBER OF STUDENTS IN EACH CLASS
  group_by(CLASS_ID) %>% mutate(NSTUD_CLASS = n()) %>% ungroup() %>%
  # ASSIGN WEIGHT FOR % OF CLASS STUDENTS ATTENDED
  mutate(WGT_WD = T_SCALE * (CLENGTH_STUDENT/CLENGTH_CLASS),
         WGT_WD = if_else(is.na(WGT_WD), 0, WGT_WD)) %>%
  # ASSIGN WEIGHT THAT ADDS UP TO 1 FOR EACH CLASS
  group_by(CLASS_ID) %>% mutate(WGT_WD_CLASS = sum(WGT_WD)) %>% ungroup() %>%
  mutate(WGT_STD_C = (WGT_WD / WGT_WD_CLASS),
         WGT_STD_C = if_else(is.na(WGT_STD_C), 0, WGT_STD_C)) %>%
  # ASSIGN WEIGHT THAT ADDS UP TO 1 FOR EACH STUDENT EACH YEAR, IF IN THE DISTRICT FOR THE WHOLE YEAR
  group_by(DISTRICT_ID, STUDENT_ID, SCHOOL_YEAR) %>% mutate(SUM_CLENGTH_STUDENT = sum(T_SCALE*CLENGTH_STUDENT),
                                                            TOTAL_CLENGTH_STUDENT = max(END_STUDENT)-min(START_STUDENT)) %>% ungroup() %>%
  mutate(WGT = (T_SCALE*CLENGTH_STUDENT/SUM_CLENGTH_STUDENT) * (TOTAL_CLENGTH_STUDENT),
         WGT = if_else(is.na(WGT), 0, WGT)) %>% 
  # ASSIGN WEIGHT THAT ADDS UP TO 1 FOR EACH STUDENT EACH YEAR, IF IN THE DISTRICT FOR THE WHOLE YEAR AND IF PERFECT ATTENDANCE
  mutate(WGT_ATTEND = (T_SCALE*CLENGTH_STUDENT/SUM_CLENGTH_STUDENT) * (ATTENDANCE/TOTAL_MEMBERSHIP) * (TOTAL_CLENGTH_STUDENT),
         WGT_ATTEND = if_else(is.na(WGT_ATTEND), 0, WGT_ATTEND)) %>% 
  select(-SUM_CLENGTH_STUDENT,TOTAL_CLENGTH_STUDENT)

studentbyclass <- studentbyclass[,c(names(sm %>% select(STUDENT_ID:INEX_VIZ_CLASS)), "WGT_WD", "WGT_STD_C","WGT","WGT_ATTEND",
                                    names(sm %>% select(NSTUD_CLASS:GPA)))]

# ___________________________________
# ___________________________________

######################################################
##### REMERGE ALL FIELDS FOR STUDENT/COURSE DATA #####

smd <- studentbyclass %>% select(-c(ENTRY_SCHOOL, EXIT_SCHOOL, NDAYS_SCHOOL))
smd %<>% left_join(e, by = c("STUDENT_ID", "SCHOOL_YEAR", "DISTRICT_ID", "SCHOOL_ID", "SCHOOL_NUMBER"))
smd %<>% left_join(scram, by = c("STUDENT_ID", "SCHOOL_YEAR", "DISTRICT_ID", "SCHOOL_ID"))
smd %<>% left_join(st_testing, by = c("STUDENT_ID", "SCHOOL_YEAR", "DISTRICT_ID", "SCHOOL_ID", "SCHOOL_NUMBER"))
smd %<>% left_join(act_testing, by = c("DISTRICT_ID", "SCHOOL_YEAR", "STUDENT_ID"))
smd %<>% left_join(ap_testing, by = c("STUDENT_ID","DISTRICT_ID","SCHOOL_ID","SCHOOL_NUMBER","SCHOOL_YEAR","CORE_CODE"))
smd %<>% left_join(student_info, by = c("STUDENT_ID"))
smd %<>% left_join(core_code_all, by = c("CORE_CODE"))
smd %<>% left_join(school_info, by = c("DISTRICT_ID", "SCHOOL_NUMBER"))
smd %<>% left_join(title_I, by = c("DISTRICT_ID", "SCHOOL_ID", "SCHOOL_YEAR"))
smd %<>% left_join(fl_testing, by = c("SSID", "SCHOOL_YEAR"))

# create class- and student-aggregated variables for visualization
smd <- class_labels(smd)
smd <- student_labels(smd)

# add in the school type
smd %<>% left_join(Loc_Cross %>% rename(SCHOOL_TYPE = school_type_mode_s), by = c("SCHOOL_NUMBER" = "location"))
smd %<>% left_join(Loc_Cross, by = c("school_number_mode_s" = "location"))

# make sure all locations have been added to the crosswalk (if any are missing, add to csv file)
smd %>% subset(is.na(SCHOOL_TYPE)) %>% distinct(SCHOOL_NUMBER, SCHOOL)
smd %>% subset(is.na(school_type_mode_s)) %>% distinct(school_number_mode_s, school_name_mode_s)

# add testing labels for visualization
smd <- test_labels(smd)

# calculate weighted number of students in each class
smd %<>% group_by(CLASS_ID) %>% mutate(no_stus = sum(WGT_WD)) %>% ungroup()

smd %<>% select(STUDENT_ID:SCHOOL,SCHOOL_TYPE,TITLE_I:school_name_mode_s,school_type_mode_s:no_stus)

# ______________________________________________
# ______________________________________________
# ADDITIONAL DISTRICT-SPECIFIC CLASS ADJUSTMENTS 

# ______________________________________________
# ______________________________________________

##############################################################
##### EXCLUDE TEACHERS FROM BOTH PAYROLL AND CLASS FILES #####

### exclude teachers 

for(year in 2014:max_year){
  teachers <- exclusions %>% filter(str_detect(YEARS, paste0("ALL|", year))) %>% pull(TEACHER_ID)
  
  # class file
  smd %<>% mutate(INEX_CLASS = ifelse(TEACHER_ID %in% teachers & SCHOOL_YEAR %in% year, 0, INEX_CLASS))
  
  # payroll files
  Payroll_With_Incourse %<>% mutate(incourse = if_else(TEACHER_ID %in% teachers & Fiscal_Year == year, 0, incourse, incourse))
  
  Teacher_Payroll %<>%
    mutate(noninst_benefits = if_else(TEACHER_ID %in% teachers & SCHOOL_YEAR == year, inst_benefits+noninst_benefits, noninst_benefits),
           inst_pay = if_else(TEACHER_ID %in% teachers & SCHOOL_YEAR == year, 0, inst_pay),
           inst_benefits = if_else(TEACHER_ID %in% teachers & SCHOOL_YEAR == year, 0, inst_benefits),
           total_instcomp = if_else(TEACHER_ID %in% teachers & SCHOOL_YEAR == year, 0, total_instcomp))
}

###############################################
##### PREP PAYROLL FILES FOR FINAL EXPORT #####

### prepare file for export to expense code

Payroll_With_Incourse %<>%
  select("fiscal_year"=Fiscal_Year,"total"=Amount,"name"=Name,"cactus_id"=TEACHER_ID,Employee_ID,
         u_fund,u_location,u_program,u_function,u_object,incourse,"account_description"=Description,
         all_of(payroll_names[str_detect(toupper(payroll_names), "DESC") & payroll_names != "Description"]))

### prepare file for export to allocation code

Teacher_Payroll %<>%
  select(TEACHER_ID,"NAME"=Cactus_Name,"LICENSE"=license_level,"DEGREE"=degree_summary,
         "NONINTERN_YRS"=nonintern_yrs,"INTERN_YRS"=intern_yrs,"INTERN"=intern_flag,SCHOOL_YEAR,
         "INST_BENEFITS"=inst_benefits,"INST_PAY"=inst_pay,
         "INST_TOTALCOMP"=total_instcomp,"TOTAL_COMP"=total_comp)

# check that incourse totals are equal between the final allocation & expense files
sum(Payroll_With_Incourse$total[Payroll_With_Incourse$incourse == 1]) == sum(Teacher_Payroll$INST_TOTALCOMP, na.rm = TRUE)
# check that incourse totals are fairly consistent across years
Payroll_With_Incourse %>% filter(incourse == 1) %>% group_by(fiscal_year) %>% summarise(sum(total))

########################################
##### JOIN CLASS AND PAYROLL DATA ######

# merge smd and teacher pay together
smd_teacherpay <- left_join(smd, Teacher_Payroll, by=c("TEACHER_ID", "SCHOOL_YEAR"))

# calculate weights, used to allocate teacher instructional compensation to individual students
cls_wgts <- smd_teacherpay %>% filter(INEX_CLASS==1) %>% 
  group_by(TEACHER_ID, SCHOOL_YEAR, INEX_CLASS) %>% 
  summarise(T_WGTA = sum(WGT), T_WGTC = sum(WGT_STD_C)) %>% 
  ungroup()

smd_teacherpay %<>% left_join(cls_wgts, by=c("TEACHER_ID","SCHOOL_YEAR","INEX_CLASS"))

# allocate teacher pay by multiple different weighting schemes, exclude certain classes based on criteria
smd_teacherpay %<>% 
  mutate(A_WGTA = (INST_TOTALCOMP/T_WGTA)*WGT,
         A_WGTC = (INST_TOTALCOMP/T_WGTC)*WGT_STD_C,
         B_WGTC = (INST_BENEFITS/T_WGTC)*WGT_STD_C,
         W_WGTC = (INST_PAY/T_WGTC)*WGT_STD_C,
         INEX_VIZ_CLASS = if_else(INST_TOTALCOMP==0 | NAME=="" | is.na(INST_TOTALCOMP)==TRUE | is.na(NAME)==TRUE | INEX_CLASS==0 ,0,INEX_VIZ_CLASS))

# create and export a file to load into tableau dashboards and assess class problems, teachers that should be excluded, etc.
classes_check <- smd_teacherpay %>%
  select(STUDENT_ID,DISTRICT_ID, TEACHER_ID,CLASS_ID,INSTITUTIONAL_SETTING,CORE_SHORT_DESC, core_short_desc_of_class, SCHOOL_COURSE_TITLE, school_course_title_of_class,
         WGT_WD, TERM_STUDENT, CYCLE_STUDENT, PERIOD_STUDENT, A_WGTC, INST_TOTALCOMP,NAME,INEX_CLASS,INEX_VIZ_CLASS,SCHOOL_YEAR,school_name_of_class,GRADE_LEVEL,school_number_of_class,NDAYS_CLASS,
         SPECIAL_ED,ELL_STATUS,LOW_INCOME, WGT) %>%
  mutate(LOW_INCOME = if_else(LOW_INCOME == 1 & !is.na(LOW_INCOME), "Low Income", "Not Low Income"),
         SPECIAL_ED = if_else(SPECIAL_ED == "Y" & !is.na(SPECIAL_ED), "Special Ed", "Not Special Ed"),
         ELL_STATUS = if_else(ELL_STATUS == "Y" & !is.na(ELL_STATUS), "ELL", "Not ELL")) %>%
  group_by(CLASS_ID) %>% mutate(Grade_Level_of_Class = getmode(GRADE_LEVEL)) %>% ungroup()

file.remove(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/classes.hyper"))
RTableau:::write_tableau(classes_check, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files/classes.hyper"))
setwd(paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Working Files"))
lapply(list.files(pattern = ".log"), file.remove)

###############################
##### FINAL DATA CLEANING #####

### Final edit of incourse file - exclude any teachers removed from class edits

smd_teachers <- smd_teacherpay %>% subset(INEX_CLASS == 1) %>% distinct(SCHOOL_YEAR, TEACHER_ID) %>% mutate(in_all = 1)

Payroll_With_Incourse %<>% mutate(cactus_id = as.character(cactus_id)) %>% 
  left_join(smd_teachers, by = c("fiscal_year" = "SCHOOL_YEAR", "cactus_id" = "TEACHER_ID")) %>%
  mutate(incourse = if_else(incourse == 1 & is.na(in_all), 0, incourse)) %>% select(-in_all)

# check that the teacher compensation totals now match
round(sum(smd_teacherpay$A_WGTC, na.rm = T)) == round(sum(Payroll_With_Incourse$total[Payroll_With_Incourse$incourse == 1]))

################################################################
##### EXPORT DATA FILES FOR EXPENSE AND STUDENT ALLOCATION #####

saveRDS(smd_teacherpay, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Allocation/smd_TeacherPay.RDS"))
saveRDS(Teacher_Payroll, paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/Teacher_Payroll_", lea_ids$district_code, ".RDS"))
saveRDS(Payroll_With_Incourse,paste0("H:/Economists/Ed/KIDS/", lea_ids$folder_name, "/Payroll/", lea_ids$district_code, "_Payroll_With_Incourse.RDS"))
