
#### GET LIBRARIES ####
source("/Users/nicklewis/Documents/Proof that Nick Lewis is Real/Libraries.R", local = TRUE)

#### LOAD IN DATA ####

concerns <- 
  read_csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/PurplePostConcerns.csv")
master <-
  read_csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/PurplePostMaster.csv")
EmployeeStatusInfo <-
  read_csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/EmployeeStatusInfo.csv")
Student_term <-
  read_csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/StudentTermInformation.csv")
Static_Info <- 
  read.csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/StudentStaticInformation.csv", 
           stringsAsFactors = FALSE)
PurplePostParticipants <- 
  read.csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/PurplePostParticipants.csv") 
replies <-
  read.csv("~/Documents/CHANGELAB/Fall 2019/OneDrive_1_10-14-2019/PurplePostReplies.csv")

#### REPLIES ####
replies <- replies %>%
  mutate(AlertId = as.character(AlertId))

#### Concerns ####
concerns <- concerns %>% # correcting type errors
  mutate(AlertId = as.factor(AlertId),
         ConcernId = as.factor(ConcernId),
         ConcernName = as.factor(ConcernName),
         ConcernDesc = as.factor(ConcernDesc),
         ConcernTypeId = as.factor(ConcernDesc),
         ConcernTypeName = as.factor(ConcernTypeName)) %>%
  filter(!is.na(AlertId)) # filtering out 3 completely empty rows

#### Master ####
master <- master %>% 
  mutate(AlertId = as.factor(AlertId), # correcting type errors
         StudentAnonId = as.factor(StudentAnonId),
         AuthorAnonId = as.factor(AuthorAnonId),
         Subject = as.factor(Subject),
         CourseLevel = as.factor(CourseLevel)) %>%
  mutate(StillActive = ifelse(is.na(ArchivedDate), 1, 0)) %>% # indicator for still active
  mutate(Course000 = ifelse(CourseLevel=="000",1,0), # indicators for course levels
         Course100 = ifelse(CourseLevel==100,1,0),
         Course200 = ifelse(CourseLevel==200,1,0),
         Course300 = ifelse(CourseLevel==300,1,0),
         Course400 = ifelse(CourseLevel==400,1,0),
         Course500 = ifelse(CourseLevel==500,1,0),
         Course600 = ifelse(CourseLevel==600,1,0),
         Course700 = ifelse(CourseLevel==700,1,0),
         GradLevel = ifelse(CourseLevel %in% c(500,600,700),1,0),
         UGradLevel = ifelse(CourseLevel %in% c(000,100,200,300,400),1,0),
         UGradLevel_not100 = ifelse(CourseLevel %in% c(000,200,300,400),1,0)) %>%
  mutate(CreatedMonth = substr(master$CreatedDate, 6, 7), # Grouping Dates by Month
         EarlyInSemester = ifelse(CreatedMonth %in% c("01","08","09"),1,0),
         MiddleOfSemester = ifelse(CreatedMonth %in% c("02","03","10","11"),1,0),
         EndOfSemester = ifelse(CreatedMonth %in% c("04","05","12"),1,0),
         SummerClass = ifelse(CreatedMonth %in% c("06","07"),1,0)) %>%
  mutate(Subject = as.factor(ifelse(Subject == "GAP", "CHNG", as.character(Subject)))) %>% # Replacing the old Changelab
  # class code with the new one
  mutate(SubjectGroup = case_when( # grouping into broader class code groups
    Subject %in% c("PH","PT","NURS","EXSS","NUTR","HS","PA","HE","HSA","AT") ~ "Health Sciences",
    Subject %in% c("ME","ENGR","CE","EE") ~ "Engineering",
    Subject %in% c("QM","COMM","MGT","MKT","ECON","ACCT","FIN","ID","BUS") ~ "Accounting and Business Administration",
    Subject %in% c("ART","MUS","APM","ARCH","THTR","ARTH") ~ "The Arts",
    Subject %in% c("PSYC","ES","SOC","MATH","NEUR","BIOL","CHEM","CS","PHYS","STAT","GEOG","ASTR","GEOL","COGS") ~ "Sciences and Math",
    Subject %in% c("SPAN","FREN","ENGL","WRTG","RUSS","EL","GERM","LATN","HEB","FL","JAPN") ~ "Language",
    Subject %in% c("CJ","LAW","PSCI","ANTH","HIST") ~ "Law and History",
    Subject %in% c("FYS","PHIL","EXED","CHNG","REL","TESL","GWS","DISC","ETH","SA","GL","EDUC") ~ "Misc"))

#### EMPLOYEE STATUS INFO ####
EmployeeStatusInfo <- EmployeeStatusInfo %>%
  mutate(ContinualEmployment = ifelse(is.na(EmpStatusEnd),1,0)) %>%
  mutate(AuthorAnonId = EmployeeAnonId) %>%
  select(-EmployeeAnonId)

#### PURPLE POST PARTICIPANTS ####
PurplePostParticipants <- PurplePostParticipants %>%
  mutate(NoViewing = ifelse(is.na(LastViewedDate),1,0)) %>%
  mutate(StudentAnonId = ParticipantId) %>%
  select(-ParticipantId) %>%
  mutate(AlertId = as.character(AlertId),
         StudentAnonId = as.factor(StudentAnonId)) 


#### STUDENT TERM INFO #### 
Student_term<-Student_term%>%
  mutate(GroupA=if_else(GroupA=="Y",1,0),
         GroupB=if_else(GroupB=="Y",1,0),
         GroupC=if_else(GroupC=="Y",1,0),
         GroupD=if_else(GroupD=="Y",1,0),
         GroupE=if_else(GroupE=="Y",1,0),
         GroupF=if_else(GroupF=="Y",1,0),
         GroupG=if_else(GroupG=="Y",1,0),
         WorkStudy=if_else(WorkStudy=="Y",1,0),
         Gender=if_else(Gender=="F",1,0))



Student_term<- Student_term %>%
  mutate(Freshmen=if_else(ClassLevelDesc=="Freshmen",1,0),
         Sophmore=if_else(ClassLevelDesc=="Sophmore",1,0),
         Junior=if_else(ClassLevelDesc=="Junior",1,0),
         Senior=if_else(ClassLevelDesc=="Senior",1,0),
         Graduate=if_else(ClassLevelDesc=="Graduate",1,0),
         Special=if_else(ClassLevelDesc=="Special",1,0),
         HarlaxtonSpecial=if_else(ClassLevelDesc=="Harlaxton Special",1,0))

Student_term<-  Student_term%>%
  mutate(Village=if_else(ResidenceType=="Village",1,0),
         Commute=if_else(ResidenceType=="Commute",1,0),
         ResidenceHall=if_else(ResidenceType=="Residence Hall",1,0))

Student_term<- Student_term%>%
  mutate(Purple=if_else(EFC_Group=="Purple",1,0),
         Silver=if_else(EFC_Group=="Silver",1,0),
         Blue= if_else(EFC_Group=="Blue",1,0),
         Orange=if_else(EFC_Group=="Orange",1,0),
         White=if_else(EFC_Group=="White",1,0),
         Yellow=if_else(EFC_Group=="Yellow",1,0),
         Black=if_else(EFC_Group=="Black",1,0),
         Green=if_else(EFC_Group=="Green",1,0),
         Red=if_else(EFC_Group=="Red",1,0),
         Gold=if_else(EFC_Group=="Gold",1,0))

#### STUDENT STATIC ####
# Create indicator variables
Static_Info <- Static_Info %>% mutate(RaceA = if_else(RaceA == "Y", 1, 0),
                                      RaceB = if_else(RaceB == "Y", 1, 0),
                                      RaceC = if_else(RaceC == "Y", 1, 0),
                                      RaceD = if_else(RaceD == "Y", 1, 0),
                                      RaceE = if_else(RaceE == "Y", 1, 0),
                                      RaceF = if_else(RaceF == "Y", 1, 0),
                                      FirstGen = if_else(FirstGen == "Y", 1, 0),
                                      TransferIn = if_else(TransferIn == "Y", 1, 0),
                                      Withdrawn = if_else(CurrentStatus == "Withdrawn", 1, 0),
                                      CurrentlyEnrolled = if_else(CurrentStatus == "CurrentlyEnrolled", 1, 0),
                                      Graduated = if_else(CurrentStatus == "Graduated", 1, 0),
                                      LegacyStudent = if_else(LegacyStudent == "Y", 1, 0))

# Geographic indicator variables
Static_Info <- Static_Info %>% mutate(Indiana = if_else(GeographicRegion == "Indiana", 1, 0),
                                      International = if_else(GeographicRegion == "International", 1, 0),
                                      `Mid Atlantic` = if_else(GeographicRegion == "Mid Atlantic", 1, 0),      
                                      MidWest = if_else(GeographicRegion == "Midwest", 1, 0),
                                      `North Plains` = if_else(GeographicRegion == "North Plains", 1, 0),        
                                      Northeast = if_else(GeographicRegion == "Northeast", 1, 0),
                                      Pacific = if_else(GeographicRegion == "Pacific", 1, 0),
                                      Plains = if_else(GeographicRegion == "Plains", 1, 0),                     
                                      South = if_else(GeographicRegion == "South", 1, 0),
                                      Southwest = if_else(GeographicRegion == "Southwest", 1, 0),
                                      Vanderburgh = if_else(GeographicRegion == "Vanderburgh", 1, 0),            
                                      West = if_else(GeographicRegion == "West", 1, 0))

# Replace NA values in ACT/SAT
Static_Info <- Static_Info %>% mutate(HS_RANK = if_else(HS_RANK != 9999, HS_RANK, NA_integer_),
                                      HS_SIZE = if_else(HS_SIZE != 9999, HS_SIZE, NA_integer_),
                                      ACT_C = if_else(ACT_C != 0, ACT_C, NA_integer_),
                                      SATR_C = if_else(SATR_C != 0, SATR_C, NA_integer_),
                                      SATO_C = if_else(SATO_C != 0, SATO_C, NA_integer_)) %>%
  mutate(StudentAnonId = AnonStudentId) %>%
  select(-AnonStudentId)


# Need to convert all to factors so that they read as 0 or 1 and not a continuous variable.
Static_Info[c(9:15, 17, 18, 21, 23:36)] <- Static_Info[c(9:15, 17, 18, 21, 23:36)] %>% lapply(as.factor) %>% data.frame()

#### JOINING DATA SETS ####

concernslists <- spread(concerns, ConcernName, ConcernId)
concernslists <- spread(concernslists, ConcernTypeName, ConcernTypeId)
joined <- left_join(master, concernslists, by = "AlertId") # multiple values in concerns per row in master

EmployeeStatusInfoCondensed <- EmployeeStatusInfo %>%
  group_by(AuthorAnonId) %>%
  summarize(EmpStatusStart = min(EmpStatusStart),
            EmpStatusEnd = as_date(if(sum(is.na(EmpStatusEnd)) == 0) {as_date(max(EmpStatusEnd))} else {NA}))
joined <- left_join(joined, EmployeeStatusInfo, by = "AuthorAnonId")
joined <- left_join(joined, PurplePostParticipants, by = c("AlertId","StudentAnonId"))
joined <- left_join(joined, replies, by = "AlertId")
joined <- left_join(joined, Static_Info, by = "StudentAnonId")
joined <- left_join(joined, Student_term, by = "StudentAnonId")
