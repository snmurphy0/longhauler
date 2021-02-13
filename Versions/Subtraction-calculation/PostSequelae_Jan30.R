#setwd("~/GitHub/longhauler")
setwd("~/Documents/DBMI/COVID-19 Longhauler Analysis/longhauler-main")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(Hmisc)
library(scales)
library(viridis)

#==== 
#==== Read Phase 2.1. Data
#==== 
#======!!! Note >> I have added the information about sequence of encounters !!!
#======!!! It is necessary to have the information about encounters in the PatientClinicalCourse file as in the simulated data

#== added the simulated data files from Arianna
#PatientSummary<-read.csv("./SimulatedData/PatientSummarySim.csv")
#PatientClinicalCourse<-read.csv("./SimulatedData/PatientClinicalCourseSim.csv")
#PatientObservations<-read.csv("./SimulatedData/PatientObservationsSim.csv")

#== added the new data files from i2b2syn (synthetic data from OMOP project) and the UTF to read Windows files
PatientSummary<-read.csv("./SimulatedData/synPatientSummary.csv",fileEncoding="UTF-8-BOM")
PatientClinicalCourse<-read.csv("./SimulatedData/synPatientClinicalCourse.csv",fileEncoding="UTF-8-BOM")
PatientObservations<-read.csv("./SimulatedData/synPatientObservations.csv",fileEncoding="UTF-8-BOM")

#== added the new data files from ACT_Stage (real data from ACT project) andth UTF to read Windows files
#PatientSummary<-read.csv("./ACTData/actPatientSummary.csv",fileEncoding="UTF-8-BOM")
#PatientClinicalCourse<-read.csv("./ACTData/actPatientClinicalCourse.csv",fileEncoding="UTF-8-BOM")
#PatientObservations<-read.csv("./ACTData/actPatientObservations.csv",fileEncoding="UTF-8-BOM")
PostSequelaeList<-list()

#====  
#====  Number of Encounters per patient
#====  
NumberOfEncounters<-PatientClinicalCourse[PatientClinicalCourse$in_hospital==1,]%>% 
  group_by(patient_num) %>% 
  summarise(NumbEncounters = as.factor(n_distinct(encounter_seq)))

NumberOfEncountersTbl<-as.data.frame(table(NumberOfEncounters$NumbEncounters))

PostSequelaeList$NumberofEncounters<-ggplot(data=NumberOfEncountersTbl, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")+xlab("Number of Encounters")+ylab("Count of Patients")

#==== #==== 
#==== Duration of admissions, Days btw admissions 
#==== #==== 

#====  
#====  Duration of first Admission
#====  
TotalDaysFirstEncounter<-PatientClinicalCourse[PatientClinicalCourse$encounter_seq==1,] %>% 
  group_by(patient_num) %>% 
  summarise(Duration = max(days_since_admission))

PostSequelaeList$Durationofthefirstadmission<-ggplot(data=TotalDaysFirstEncounter, aes(Duration)) + 
  geom_histogram(binwidth = 1) + xlab("Duration of the first admission")+ylab("")

PostSequelaeList$DurationofthefirstadmissionSummary<-summary(TotalDaysFirstEncounter$Duration)

#====  
#====  Duration of first Admission for patients with/without a second readmission
#====  
PtsReadmitted<-unique(PatientClinicalCourse[PatientClinicalCourse$encounter_seq>1,c("patient_num")]) #list of patients who have an encounter sequence value higher than 1
TotalDaysFirstEncounter$Readmitted<-ifelse(TotalDaysFirstEncounter$patient_num %in% PtsReadmitted, "YES","NO")

PostSequelaeList$DurationofthefirstadmissionGroups<-ggplot(data=TotalDaysFirstEncounter, aes(x=Duration, fill=as.factor(Readmitted))) + 
  geom_histogram(binwidth = 1, alpha=0.5) + xlab("Duration of the first admission")+ylab("")

wilcox.test(Duration ~ Readmitted, data = TotalDaysFirstEncounter) #NOTE: Duration and Readmitted are not two independent groups though 


#==== 
#==== DIAGNOSIS AND PROCEDURES
#===

PatientClinicalCourseGrouped<-PatientClinicalCourse %>% 
  group_by(siteid,patient_num, encounter_seq) %>% 
  summarise(FirstDay = min(days_since_admission), LastDay=max(days_since_admission))

PatientObservations<-na.omit(PatientObservations)

# Add encounter information (observation admission day > 0)
PatientObservationsMerge<-merge(PatientObservations[PatientObservations$days_since_admission>=0,],PatientClinicalCourseGrouped, by=c("siteid","patient_num") , all = TRUE)

PatientObservationsMergeSelect<-PatientObservationsMerge[PatientObservationsMerge$days_since_admission>= PatientObservationsMerge$FirstDay & 
                                                           PatientObservationsMerge$days_since_admission<= PatientObservationsMerge$LastDay ,]

# Create encounters (observation admission day < 0 )
PatientObservationsMergePreviousHosp<-PatientObservations[PatientObservations$days_since_admission<0,]
if (nrow(PatientObservationsMergePreviousHosp)>0) {
  PatientObservationsMergePreviousHosp$encounter_seq<--1
  #PatientObservationsMergePreviousHosp$ward<-"Pre-Covid"  #== not sure why i had to comment this to work, but columns did not match in merge otherwise
  PatientObservationsMergePreviousHosp$FirstDay<-PatientObservationsMergePreviousHosp$days_since_admission
  PatientObservationsMergePreviousHosp$LastDay<--1
}

PatientObservationsEnctrs<-rbind(PatientObservationsMergeSelect,PatientObservationsMergePreviousHosp)
PatientObservationsEnctrs$FirstDay<-NULL
PatientObservationsEnctrs$LastDay<-NULL



#=========
#=========DIAGNOSIS - PheCode
#=========
#PheCodes<-read.csv("./PheCodesMapping/map_file_icd9_all.csv")
#=== added the new phecode mapping file provided from Victor
PheCodes<-read.csv("./PheCodesMapping/phecode_map_ICD910CM.csv")
load(file = "./PheCodesMapping/phecode_map_file.rda")
PheCodes<-merge(PheCodes, phecode_description, by=c("Phecode"),all.x = TRUE)

PatientObservationsEnctrs$concept_code<-str_trim(PatientObservationsEnctrs$concept_code, side = c("both"))

#PatientObservationsEnctrsDiagPheCodes<-merge(PatientObservationsEnctrs[PatientObservationsEnctrs$concept_type=="DIAG-ICD9",], 
#                                             PheCodes[,c("code","Description","Phecode")], by.x=c("concept_code"),  by.y = ("code"), all.x  = TRUE  )
#== changed this to ICD10 concept type from the i2b2 files
PatientObservationsEnctrsDiagPheCodes<-merge(PatientObservationsEnctrs[PatientObservationsEnctrs$concept_type=="DIAG-ICD10",], 
                                             PheCodes[,c("code","Description","Phecode")], by.x=c("concept_code"),  by.y = ("code"), all.x  = TRUE  )

#converted PheCode column to character vector 
PatientObservationsEnctrsDiagPheCodes$Phecode <- as.character(PatientObservationsEnctrsDiagPheCodes$Phecode)

#commented out original
PatientObservationsEnctrsDiagPheCodes$PhecodeLength<-nchar(PatientObservationsEnctrsDiagPheCodes$Phecode)

#created new proxy column, to determine whether the PheCode was assigned before or after COVID diagnosis 
#NOTE: noticing duplicates in data (02/12)
PatientObservationsEnctrsDiagPheCodes <-PatientObservationsEnctrsDiagPheCodes %>% mutate("Pre-Covid?" = sample(c("Yes","No"),dim(PatientObservationsEnctrsDiagPheCodes)[1],replace=TRUE))

#PatientObservationsEnctrsDiagPheCodes should include combined patient summary / observations + PheCodes

#=========Time Window (use days form admission or days from the first discharge)

PatientObservationsEnctrsDiagPheCodes<-na.omit(PatientObservationsEnctrsDiagPheCodes)
PatientObservationsEnctrsDiagPheCodes<-PatientObservationsEnctrsDiagPheCodes[PatientObservationsEnctrsDiagPheCodes$encounter_seq>0,]  ## changed from 1


#== uncommented the fixed windows
#PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
#                                                     breaks=c(-1000,0, 30, 60, 90, 120,1000),
#                                                     right = FALSE, labels = c( "<0" ,   "1-30"  , "31-60" ,  "61-90",  "91-120" ,">120" ))

#reduced time window size 
#confirm if we should reduce to weekly increments (7 instead of 10)
# PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
#                                                      #breaks=c(-Inf,0, 30, 60, 90, 120,Inf),
#                                                      #right = FALSE, labels = c( "<0" ,   "0-29"  , "30-59" ,  "60-89",  "90-119" ,"120-inf" ))
#                                                      breaks=c(-Inf,0,10,20,30,40,50,60,70,80,90,100,110,120,Inf),
#                                                      right = FALSE, labels = c("<0","0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100-109","110-119","120-inf"))

PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
                                                     #breaks=c(-Inf,0, 30, 60, 90, 120,Inf),
                                                     #right = FALSE, labels = c( "<0" ,   "0-29"  , "30-59" ,  "60-89",  "90-119" ,"120-inf" ))
                                                     breaks=c(-Inf,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,Inf),
                                                     right = FALSE, labels = c("<0","0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45",
                                                     "45-50","50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90","90-95","95-100","100-105","105-110","110-115","115-120","120-inf"))
#Scenario Modeling:

#case #1 (on, prior, or after admission)
PatientObservationsEnctrsDiagPheCodes_case1 <- PatientObservationsEnctrsDiagPheCodes

#case #2 (on, prior, or after admission)
PatientObservationsEnctrsDiagPheCodes_case2 <- PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(patient_num) %>% filter(!timewindw %in% c("<0","0-5","5-10","10-15","15-20","20-25","25-30"))

#case #3 (diagnosis which are new discharge - 60d-150d)
PatientObservationsEnctrsDiagPheCodes_case3 <- PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(patient_num) %>% filter(timewindw %in% c("60-65","65-70","70-75","75-80","80-85","85-90","90-95",
                          "95-100","100-105","105-110","110-115","115-120","120-inf"))

#case #4 (first diagnosis which are new discharge - 60d and 100-150d)
PatientObservationsEnctrsDiagPheCodes_case4 <- PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(patient_num) %>% filter(timewindw %in% c("60-65","100-105","105-110","115-120","120-inf"))


#== commented the computed windows
#PatientObservationsEnctrsDiagPheCodes$timewindw <- cut2(PatientObservationsEnctrsDiagPheCodes$days_since_firstdischarge, g =5)

#=========Diagnosis in time Window - Bubble Graph

countdiagnosis <- function(PatientObs) {
  
  CountDiagnosisTW<-PatientObs[PatientObs$PhecodeLength>=3,] %>%
    group_by(Description,timewindw) %>%
    #summarise(Freq=n())  ## this was giving duplicates
    summarise(Freq=n_distinct(patient_num)) 
  
  CountDiagnosisNPts<-PatientObs %>% 
    group_by(timewindw) %>%
    summarise(Pts=n_distinct(patient_num)) 
  
  CountDiagnosisDescr<-PatientObs %>% 
    group_by(Description) %>% summarise(Pts=n_distinct(patient_num)) 
  
  ####version 2 
  #CountDiagnosisDescr_subtracted<-PatientObs %>% filter(PatientObs["Pre-Covid?"] == "No") %>%
  #  group_by(Description) %>% summarise(Pts=n_distinct(patient_num)) 
  
  CountDiagnosisDescr$perc<-(CountDiagnosisDescr$Pts/length(unique(CountDiagnosisDescr$Pts)))*100
  CountDiagnosisTW$perc<-(CountDiagnosisTW$Freq/length(unique(CountDiagnosisTW$Freq)))*100  
  
  CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
  CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100
  
  CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
  CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100
  
  #keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$Freq>mean(CountDiagnosisTW$Freq)/2), c("Description") ]))  #specify bubble cutoff here
  
  PostSequelaeList$CountDiagnosisTot<-CountDiagnosis
  #CountDiagnosis<-CountDiagnosis[CountDiagnosis$Description %in% keep$Description,]
  CountDiagnosis$Description <- factor(CountDiagnosis$Description, levels = CountDiagnosisDescr$Description[order(CountDiagnosisDescr$perc)])
  
  return(CountDiagnosis)
}

CountDiagnosis_case1 <- countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case1)
CountDiagnosis_case2 <- countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case2)
CountDiagnosis_case3 <- countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case3)
CountDiagnosis_case4 <- countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case4)
#CountDiagnosis_case5 <- countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case5) [will update after finalizing control matching method]

#countdiagnosis(PatientObservationsEnctrsDiagPheCodes_case5)


#original size: 369338
#new size (after subtraction): 739174
#CountDiagnosisDescr_subtracted$perc<-(CountDiagnosisDescr_subtracted$Pts/length(unique(CountDiagnosisDescr_subtracted$Pts)))*100

  ################################# HERE


#For the Bubble Chart > select only Frequent Phecodes 
#keep<-data.frame(Description=unique(CountDiagnosisDescr[CountDiagnosisDescr$perc>200,c("Description")]))  #specify bubble cutoff here
#keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$Freq>24) & (CountDiagnosisTW$timewindw==c("60-89")), c("Description") ]))  #specify bubble cutoff here
#keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$Freq>24), c("Description") ]))  #specify bubble cutoff here

#keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$perc>20) & (CountDiagnosisTW$timewindw==c("60-64")), c("Description") ]))  #specify bubble cutoff here ################################# HERE
#keep<-data.frame(Description=unique(CountDiagnosisTW[ CountDiagnosisTW$timewindw==c("60-64"), c("Description") ]))  #specify bubble cutoff here ################################# HERE
#keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$perc>20) & (CountDiagnosisTW$timewindw==c("60-89")), c("Description") ]))  #specify bubble cutoff here ################################# HERE


#original graph 
# CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
# CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100

#cs <- c(min(CountDiagnosis$Freq),mean(CountDiagnosis$Freq),max(CountDiagnosis$Freq))
cs <- seq(from = min(CountDiagnosis$Freq), to = max(CountDiagnosis$Freq), by = as.integer(max(CountDiagnosis$Freq)/5))

CountDiagnosis_case3_filtered <- CountDiagnosis_case3 %>% filter (Description %in% as.data.frame(CountDiagnosis %>% 
                                                                                       select(timewindw,Description,perc,) %>% arrange(perc) 
                                                                                     #%>% filter(timewindw == '90-95'| timewindw == '100-105'| timewindw == '105-110') #to change to 120-inf
                                                                                     %>% top_n(200,Description))$Description)

PostSequelaeList$DiagnosisBubblePlotA_original<- ggplot(CountDiagnosis_case3_filtered, aes(x=timewindw, y=Description,  size = perc,color=Freq)) +
  geom_point(alpha=0.7)+ scale_size(range = c(.1, 15), name="% Patients")+
  scale_colour_gradient(low = "#4895ef", high = "#990000", 
                        n.breaks = 5, name="# Patients")+
  # scale_color_manual(breaks = c("7500", "5000", "2500"),
  #                    values=c("#4895ef", "blue", "red")) +
  #scale_color_brewer(palette="Spectral")+
  theme(text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75))+
  xlab("Day since admission")+ylab("PheCode")
#xlab("Day since First Discharge")+ylab("Murphy PheCode")

PostSequelaeList$DiagnosisBubblePlotA_original

#Output a Summary Table 
library(kableExtra)
library(DT)
datatable(t(unique(CountDiagnosis %>% group_by(Pts) %>% select(timewindw,Pts) %>% top_n(1))))

#Time Plot (Top 5 disease states)
CountDiagnosis %>% filter(Description %in% c("Viral infection", "Viremia", "Acute upper respiratory infections of multiple or unspecified sites",
                                                   "Viremia, NOS", "Acute pharyngitis","Acute bronchitis and bronchiolitis")) %>% 
  ggplot(aes(x=timewindw, y=Freq, group=Description, color=Description)) + geom_line()

#Patient Trajectory Time Plot

# patient_num = 21449 (contributed to disease with highest %)
# patinet_num = 53149 (last patient to be diagnosise (highest days_since_admission))

PatientObservationsEnctrsDiagPheCodes %>% filter(patient_num == 53149) %>% 
  ggplot(aes(x=timewindw, y=count(concept_code), group=Description, color=Description)) + geom_line(stat="identity")

#Filtered graph (version A), showing top 5 diagnoses (based on % patients in 100-109 timewindow) + subtracting pre-covid diagnoses
CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100

PostSequelaeList$CountDiagnosisTot<-CountDiagnosis

CountDiagnosis<-CountDiagnosis[CountDiagnosis$Description %in% keep$Description,]
CountDiagnosis$Description <- factor(CountDiagnosis$Description, levels = CountDiagnosisDescr_subtracted$Description[order(CountDiagnosisDescr_subtracted$perc)])

CountDiagnosis_filtered <- CountDiagnosis %>% filter (Description %in% as.data.frame(CountDiagnosis %>% 
                                                                                       select(timewindw,Description,perc,) %>% arrange(perc) 
                                                                                     %>% filter(timewindw == '120-inf'| timewindw == '110-119'| timewindw == '100-109' | timewindw == '90-99') #to change to 120-inf
                                                                                     %>% top_n(5,Description))$Description)

PostSequelaeList$DiagnosisBubblePlotA_subtracted<-ggplot(CountDiagnosis_filtered, aes(x=timewindw, y=Description,  size = Freq,color=perc)) +
  geom_point(alpha=0.7)+ scale_size(range = c(7, 25), name="# Patients")+
  scale_colour_gradient(low = "#4895ef", high = "red", name="% Patients")+
  theme(text = element_text(size=15))+
  xlab("Day since admission")+ylab("PheCode") + 
  scale_y_discrete(limits = as.data.frame(CountDiagnosis_filtered %>% 
                                            select(timewindw,Description,perc,) %>% arrange(perc) 
                                          %>% filter(timewindw == '90-99' | timewindw == '100-109' | timewindw == '110-119') 
                                          %>% top_n(10,Description))$Description) 
#xlab("Day since First Discharge")+ylab("Murphy PheCode")

PostSequelaeList$DiagnosisBubblePlotA_subtracted

#save(PostSequelaeList, file="PostSequelaeListSIMULATED.RData")
save(PostSequelaeList, file="./SimulatedData/synPostSequelaeList.RData") #TBD
#save(PostSequelaeList, file="./ACTData/actPostSequelaeList.RData")

PostSequelaeList$DiagnosisBubblePlotA_bycase<-ggplot(CountDiagnosis_case3, aes(x=timewindw, y=Description,  size = perc,color=Freq)) +
  geom_point(alpha=0.7)+ scale_size(range = c(.1, 15), name="% Patients")+
  scale_colour_gradient(low = "#4895ef", high = "#990000", 
                        n.breaks = 5, name="# Patients")+
  theme(text = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.75))+
  xlab("Day since admission")+ylab("PheCode") + 
  scale_y_discrete(limits = as.data.frame(CountDiagnosis_case2 %>% 
                                            select(timewindw,Description,perc,) %>% arrange(perc) 
                                          %>% filter(timewindw == '100-105'| timewindw == '105-110') 
                                          %>% top_n(10,Description))$Description) 
#xlab("Day since First Discharge")+ylab("Murphy PheCode")

PostSequelaeList$DiagnosisBubblePlotA_bycase

#Summary Graph of latest time window (x = # of patients, y = percent)
#time windows above "100-109" did not have sufficient patient observations to visualize
CountDiagnosis_filtered_B <- CountDiagnosis %>% filter(timewindw == "0-9" | timewindw == "90-99") %>% 
  group_by(Description) %>% arrange(timewindw, .by_group = TRUE) %>% 
  mutate(perc_growthrate = (perc/lag(perc) - 1) * 100) %>% filter(timewindw == "90-99")

PostSequelaeList$DiagnosisBubblePlotB<-ggplot(CountDiagnosis_filtered_B, aes(x=Freq, y=perc_growthrate,  size = perc,color=Description,label=Description)) +
  geom_point(alpha=0.7)+ scale_size(range = c(10, 20), name="% Patients")+
  #scale_fill_brewer(discrete=TRUE, guide=FALSE, option="B") +
  scale_color_brewer(palette = "Set1") +
  theme(text = element_text(size=15))+
  xlab("# of Patients")+ylab("% Patients (Growth Rate)")+
  geom_text(color="black",size=2,position=position_dodge(width=0.9)) +
  ggtitle("Days Since Admission: 90-99")

PostSequelaeList$DiagnosisBubblePlotB

