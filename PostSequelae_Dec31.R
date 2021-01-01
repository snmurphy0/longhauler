rm(list = ls())

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(Hmisc)

#==== 
#==== Read Phase 2.1. Data
#==== 
#======!!! Note >> I have added the information about sequence of encounters !!!
#======!!! It is necessary to have the information about encounters in the PatientClinicalCourse file as in the simulated data


#PatientSummary<-read.csv("./SimulatedData/PatientSummarySim.csv")
#PatientClinicalCourse<-read.csv("./SimulatedData/PatientClinicalCourseSim.csv")
#PatientObservations<-read.csv("./SimulatedData/PatientObservationsSim.csv")

#== added the new data files from i2b2syn (synthetic data from OMOP project) andth UTF to read Windows files
PatientSummary<-read.csv("./SimulatedData/synPatientSummary.csv",fileEncoding="UTF-8-BOM")
PatientClinicalCourse<-read.csv("./SimulatedData/synPatientClinicalCourse.csv",fileEncoding="UTF-8-BOM")
PatientObservations<-read.csv("./SimulatedData/synPatientObservations.csv",fileEncoding="UTF-8-BOM")
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
PtsReadmitted<-unique(PatientClinicalCourse[PatientClinicalCourse$encounter_seq>1,c("patient_num")])
TotalDaysFirstEncounter$Readmitted<-ifelse(TotalDaysFirstEncounter$patient_num %in% PtsReadmitted, "YES","NO")

PostSequelaeList$DurationofthefirstadmissionGroups<-ggplot(data=TotalDaysFirstEncounter, aes(x=Duration, fill=as.factor(Readmitted))) + 
  geom_histogram(binwidth = 1, alpha=0.5) + xlab("Duration of the first admission")+ylab("")

wilcox.test(Duration ~ Readmitted, data = TotalDaysFirstEncounter)


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

PatientObservationsEnctrsDiagPheCodes$PhecodeLength<-nchar(PatientObservationsEnctrsDiagPheCodes$Phecode)




#=========Time Window (use days form admission or days from the first discharge)

PatientObservationsEnctrsDiagPheCodes<-na.omit(PatientObservationsEnctrsDiagPheCodes)
PatientObservationsEnctrsDiagPheCodes<-PatientObservationsEnctrsDiagPheCodes[PatientObservationsEnctrsDiagPheCodes$encounter_seq>1,]

#== uncommented the fixed windows
PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
                                              breaks=c(-1000,0, 30, 60, 90, 120,1000),
                                              right = FALSE, labels = c( "<0" ,   "<30"  , "31-60" ,  "61-90",  "91-120" ,">120" ))

#== commented the computed windows
#PatientObservationsEnctrsDiagPheCodes$timewindw <- cut2(PatientObservationsEnctrsDiagPheCodes$days_since_firstdischarge, g =5)

#=========Diagnosis in time Window - Bubble Graph
CountDiagnosisTW<-PatientObservationsEnctrsDiagPheCodes[PatientObservationsEnctrsDiagPheCodes$PhecodeLength>=3,] %>%
  group_by(Description,timewindw) %>%
  summarise(Freq=n()) 

CountDiagnosisNPts<-PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(timewindw) %>%
  summarise(Pts=n_distinct(patient_num)) 

CountDiagnosisDescr<-PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(Description) %>%
  summarise(Pts=n_distinct(patient_num)) 
CountDiagnosisDescr$perc<-(CountDiagnosisDescr$Pts/length(unique(CountDiagnosisDescr$Pts)))*100

#For the Bubble Chart > select only Frequent Phecodes 
keep<-data.frame(Description=unique(CountDiagnosisDescr[CountDiagnosisDescr$perc>50,c("Description")]))


CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100

PostSequelaeList$CountDiagnosisTot<-CountDiagnosis

CountDiagnosis<-CountDiagnosis[CountDiagnosis$Description %in% keep$Description,]
CountDiagnosis$Description <- factor(CountDiagnosis$Description, levels = CountDiagnosisDescr$Description[order(CountDiagnosisDescr$perc)])

PostSequelaeList$DiagnosisBubblePlot<-ggplot(CountDiagnosis, aes(x=timewindw, y=Description,  size = perc,color=Freq)) +
  geom_point(alpha=0.7)+ scale_size(range = c(.1, 15), name="% Patients")+
  scale_colour_gradient(low = "#4895ef", high = "#3a0ca3", name="# Patients")+
  theme(text = element_text(size=15))+
  # xlab("Day since admission")+ylab("PheCode")
  xlab("Day since First Discharge")+ylab("Murphy PheCode")






save(PostSequelaeList, file="PostSequelaeListSIMULATED.RData")




