#setwd("~/GitHub/longhauler")
setwd("~/Documents/DBMI/COVID-19 Longhauler Analysis/longhauler-main")
rm(list = ls())

library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(Hmisc)
library(scales)

#==== 
#==== Read Phase 2.1. Data
#==== 
#======!!! Note >> I have added the information about sequence of encounters !!!
#======!!! It is necessary to have the information about encounters in the PatientClinicalCourse file as in the simulated data


#== added the simulated data files from Arianna
PatientSummary<-read.csv("./SimulatedData/PatientSummarySim.csv")
PatientClinicalCourse<-read.csv("./SimulatedData/PatientClinicalCourseSim.csv")
PatientObservations<-read.csv("./SimulatedData/PatientObservationsSim.csv")

#== added the new data files from i2b2syn (synthetic data from OMOP project) and the UTF to read Windows files
#PatientSummary<-read.csv("./SimulatedData/synPatientSummary.csv",fileEncoding="UTF-8-BOM")
#PatientClinicalCourse<-read.csv("./SimulatedData/synPatientClinicalCourse.csv",fileEncoding="UTF-8-BOM")
#PatientObservations<-read.csv("./SimulatedData/synPatientObservations.csv",fileEncoding="UTF-8-BOM")

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
#Tiffany: might need to come back to 
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

#Tiff: converted PheCode column to character vector 
PatientObservationsEnctrsDiagPheCodes$Phecode <- as.character(PatientObservationsEnctrsDiagPheCodes$Phecode)

#Tiff: commented out original
PatientObservationsEnctrsDiagPheCodes$PhecodeLength<-nchar(PatientObservationsEnctrsDiagPheCodes$Phecode)


#Tiff: PatientObservationsEnctrsDiagPheCodes should include combined patient summary / observations + PheCodes

#=========Time Window (use days form admission or days from the first discharge)

PatientObservationsEnctrsDiagPheCodes<-na.omit(PatientObservationsEnctrsDiagPheCodes)
PatientObservationsEnctrsDiagPheCodes<-PatientObservationsEnctrsDiagPheCodes[PatientObservationsEnctrsDiagPheCodes$encounter_seq>0,]  ## changed from 1

#Tiff: figure out why it had to be changed from 1 to 0
#Tiff: aren't most of them bigger than 3 anyway?

#== uncommented the fixed windows
#PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
#                                              breaks=c(-1000,0, 30, 60, 90, 120,1000),
#                                              right = FALSE, labels = c( "<0" ,   "1-30"  , "31-60" ,  "61-90",  "91-120" ,">120" ))

PatientObservationsEnctrsDiagPheCodes$timewindw<-cut(PatientObservationsEnctrsDiagPheCodes$days_since_admission,
                                                     breaks=c(-Inf,0, 30, 60, 90, 120,Inf),
                                                     right = FALSE, labels = c( "<0" ,   "0-29"  , "30-59" ,  "60-89",  "90-119" ,"120-inf" ))

#== commented the computed windows
#PatientObservationsEnctrsDiagPheCodes$timewindw <- cut2(PatientObservationsEnctrsDiagPheCodes$days_since_firstdischarge, g =5)

#=========Diagnosis in time Window - Bubble Graph
CountDiagnosisTW<-PatientObservationsEnctrsDiagPheCodes[PatientObservationsEnctrsDiagPheCodes$PhecodeLength>=3,] %>%
  group_by(Description,timewindw) %>%
  #summarise(Freq=n())  ## this was giving duplicates
  summarise(Freq=n_distinct(patient_num)) 

CountDiagnosisNPts<-PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(timewindw) %>%
  summarise(Pts=n_distinct(patient_num)) 

CountDiagnosisDescr<-PatientObservationsEnctrsDiagPheCodes %>% 
  group_by(Description) %>%
  summarise(Pts=n_distinct(patient_num)) 

CountDiagnosisDescr$perc<-(CountDiagnosisDescr$Pts/length(unique(CountDiagnosisDescr$Pts)))*100
CountDiagnosisTW$perc<-(CountDiagnosisTW$Freq/length(unique(CountDiagnosisTW$Freq)))*100    ################################# HERE

#For the Bubble Chart > select only Frequent Phecodes 
#keep<-data.frame(Description=unique(CountDiagnosisDescr[CountDiagnosisDescr$perc>200,c("Description")]))  #specify bubble cutoff here
#keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$Freq>24) & (CountDiagnosisTW$timewindw==c("60-89")), c("Description") ]))  #specify bubble cutoff here
keep<-data.frame(Description=unique(CountDiagnosisTW[ (CountDiagnosisTW$perc>20) & (CountDiagnosisTW$timewindw==c("60-89")), c("Description") ]))  #specify bubble cutoff here ################################# HERE


CountDiagnosis<-merge(CountDiagnosisTW,CountDiagnosisNPts, by=c("timewindw"), all.x = TRUE)
CountDiagnosis$perc<-(CountDiagnosis$Freq/CountDiagnosis$Pts)*100

PostSequelaeList$CountDiagnosisTot<-CountDiagnosis

CountDiagnosis<-CountDiagnosis[CountDiagnosis$Description %in% keep$Description,]
CountDiagnosis$Description <- factor(CountDiagnosis$Description, levels = CountDiagnosisDescr$Description[order(CountDiagnosisDescr$perc)])

PostSequelaeList$DiagnosisBubblePlot_A<-ggplot(CountDiagnosis, aes(x=timewindw, y=Description,  size = perc,color=Freq)) +
  geom_point(alpha=0.7)+ scale_size(range = c(.1, 15), name="% Patients")+
  scale_colour_gradient(low = "#4895ef", high = "#3a0ca3", name="# Patients")+
  theme(text = element_text(size=15))+
  xlab("Day since admission")+ylab("PheCode")
  #xlab("Day since First Discharge")+ylab("Murphy PheCode")

PostSequelaeList$DiagnosisBubblePlot_A

#save(PostSequelaeList, file="PostSequelaeListSIMULATED.RData")
save(PostSequelaeList, file="./SimulatedData/synPostSequelaeList.RData") #TBD
#save(PostSequelaeList, file="./ACTData/actPostSequelaeList.RData")


# 371     61-90                                                                     Viral infection  370 342 108.1871345

#Graph 1 (edited version of original)

#filter to show the top five conditions with the highest percentages at 90-119
CountDiagnosis_filtered <- CountDiagnosis %>% filter (Description %in% as.data.frame(CountDiagnosis %>% 
                                           select(timewindw,Description,perc,) %>% arrange(perc) 
                                         %>% filter(timewindw == '90-119') #to change to 120-inf
                                         %>% top_n(10,Description))$Description)

PostSequelaeList$DiagnosisBubblePlot_A_v2<-ggplot(CountDiagnosis, aes(x=timewindw, y=Description,  
                                                                      size = Freq,color=perc,
                                                                      label= paste(Freq, "^(", perc, ")", sep = ""))) +
  geom_point(alpha=0.8)+ scale_size_manual(range = c(10, 25), name="# Patients",n.breaks = 5, limits = c(1,100))+
  scale_colour_steps3(low = "#4895ef", high = 'red', name="% Patients")+
  #scale_colour_gradient(low = muted("blue"), high = muted("red"), 
                          #guide = 'colourbar',
                         #midpoint = median(CountDiagnosis_filtered$Freq),name="# Patients")+
  scale_y_discrete(limits = as.data.frame(CountDiagnosis_filtered %>% 
                                            select(timewindw,Description,perc,) %>% arrange(perc) 
                                          %>% filter(timewindw == '90-119') 
                                          %>% top_n(10,Description))$Description) + 
  #geom_text(parse = TRUE,position = "dodge") +
  geom_text() +
  theme(text = element_text(size=15))+
  xlab("Day since admission")+ylab("PheCode")

#mid = muted('yellow'),

PostSequelaeList$DiagnosisBubblePlot_A_v2

#low = "#6B8E23"

#Tiff: switched from scale_colour_gradient to scale_colour_gradient2

#color (done)
#filter (top 5 PheCodes) (done)
#sort (90-119 -> change to 120-inf later)
#figure out how perc was calculated (done)

#Graph 2 

#create new column with percent decrease 
library(viridis)

CountDiagnosis_filtered_B <- CountDiagnosis %>% filter(timewindw == "0-29" | timewindw == "90-119") %>% 
  group_by(Description) %>% arrange(timewindw, .by_group = TRUE) %>% 
  mutate(perc_growthrate = (perc/lag(perc) - 1) * 100) %>% filter(timewindw == "90-119")

PostSequelaeList$DiagnosisBubblePlot_B<-ggplot(CountDiagnosis_filtered_B, aes(x=Freq, y=perc_growthrate,  size = perc,color=Description,label=Description)) +
  geom_point(alpha=0.7)+ scale_size(range = c(10, 20), name="% Patients")+
  #scale_fill_brewer(discrete=TRUE, guide=FALSE, option="B") +
  scale_color_brewer(palette = "Set1") +
  theme(text = element_text(size=15))+
  xlab("# of Patients")+ylab("% Patients (Growth Rate)")+
  geom_text(color="black",size=2,position=position_dodge(width=0.9)) +
  ggtitle("Days Since Admission: 90-119")

PostSequelaeList$DiagnosisBubblePlot_B

#Graph 3


