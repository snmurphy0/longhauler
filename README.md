# Longhauler
### Description
Project to analyze long term covid-19 outcomes
 
This GitHub directory has the following:
 
* Code to create charts relevant to longhauler analysis
   + SQL code that can make files form an i2b2 database
   + R code that analyses and renders the data files into graphs 
* Simulated data from two different databases
   + A simulated database from the Italian sites (Sim)
   + An i2b2 project simulated database (i2b2syn)
* Tables that map icd9 and icd10 codes into PheCodes
* Publications relevant to longhauler analysis
 
To modify this document, use [markdown](https://rmarkdown.rstudio.com/articles_intro.html)
 
### The Project
The code is able to create graphs of what is afflicting patients who have had COXID-19 infections and now are being followed in the health care system.

1. The current analysis shows just the codes (grouped into PheNorm codes) over time.  some issues are:
   + It does no account for patients that may be lost to follow-up.  *An analysis of only patients that have codes after 90 days (for example) might be better.*
   + Instead of just showin days post admission, *it may be better to group the codes into pre-covid, covid admission, and days after discharge.*
   + *Could add stratification displays (perhaps in SHINEY) for severity, age, race, ethnicity.*
   
2. The i2b2syn SQL codes could be enhanced to :
   + Show age/race/severity/ethnicity
   + Not always say in-hospital.  This was done to adapt to the Sim analysis which only shows in-hospital patients (in rehab), but those are not many readmissions 
   in the i2b2syn data.
 