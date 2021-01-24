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
   + It does not account for patients who are lost to follow-up.  *An analysis of only patients who have codes after 90 days (for example) might be better.* 
   In the "Versions" folder under the "subtraction-calculation" folder is a plan for developing this.
   + Instead of just showing days post admission, *it may be better to group the codes into pre-covid, covid admission, and number of days after discharge.*
   + *Could add stratification displays (perhaps in SHINEY) for severity, age, race, ethnicity.*
   
2. The i2b2syn SQL code could be enhanced to :
   + Produce age/race/severity/ethnicity in the PatientSummary table.
   + Produce a file (and code) that does not always set in-hospital="1".  This was done to adapt to the Sim analysis which only shows in-hospital patients.  
   In the syn(thetic) data there are not many readmissions, usually only admission (encounter #1) and outpatient (encounter #2).
 