--
use shawn_i2b2syn
--------------------------------------------------------------------------------
--build_squelae_tables_into_csv.sql
--
--This is a sql script that works in Microsoft SQL Server and is able to build the csv
--tables that are needed for the CovidSequelae.R package.
--
--It starts with the following tables that need to be built if i2b2 does not already exist:
--  observation_fact: with the following columns
--    patient_num



--These are all the intermedate tables that are built and can be erased when the csv files 
--are ready
drop table fource_admissions
drop table fource_code_map
drop table fource_config
drop table fource_config2
drop table fource_covid_cohort
drop table fource_covid_pos_patients
drop table fource_date_list
drop table fource_LocalPatientClinicalCourse
drop table fource_LocalPatientMapping
drop table fource_LocalPatientObservations
drop table fource_LocalPatientSummary

--Internal changes to 4CE scripts
--1) # changed to fource_
--2) added encounter_seq onto the patientclinicalcourse table.  It i always 1 for the COVID admission
-- and then 2 after that for outpatient stuff.
--3) set the in_hospital variable to always be 1
--4) added siteid to the begining of csv output (option 3)
--5) changed the substring 3 -> 10 for ICD9 and ICD10

--------------------------------------------------------------------------------
-- General settings
--------------------------------------------------------------------------------
create table fource_config (
	siteid varchar(20), -- Up to 20 letters or numbers, must start with letter, no spaces or special characters.
	include_race bit, -- 1 if your site collects race/ethnicity data; 0 if your site does not collect this.
	race_in_fact_table bit, -- 1 if race in observation_fact.concept_cd; 0 if in patient_dimension.race_cd
	hispanic_in_fact_table bit, -- 1 if Hispanic/Latino in observation_fact.concept_cd; 0 if in patient_dimension.race_cd
	death_data_accurate bit, -- 1 if the patient_dimension.death_date field is populated and is accurate
	code_prefix_icd9cm varchar(50), -- prefix (scheme) used in front of a ICD9CM diagnosis code [required]
	code_prefix_icd10cm varchar(50), -- prefix (scheme) used in front of a ICD10CM diagnosis code [required]
	code_prefix_icd9proc varchar(50), -- prefix (scheme) used in front of a ICD9 procedure code [required]
	code_prefix_icd10pcs varchar(50), -- prefix (scheme) used in front of a ICD10 procedure code [required]
	obfuscation_blur int, -- Add random number +/-blur to each count (0 = no blur)
	obfuscation_small_count_mask int, -- Replace counts less than mask with -99 (0 = no small count masking)
	obfuscation_small_count_delete bit, -- Delete rows with small counts (0 = no, 1 = yes)
	obfuscation_demographics bit, -- Replace combination demographics and total counts with -999 (0 = no, 1 = yes)
	output_as_columns bit, -- Return the data in tables with separate columns per field
	output_as_csv bit -- Return the data in tables with a single column containing comma separated values
)
insert into fource_config
	select 'i2b2syn', -- siteid
		1, -- include_race
		0, -- race_in_fact_table
		1, -- hispanic_in_fact_table
		1, -- death_data_accurate
		'ICD9CM:', -- code_prefix_icd9cm
		'ICD10CM:', -- code_prefix_icd10cm
		'ICD9PCS:', -- code_prefix_icd9proc
		'ICD10PCS:', -- code_prefix_icd10pcs
		0, -- obfuscation_blur
		0, -- obfuscation_small_count_mask
		0, -- obfuscation_small_count_delete
		0, -- obfuscation_demographics
		0, -- output_as_columns
		0  -- output_as_csv

create table fource_config2 (
	replace_patient_num bit, -- Replace the patient_num with a unique random number
	save_as_columns bit, -- Save the data as tables with separate columns per field
	save_as_prefix varchar(50), -- Table name prefix when saving the data as tables
	output_as_columns bit, -- Return the data in tables with separate columns per field
	output_as_csv bit, -- Return the data in tables with a single column containing comma separated values
	output_as_csv_nodates bit -- tables with a single column containing comma separated values with NO DATES
)
insert into fource_config2
	select 
		0, -- do NOT replace_patient_num
		0, -- do NOT save_as_columns
		'dbo.fource_out_', -- save_as_prefix (don't use "4CE" since it starts with a number)
		1, -- output_as_columns
		0, -- output_as_csv
		1  -- output_as_csv with NO DATES

create table fource_code_map (
	code varchar(50) not null,
	local_code varchar(50) not null
)
alter table fource_code_map add primary key (code, local_code)
-- Inpatient visits (visit_dimension.inout_cd)
insert into fource_code_map
	select 'inpatient', 'I'
	union all select 'inpatient', 'IN'
-- Sex (patient_dimension.sex_cd)
insert into fource_code_map
	select 'male', 'M'
	union all select 'male', 'Male'
	union all select 'female', 'F'
	union all select 'female', 'Female'
-- Race (field based on fource_config.race_in_fact_table; ignore if you don't collect race/ethnicity)
insert into fource_code_map
	select 'american_indian', 'NA'
	union all select 'asian', 'A'
	union all select 'asian', 'AS'
	union all select 'black', 'B'
	union all select 'hawaiian_pacific_islander', 'H'
	union all select 'hawaiian_pacific_islander', 'P'
	union all select 'white', 'W'
-- Hispanic/Latino (field based on fource_config.hispanic_in_fact_table; ignore if you don't collect race/ethnicity)
insert into fource_code_map
	select 'hispanic_latino', 'DEM|HISP:Y'
	union all select 'hispanic_latino', 'DEM|HISPANIC:Y'
-- Codes that indicate a positive COVID-19 test result (use either option #1 and/or option #2)
-- COVID-19 Positive Option #1: individual concept_cd values
insert into fource_code_map
	select 'covidpos', 'LOINC:COVID19POS'
-- COVID-19 Positive Option #1,5: individual concept_cd values
insert into fource_code_map
	select 'covidpos', 'ICD10CM:U07.1'
-- COVID-19 Positive Option #2: an ontology path (the example here is the COVID ACT "Any Positive Test" path)
insert into fource_code_map
	select distinct 'covidpos', concept_cd
	from concept_dimension c
	where concept_path like '\ACT\UMLS_C0031437\SNOMED_3947185011\UMLS_C0022885\UMLS_C1335447\%'
		and concept_cd is not null
		and not exists (select * from fource_code_map m where m.code='covidpos' and m.local_code=c.concept_cd)
		
		
		
--******************************************************************************
--******************************************************************************
--*** Define the COVID cohort (COVID postive test + admitted)
--******************************************************************************
--******************************************************************************

--------------------------------------------------------------------------------
-- Create the list of COVID-19 positive patients.
-- Use the earliest date where the patient is known to be COVID positive,
--   for example, a COVID positive test result.
--------------------------------------------------------------------------------
create table fource_covid_pos_patients (
	patient_num int not null,
	covid_pos_date date not null
)
alter table fource_covid_pos_patients add primary key (patient_num, covid_pos_date)
insert into fource_covid_pos_patients
	select patient_num, cast(min(start_date) as date) covid_pos_date
	from observation_fact f
		inner join fource_code_map m
			on f.concept_cd = m.local_code and m.code = 'covidpos'
	group by patient_num

--------------------------------------------------------------------------------
-- Create a list of dates when patients were inpatient starting one week  
--   before their COVID pos date.
--------------------------------------------------------------------------------
create table fource_admissions (
	patient_num int not null,
	admission_date date not null,
	discharge_date date not null
)
alter table fource_admissions add primary key (patient_num, admission_date, discharge_date)
insert into fource_admissions
	select distinct v.patient_num, cast(start_date as date), cast(isnull(end_date,GetDate()) as date)
	from visit_dimension v
		inner join fource_covid_pos_patients p
			on v.patient_num=p.patient_num 
				and v.start_date >= dateadd(dd,-7,p.covid_pos_date)
		inner join fource_code_map m
			on v.inout_cd = m.local_code and m.code = 'inpatient'

--------------------------------------------------------------------------------
-- Get the list of patients who will be the covid cohort.
-- These will be patients who had an admission between 7 days before and
--   14 days after their covid positive test date.
--------------------------------------------------------------------------------
create table fource_covid_cohort (
	patient_num int not null,
	admission_date date,
	severe int,
	severe_date date,
	death_date date
)
alter table fource_covid_cohort add primary key (patient_num)
insert into fource_covid_cohort
	select p.patient_num, min(admission_date) admission_date, 0, null, null
	from fource_covid_pos_patients p
		inner join fource_admissions a
			on p.patient_num = a.patient_num	
				and a.admission_date <= dateadd(dd,14,covid_pos_date)
	group by p.patient_num

--------------------------------------------------------------------------------
-- Create a list of dates since the first case.
--------------------------------------------------------------------------------
create table fource_date_list (
	d date not null
)
alter table fource_date_list add primary key (d)
;with n as (
	select 0 n union all select 1 union all select 2 union all select 3 union all select 4 
	union all select 5 union all select 6 union all select 7 union all select 8 union all select 9
)
insert into fource_date_list
	select d
	from (
		select isnull(cast(dateadd(dd,a.n+10*b.n+100*c.n,p.s) as date),'1/1/2020') d
		from (select min(admission_date) s from fource_covid_cohort) p
			cross join n a cross join n b cross join n c
	) l
	where d<=GetDate()

--------------------------------------------------------------------------------
-- Patient Summary: Dates, Outcomes, and Demographics
--------------------------------------------------------------------------------

create table fource_LocalPatientSummary (
	siteid varchar(50) not null,
	patient_num int not null,
	admission_date date not null,
	days_since_admission int not null,
	last_discharge_date date not null,
	still_in_hospital int not null,
	severe_date date not null,
	severe int not null,
	death_date date not null,
	deceased int not null,
	sex varchar(50) not null,
	age_group varchar(50) not null,
	race varchar(50) not null,
	race_collected int not null
)

alter table fource_LocalPatientSummary add primary key (patient_num)

insert into fource_LocalPatientSummary (siteid, patient_num, admission_date, days_since_admission, last_discharge_date, still_in_hospital, severe_date, severe, death_date, deceased, sex, age_group, race, race_collected)
	select '', c.patient_num, c.admission_date, 
		datediff(dd,c.admission_date,GetDate()),
		(case when a.last_discharge_date = cast(GetDate() as date) then '1/1/1900' else a.last_discharge_date end),
		(case when a.last_discharge_date = cast(GetDate() as date) then 1 else 0 end),
		isnull(c.severe_date,'1/1/1900'),
		c.severe, 
		isnull(c.death_date,'1/1/1900'),
		(case when c.death_date is not null then 1 else 0 end),
		'',
		'',
		'',
		''
	from fource_config x
		cross join fource_covid_cohort c
		inner join (
			select patient_num, max(discharge_date) last_discharge_date
			from fource_admissions
			group by patient_num
		) a on c.patient_num=a.patient_num
		
--------------------------------------------------------------------------------
-- Patient Clinical Course: Status by Number of Days Since Admission
--------------------------------------------------------------------------------

create table fource_LocalPatientClinicalCourse (
	siteid varchar(50) not null,
	patient_num int not null,
	days_since_admission int not null,
	calendar_date date not null,
	in_hospital int not null,
	severe int not null,
	deceased int not null,
	encounter_seq int not null
)

alter table fource_LocalPatientClinicalCourse add primary key (patient_num, days_since_admission)

insert into fource_LocalPatientClinicalCourse (siteid, patient_num, days_since_admission, calendar_date, in_hospital, severe, deceased, encounter_seq)
	select '', p.patient_num, 
		datediff(dd,p.admission_date,d.d) days_since_admission,
		d.d calendar_date,
		-- max(case when a.patient_num is not null then 1 else 0 end) in_hospital,   -- this is commented out as above
		max(case when a.patient_num is not null then 1 else 1 end) in_hospital,
		max(case when p.severe=1 and d.d>=p.severe_date then 1 else 0 end) severe,
		max(case when p.deceased=1 and d.d>=p.death_date then 1 else 0 end) deceased,
		max(case when a.patient_num is not null then 1 else 2 end) encounter_seq
	from fource_LocalPatientSummary p
		inner join fource_date_list d
			on d.d>=p.admission_date
		left outer join fource_admissions a
			on a.patient_num=p.patient_num 
				and a.admission_date>=p.admission_date 
				and a.admission_date<=d.d 
				and a.discharge_date>=d.d
	group by p.patient_num, p.admission_date, d.d

--------------------------------------------------------------------------------
-- Patient Observations: Selected Data Facts
--------------------------------------------------------------------------------

create table fource_LocalPatientObservations (
	siteid varchar(50) not null,
	patient_num int not null,
	days_since_admission int not null,
	concept_type varchar(50) not null,
	concept_code varchar(50) not null,
	value numeric(18,5) not null
)

alter table fource_LocalPatientObservations add primary key (patient_num, concept_type, concept_code, days_since_admission)

-- Diagnoses (3 character ICD9 codes) since 365 days before COVID
insert into fource_LocalPatientObservations (siteid, patient_num, days_since_admission, concept_type, concept_code, value)
	select distinct '',
		p.patient_num,
		datediff(dd,p.admission_date,cast(f.start_date as date)),
		'DIAG-ICD9',
		left(substring(f.concept_cd, len(code_prefix_icd9cm)+1, 999), 3)   -- changed the substring 3 -> 10
		+ left(substring(f.concept_cd, len(code_prefix_icd9cm)+5, 999), 10),
		-999
 	from fource_config x
		cross join observation_fact f
		inner join fource_covid_cohort p 
			on f.patient_num=p.patient_num 
				and f.start_date >= dateadd(dd,-365,p.admission_date)
	where concept_cd like code_prefix_icd9cm+'%' and code_prefix_icd9cm<>''

-- Diagnoses (3 character ICD10 codes) since 365 days before COVID
insert into fource_LocalPatientObservations (siteid, patient_num, days_since_admission, concept_type, concept_code, value)
	select distinct '',
		p.patient_num,
		datediff(dd,p.admission_date,cast(f.start_date as date)),
		'DIAG-ICD10',
		left(substring(f.concept_cd, len(code_prefix_icd10cm)+1, 999), 3)
		+ left(substring(f.concept_cd, len(code_prefix_icd10cm)+5, 999), 10),   -- changed the substring 3 -> 10
		-999
 	from fource_config x
		cross join observation_fact f
		inner join fource_covid_cohort p 
			on f.patient_num=p.patient_num 
				and f.start_date >= dateadd(dd,-365,p.admission_date)
	where concept_cd like code_prefix_icd10cm+'%' and code_prefix_icd10cm<>''

-- Procedures (ICD9) each day since COVID (only procedures used in 4CE Phase 1.1 to determine severity)
insert into fource_LocalPatientObservations (siteid, patient_num, days_since_admission, concept_type, concept_code, value)
	select distinct '', 
		p.patient_num,
		datediff(dd,p.admission_date,cast(f.start_date as date)),
		'PROC-ICD9',
		substring(f.concept_cd, len(code_prefix_icd9proc)+1, 999),
		-999
 	from fource_config x
		cross join observation_fact f
		inner join fource_covid_cohort p 
			on f.patient_num=p.patient_num 
				and f.start_date >= p.admission_date
	where concept_cd like code_prefix_icd9proc+'%' and code_prefix_icd9proc<>''
		and (
			-- Insertion of endotracheal tube
			f.concept_cd = x.code_prefix_icd9proc+'96.04'
			-- Invasive mechanical ventilation
			or f.concept_cd like x.code_prefix_icd9proc+'96.7[012]'
		)

-- Procedures (ICD10) each day since COVID (only procedures used in 4CE Phase 1.1 to determine severity)
insert into fource_LocalPatientObservations (siteid, patient_num, days_since_admission, concept_type, concept_code, value)
	select distinct '', 
		p.patient_num,
		datediff(dd,p.admission_date,cast(f.start_date as date)),
		'PROC-ICD10',
		substring(f.concept_cd, len(code_prefix_icd10pcs)+1, 999),
		-999
 	from fource_config x
		cross join observation_fact f
		inner join fource_covid_cohort p 
			on f.patient_num=p.patient_num 
				and f.start_date >= p.admission_date
	where concept_cd like code_prefix_icd10pcs+'%' and code_prefix_icd10pcs<>''
		and (
			-- Insertion of endotracheal tube
			f.concept_cd = x.code_prefix_icd10pcs+'0BH17EZ'
			-- Invasive mechanical ventilation
			or f.concept_cd like x.code_prefix_icd10pcs+'5A09[345]%'
		)



--------------------------------------------------------------------------------
-- Replace the patient_num with a random study_num integer if configured to do so
--------------------------------------------------------------------------------

create table fource_LocalPatientMapping (
	siteid varchar(50) not null,
	patient_num int not null,
	study_num int not null
)

alter table fource_LocalPatientMapping add primary key (patient_num, study_num)

if exists (select * from fource_config2 where replace_patient_num = 1)
begin
	insert into fource_LocalPatientMapping (siteid, patient_num, study_num)
		select '', patient_num, row_number() over (order by newid()) 
		from fource_LocalPatientSummary
	update t 
		set t.patient_num = m.study_num 
		from fource_LocalPatientSummary t 
			inner join fource_LocalPatientMapping m on t.patient_num = m.patient_num
	update t 
		set t.patient_num = m.study_num 
		from fource_LocalPatientClinicalCourse t 
			inner join fource_LocalPatientMapping m on t.patient_num = m.patient_num
	update t 
		set t.patient_num = m.study_num 
		from fource_LocalPatientObservations t 
			inner join fource_LocalPatientMapping m on t.patient_num = m.patient_num
end
else
begin
	insert into fource_LocalPatientMapping (siteid, patient_num, study_num)
		select '', patient_num, patient_num
		from fource_LocalPatientSummary
end

--------------------------------------------------------------------------------
-- Set the siteid to a unique value for your institution.
--------------------------------------------------------------------------------
update fource_LocalPatientSummary set siteid = (select siteid from fource_config)
update fource_LocalPatientClinicalCourse set siteid = (select siteid from fource_config)
update fource_LocalPatientObservations set siteid = (select siteid from fource_config)
update fource_LocalPatientMapping set siteid = (select siteid from fource_config)



--******************************************************************************
--******************************************************************************
--*** Finish up
--******************************************************************************
--******************************************************************************

--------------------------------------------------------------------------------
-- OPTION #1: Save the data as tables.
-- * Make sure everything looks reasonable.
-- * Export the tables to csv files.
--------------------------------------------------------------------------------
if exists (select * from fource_config2 where save_as_columns = 1)
begin
	declare @SaveAsTablesSQL nvarchar(max)
	select @SaveAsTablesSQL = '
		if (select object_id('''+save_as_prefix+'DailyCounts'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'DailyCounts;
		if (select object_id('''+save_as_prefix+'ClinicalCourse'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'ClinicalCourse;
		if (select object_id('''+save_as_prefix+'Demographics'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'Demographics;
		if (select object_id('''+save_as_prefix+'Labs'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'Labs;
		if (select object_id('''+save_as_prefix+'Diagnoses'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'Diagnoses;
		if (select object_id('''+save_as_prefix+'Medications'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'Medications;
		if (select object_id('''+save_as_prefix+'LocalPatientMapping'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'LocalPatientMapping;
		if (select object_id('''+save_as_prefix+'LocalPatientSummary'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'LocalPatientSummary;
		if (select object_id('''+save_as_prefix+'LocalPatientClinicalCourse'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'LocalPatientClinicalCourse;
		if (select object_id('''+save_as_prefix+'LocalPatientObservations'', ''U'') from fource_config2) is not null
			drop table '+save_as_prefix+'LocalPatientObservations;
		'
		from fource_config2
	exec sp_executesql @SaveAsTablesSQL
	select @SaveAsTablesSQL = '
		select * into '+save_as_prefix+'LocalPatientMapping from fource_LocalPatientMapping;
		select * into '+save_as_prefix+'LocalPatientSummary from fource_LocalPatientSummary;
		select * into '+save_as_prefix+'LocalPatientClinicalCourse from fource_LocalPatientClinicalCourse;
		select * into '+save_as_prefix+'LocalPatientObservations from fource_LocalPatientObservations;
		'
		from fource_config2
	exec sp_executesql @SaveAsTablesSQL
end

--------------------------------------------------------------------------------
-- OPTION #2: View the data as tables.
-- * Make sure everything looks reasonable.
-- * Copy into Excel, convert dates into YYYY-MM-DD format, save in csv format.
--------------------------------------------------------------------------------
if exists (select * from fource_config2 where output_as_columns = 1)
begin
	select * from fource_LocalPatientSummary order by admission_date, patient_num
	select * from fource_LocalPatientClinicalCourse order by patient_num, days_since_admission
	select * from fource_LocalPatientObservations order by patient_num, concept_type, concept_code, days_since_admission
	select * from fource_LocalPatientMapping order by patient_num
end

--------------------------------------------------------------------------------
-- OPTION #3: View the data as csv strings.
-- * Copy and paste to a text file, save it FileName.csv.
-- * Make sure it is not saved as FileName.csv.txt.
--------------------------------------------------------------------------------
if exists (select * from fource_config2 where output_as_csv = 1)
begin

	-- LocalPatientSummary
	select s LocalPatientSummaryCSV
		from (
			select 0 i, 'siteid,patient_num,admission_date,days_since_admission,last_discharge_date,still_in_hospital,'
				+'severe_date,severe,death_date,deceased,sex,age_group,race,race_collected' s
			union all 
			select row_number() over (order by admission_date, patient_num) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+convert(varchar(50),admission_date,23) --YYYY-MM-DD
				+','+cast(days_since_admission as varchar(50))
				+','+convert(varchar(50),last_discharge_date,23)
				+','+cast(still_in_hospital as varchar(50))
				+','+convert(varchar(50),severe_date,23)
				+','+cast(severe as varchar(50))
				+','+convert(varchar(50),death_date,23)
				+','+cast(deceased as varchar(50))
				+','+cast(sex as varchar(50))
				+','+cast(age_group as varchar(50))
				+','+cast(race as varchar(50))
				+','+cast(race_collected as varchar(50))
			from fource_LocalPatientSummary
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientClinicalCourse
	select s LocalPatientClinicalCourseCSV
		from (
			select 0 i, 'siteid,patient_num,days_since_admission,calendar_date,in_hospital,severe,deceased,encounter_seq' s
			union all 
			select row_number() over (order by patient_num, days_since_admission) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(days_since_admission as varchar(50))
				+','+convert(varchar(50),calendar_date,23) --YYYY-MM-DD
				+','+cast(in_hospital as varchar(50))
				+','+cast(severe as varchar(50))
				+','+cast(deceased as varchar(50))
				+','+cast(encounter_seq as varchar(50))
			from fource_LocalPatientClinicalCourse
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientObservations
	select s LocalPatientObservationsCSV
		from (
			select 0 i, 'siteid,patient_num,days_since_admission,concept_type,concept_code,value' s
			union all 
			select row_number() over (order by patient_num, concept_type, concept_code, days_since_admission) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(days_since_admission as varchar(50))
				+','+cast(concept_type as varchar(50))
				+','+cast(concept_code as varchar(50))
				+','+cast(value as varchar(50))
			from fource_LocalPatientObservations
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientMapping
	select s LocalPatientMappingCSV
		from (
			select 0 i, 'siteid,patient_num,study_num' s
			union all 
			select row_number() over (order by patient_num) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(study_num as varchar(50))
			from fource_LocalPatientMapping
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

end


--------------------------------------------------------------------------------
-- OPTION #4: View the data as csv strings.
-- * Copy and paste to a text file, save it FileName.csv.
-- * Make sure it is not saved as FileName.csv.txt.
--   DON'T INCLUDE ANY DATES
--------------------------------------------------------------------------------
if exists (select * from fource_config2 where output_as_csv_nodates = 1)
begin

	-- LocalPatientSummary
	select s LocalPatientSummaryCSV
		from (
			select 0 i, 'siteid,patient_num,admission_date,days_since_admission,last_discharge_date,still_in_hospital,'
				+'severe_date,severe,death_date,deceased,sex,age_group,race,race_collected' s
			union all 
			select row_number() over (order by admission_date, patient_num) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+ ''
				+','+cast(days_since_admission as varchar(50))
				+','+ ''
				+','+cast(still_in_hospital as varchar(50))
				+','+ ''
				+','+cast(severe as varchar(50))
				+','+ ''
				+','+cast(deceased as varchar(50))
				+','+cast(sex as varchar(50))
				+','+cast(age_group as varchar(50))
				+','+cast(race as varchar(50))
				+','+cast(race_collected as varchar(50))
			from fource_LocalPatientSummary
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientClinicalCourse
	select s LocalPatientClinicalCourseCSV
		from (
			select 0 i, 'siteid,patient_num,days_since_admission,calendar_date,in_hospital,severe,deceased,encounter_seq' s
			union all 
			select row_number() over (order by patient_num, days_since_admission) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(days_since_admission as varchar(50))
				+','+ ''
				+','+cast(in_hospital as varchar(50))
				+','+cast(severe as varchar(50))
				+','+cast(deceased as varchar(50))
				+','+cast(encounter_seq as varchar(50))
			from fource_LocalPatientClinicalCourse
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientObservations
	select s LocalPatientObservationsCSV
		from (
			select 0 i, 'siteid,patient_num,days_since_admission,concept_type,concept_code,value' s
			union all 
			select row_number() over (order by patient_num, concept_type, concept_code, days_since_admission) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(days_since_admission as varchar(50))
				+','+cast(concept_type as varchar(50))
				+','+cast(concept_code as varchar(50))
				+','+cast(value as varchar(50))
			from fource_LocalPatientObservations
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

	-- LocalPatientMapping
	select s LocalPatientMappingCSV
		from (
			select 0 i, 'siteid,patient_num,study_num' s
			union all 
			select row_number() over (order by patient_num) i,
				cast(siteid as varchar(50))
				+','+cast(patient_num as varchar(50))
				+','+cast(study_num as varchar(50))
			from fource_LocalPatientMapping
			union all select 9999999, '' --Add a blank row to make sure the last line in the file with data ends with a line feed.
		) t
		order by i

end
