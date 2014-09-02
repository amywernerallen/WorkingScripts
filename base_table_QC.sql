----------------------------------------------------------------------
-- DATA QUALITY CHECKS for CHPW Warehouse Dev
-- Looking through base data tables to see if any anomolies, etc.
--
-- Set Database to 'CHPW_WAREHOUSE_DEV'
----------------------------------------------------------------------
-- General methodology for data quality check:
-- 
-- 1) Look at table to identify column names and check for blatant anomolies
-- 2) Check size of table and make sure that primary keys are unique
-- 3) Look into NULL values or other columns to examine distinct values
--   3a) Map these values to the seed table lookups when necessary
--
-- All problem scripts should be compiled at the end under PROBLEM header
--    Identify these with *** on each side
-- All observations should be noted in the script at the bottom of a section
--
-- Things to look out for:
-- * Numeric anomolies
-- * Null values or blanks
-- * Non-distinct values or duplicates
-- * Dates/times that are unreasonable
----------------------------------------------------------------------

------------------------- ALLERGY TABLE ------------------------------
select top 100 * from [dbo].[t_allergy]

select count(*) from [dbo].[t_allergy] --77694
	select count(distinct allergy_id) from [dbo].[t_allergy] --77694 (match? Y)

select distinct allergy_enc_id from [dbo].[t_allergy] order by allergy_enc_id --notice NULL value
select count(*) from [dbo].[t_allergy] where allergy_enc_id is NULL --49087

select distinct allergy_description from [dbo].[t_allergy] --list of allergies
select distinct allergy_type from [dbo].[t_allergy] --Drug, Non-Drug, and NULL
select distinct allergy_delete_ind from [dbo].[t_allergy] --Y/N
	select * from [dbo].[t_allergy] where allergy_delete_ind = 'Y' --4407

select distinct allergy_reaction from [dbo].[t_allergy] --list of reactions
select distinct allergy_start_date from [dbo].[t_allergy] order by allergy_start_date asc

-- Observations:
-- There are NULL values in allergy_enc_id : should that be the case?

----------------------- APPOINTMENT TABLE ----------------------------
select top 100 * from [dbo].[t_appointment]

select count(*) from [dbo].[t_appointment] --960879
	select count(distinct appt_original_id) from [dbo].[t_appointment] --960879 (match? Y)
	select count(distinct appt_id) from [dbo].[t_appointment] --960879 (match? Y)

select top 100 * from [dbo].[t_appointment] order by appt_patient_id --notice duplicate appt_patient_ids
select distinct appt_date from [dbo].[t_appointment] order by appt_date asc --some old dates
select distinct appt_kept_ind from [dbo].[t_appointment] --some are Y/N, some 1/0
select distinct appt_billed_ind from [dbo].[t_appointment] --NULL, N, Y
	select count(*) from [dbo].[t_appointment] where appt_billed_ind is NULL --153653

-- Observations:
-- Some appointment dates are back in the 40s and 50s

----------------------- ASSESSMENT TABLE ----------------------------
select top 100 * from [dbo].[t_assessment]

select count(*) from [dbo].[t_assessment] --830267
	select count(distinct assessment_id) from [dbo].[t_assessment] --830267 (match? Y)

select count(*) from t_assessment where assessment_id is NULL --0
select count(*) from t_assessment where encounter_id is NULL --613201

select count(distinct patient_id) from [dbo].[t_assessment] --68465
select count(distinct provider_id) from [dbo].[t_assessment] --158
select count(distinct original_id) from [dbo].[t_assessment] --830267
select distinct assessment_severity from [dbo].[t_assessment] --list of severities
select * from [dbo].[t_assessment] where icd9_code <> icd9_code_unscrubbed --empty

select distinct assessment_date from [dbo].[t_assessment] order by assessment_date asc --old

-- Observations:
-- Some assessment dates are back in the 30s and 40s

----------------------- CHARGECAPTURE TABLE ----------------------------
select top 100 * from [dbo].[t_chargecapture] 

select count(*) from [dbo].[t_chargecapture] --1226921
	select count(distinct cc_id) from t_chargecapture --1226921 (match? Y)

select count(*) from [dbo].[t_chargecapture] where cc_enc_id is NULL --1062803
select * from [dbo].[t_chargecapture] where cc_cpt_Code not Like '9%' --some non-standard cpt codes?

--**************************
select top 100 * from [dbo].[t_chargecapture] order by cc_amount --negative values
select top 100 * from [dbo].[t_chargecapture] order by cc_amount desc

	-- all one center?
	select distinct cc_rendering_provider_id, cc_orig_site_id, cc_amount from [dbo].[t_chargecapture] order by cc_amount 
	select top 100 cc_amount, prov_usual_location_unscrubbed from
		t_chargecapture INNER JOIN Provider_Master
		on t_chargecapture.cc_rendering_provider_id = Provider_Master.prov_id
	order by cc_amount desc
--**************************

select distinct cc_date_of_service from [dbo].[t_chargecapture] order by cc_date_of_service asc

-- Observations:
-- Again lots of NULL values for cc_enc_id : is this unexpected?
-- Some CPT codes seem irregular (eg. RX, 00000, ones longer than 5 digits)
-- Negative charges in cc_amount column : see problem section below

----------------------- CONTACTINFO TABLE ----------------------------
select top 100 * from [dbo].[t_contactinfo]
select count(*) from [dbo].[t_contactinfo] --This table is empty (to be updated)

-- Observations:
-- 

------------------ CONTACT INFO RELATIONSHIP TABLE --------------------
select * from [dbo].[t_contactinfo_relationship]
select * from CHPW_STAGING_DEV.lookup.contactinfo_relationship

-- Observations:
-- These two tables do not match : should they?

----------------------- DIAGNOSIS TABLE ----------------------------
select * from [dbo].[t_diagnosis] --This table is empty (deprecated)


----------------------- ENCOUNTER TABLE ----------------------------
select top 100 * from [dbo].[t_encounter]

select count(*) from t_encounter --1512679
	select count(distinct enc_id) from t_encounter --1512679 (match? Y)

select count(*) from t_encounter where enc_id is NULL --0
select distinct enc_type from t_encounter --different types of encounters
select distinct enc_delete_ind from t_encounter --all Ns

select distinct enc_timestamp from t_encounter order by enc_timestamp asc
select distinct enc_medical_ind from t_encounter --Y/N

-- Observations:
-- Some encounter time stamps date back to the 30s and 40s

----------------------- IMMUNIZATION TABLE ----------------------------
select top 100 * from [dbo].[t_immunization]

select count(*) from t_immunization --14392
	select count(distinct imm_id) from t_immunization --14392 (match? Y)

select count(*) from t_immunization where enc_id is NULL --14392
select distinct completed_date from t_immunization order by completed_date asc

select distinct imm_type, cpt_code from t_immunization --Flu and PCV --diff cpt codes
select distinct imm_type, cpt_code from t_immunization where imm_type = 'Flu'

-- Observations:
-- All enc_ids are NULL : should that be the case?

----------------------- LAB RESULTS TABLE ----------------------------
select top 100 * from t_labResult --empty (now Results table)


----------------------- MAINTENANCE TABLE ----------------------------
select top 100 * from t_maintenance

select count(*) from t_maintenance --2721743
select count(distinct(maint_id)) from t_maintenance --2721743

select top 100 * from t_maintenance where maint_enc_id is not NULL --all maint-enc-ids are null
select distinct maint_observation from t_maintenance

-- Observations:
-- All maint_enc_ids are NULL : should that be the case?

----------------------- MEDICATION TABLE ----------------------------
select top 100 * from t_medication --this table is empty (deprecated)


--------------------- MEDICATION LIST TABLE --------------------------
select top 100 * from t_medicationlist 

select count(*) from t_medicationlist --123348
select count(distinct patient_id) from t_medicationlist --14979

select count(*) from t_medicationlist where ndc_code is NULL --7145
select * from t_medicationlist where start_date is NULL --none
select * from t_medicationlist where delete_ind = 'Y' --358 cases
select distinct location_unscrubbed from t_medicationlist --9 locations

select distinct start_date from t_medicationlist order by start_date asc

-- Observations:
-- Nothing to note

----------------------- ORDER TABLE ----------------------------
select top 100 * from t_order

select count(*) from t_order --1038078
select count(*) from t_order where enc_id is NULL --1038078 all encounter ids are null
select distinct order_type from t_order -- all needs update 
select cpt_Code from t_order where cpt_Code not Like '9%' --some strange cpt codes

-- Observations:
-- All enc_ids are NULL : should this be the case?
-- A lot of NULL values/columns : perhaps not fully updated yet?

----------------------- PATIENT TABLE ---------------------------- 
select top 100 * from t_patient

-- Note: some queries restricted to NewHP only (site_id 6)

select count(*) from t_patient --111943
select count(distinct pat_id) from t_patient --111943 (match? Y)
select distinct pat_race from t_patient --check this against staging seed table

--American Indian/Alaskan Native
--Asian
--Black/African American
--More Than One Race
--Native Hawaiian
--Other
--Other Pacific Islander
--Unreported/Refused to report
--White
( -- select distinct destination from lookup.race order by destination)

select distinct pat_ethnicity from t_patient --also check against lookup table
select distinct pat_income from t_patient where pat_orig_site_id = '6' order by pat_income desc --NULL
select distinct pat_family_size from t_patient where pat_orig_site_id = '6' order by pat_family_size desc --NULL

select distinct pat_financial_class from t_patient --all NULL
select distinct pat_date_of_birth from t_patient where pat_orig_site_id = '6' order by pat_date_of_birth 
	select * from t_patient where pat_date_of_birth = '1801-01-01 00:00:00.000'
	select * from t_patient where pat_date_of_birth = '1850-01-01 00:00:00.000' -- test patient

select distinct pat_sex from t_patient --F/M/O (what is 0?)
select distinct pat_birth_country from t_patient -- all NULL

select distinct pat_marital_status from t_patient -- all NULL
select distinct pat_homelessness_status from t_patient --shouldn't null be unreported?
select top 100 * from t_patient where pat_delete_ind = 'Y' --no deletes

select count(distinct pat_medical_record_number) from t_patient --111912
select count(distinct pat_id) from t_patient --111943

-- Observations:
-- Some issues with birth date : probably 1801 is default no-entry?
-- Why would pat_id not be one-to-one with pat_MRN?

----------------------- PAYER TABLE ----------------------------
select top 100 * from t_payer 

select count(*) from t_payer --200098
	select count(distinct payer_id) from t_payer --200098 (match? Y)

select distinct policy_start_dt from t_payer where payer_orig_site_id = '6' order by policy_start_dt --1900? seems odd
select * from t_payer where policy_start_dt = '1900-01-01 00:00:00.000'
	select * from t_patient where pat_id = '3E748AF3-3007-47E8-A776-6C3CA1C59A1F' 
	select * from t_patient where pat_id = '311D459F-B2BA-4713-886D-B20F5D898385' 
select distinct policy_start_dt from t_payer order by policy_start_dt desc --2199? seems odd

-- Observations:
-- Policy start dates go back to the early 1900s : is this reasonable?
-- Policy start dates go up to 2199 : is this reasonable?

----------------------- PRESCRIPTION TABLE ----------------------------
select top 100 * from t_prescription 

select count(*) from t_prescription --743930
select count(distinct patient_id) from t_prescription --54875
select count(*) from t_prescription where enc_id is NOT NULL --0

select quantity from t_prescription order by quantity desc --not standardized
select distinct prescription_method from t_prescription 

select distinct entry_timestamp from t_prescription order by entry_timestamp asc

-- Observations:
-- All enc_ids are NULL : is that expected?

----------------------- PROBLEM TABLE ----------------------------
select top 100 * from t_problem 

select count(*) from t_problem --324123
select count(distinct patient_id) from t_problem --50481

select icd9_code, icd9_code_unscrubbed from t_problem where icd9_code <> icd9_code_unscrubbed
--V20.2 <> V202
select distinct onset_date from t_problem order by onset_date -- dates back from 1938

-- Observations:
-- None to note

----------------------- RESULT TABLE ----------------------------
select top 100 * from t_result 

select count(*) from t_result --3716327
select count(distinct result_id) from t_result --3716327 (match? Y)

select distinct collection_date from t_result order by collection_date -- 1900?

-- Observations:
-- One collection date from 1900; some from 1980s : is this correct:

----------------------- VITALS TABLE ----------------------------
select top 100 * from t_vitals 

select count(*) from t_vitals --471513
select count(distinct vitals_id) from t_vitals --471513

select distinct vitals_height, vitals_height_unit from t_vitals order by vitals_height --not good
select distinct vitals_weight, vitals_weight_unit from t_vitals order by vitals_weight --901126 lbs??
select distinct vitals_BMI from t_vitals order by vitals_BMI 
select distinct vitals_systolic from t_vitals order by vitals_systolic 
select distinct vitals_diastolic from t_vitals order by vitals_diastolic 
select distinct vitals_entry_timestamp from t_vitals order by vitals_entry_timestamp --only three entries?
select distinct vitals_temperature, vitals_temperature_units from t_vitals order by vitals_temperature 
select distinct vitals_heart_rate from t_vitals order by vitals_heart_rate 

select distinct vitals_height, vitals_height_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_height 
select distinct vitals_weight, vitals_weight_unit from t_vitals where vitals_orig_site_id = '6' order by vitals_weight 
select distinct vitals_BMI from t_vitals where vitals_orig_site_id = '6' order by vitals_BMI 
select distinct vitals_systolic from t_vitals where vitals_orig_site_id = '6' order by vitals_systolic 
select distinct vitals_diastolic from t_vitals where vitals_orig_site_id = '6' order by vitals_diastolic 
select distinct vitals_entry_timestamp from t_vitals where vitals_orig_site_id = '6' order by vitals_entry_timestamp --only three entries?
select distinct vitals_temperature, vitals_temperature_units from t_vitals where vitals_orig_site_id = '6' order by vitals_temperature 
select distinct vitals_heart_rate from t_vitals where vitals_orig_site_id = '6' order by vitals_heart_rate

select avg(vitals_height) from t_vitals where vitals_orig_site_id = '6' --64.26
select avg(vitals_weight) from t_vitals where vitals_orig_site_id = '6' --172.57
select avg(vitals_BMI) from t_vitals where vitals_orig_site_id = '6' --31.94
select avg(vitals_systolic) from t_vitals where vitals_orig_site_id = '6' --121
select avg(vitals_diastolic) from t_vitals where vitals_orig_site_id = '6' --73
select avg(vitals_temperature) from t_vitals where vitals_orig_site_id = '6' --98.15
select avg(vitals_heart_rate) from t_vitals where vitals_orig_site_id = '6' --88.36

-- Observations:
-- See averages above; R script goes into more detail on these

----------------------------------------------------------------------------------
-- ******************************* PROBLEMS ***************************************
select distinct pat_income from t_patient order by pat_income desc
-- very high patient income (no values for NewHP yet)

select distinct pat_family_size from t_patient order by pat_family_size desc
-- very high family size (no values for NewHP yet)

select distinct pat_date_of_birth from t_patient order by pat_date_of_birth
select * from t_patient where pat_date_of_birth = '1850-01-01 00:00:00.000'
-- someone born in 1801 and 1850: seems unreasonable

select top 100 * from [dbo].[t_chargecapture] order by cc_amount --negative values
select top 100 * from [dbo].[t_chargecapture] order by cc_amount desc --positive values
-- high negative charges (balanced by high positive charges) coming from variety of centers

select distinct collection_date from t_result order by collection_date
select * from t_result where collection_date = '1900-01-01 00:00:00.000'
-- collection date all the way back in 1900

select distinct policy_start_dt from t_payer order by policy_start_dt
select * from t_payer where policy_start_dt = '2112-07-08 00:00:00.000'
-- start dates back to 1900 and all the way up to 2199

select distinct vitals_height from t_vitals order by vitals_height
-- this seems messy: see supplemental R scripts for further analysis
