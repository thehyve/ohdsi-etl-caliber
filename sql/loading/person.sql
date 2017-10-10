-- Script to fill the person table with data from the patient and hes_patient CALIBER tables

INSERT INTO person
(
	person_id,
	person_source_value,
	care_site_id,
	gender_concept_id,
	gender_source_value,
	year_of_birth,
	month_of_birth,
	day_of_birth,
	birth_datetime,
	race_concept_id,
	race_source_value,
	ethnicity_concept_id,
	ethnicity_source_value,
	location_id,
	provider_id,
	gender_source_concept_id,
	race_source_concept_id,
	ethnicity_source_concept_id
)
SELECT (
	p.patid AS person_id,
	p.patid AS person_source_value,
	p.pracid AS care_site_id,

	CASE
	WHEN p.gender = 2 THEN 8532 -- FEMALE
	WHEN p.gender = 1 THEN 8507 -- MALE
	ELSE 0 -- Data not entered | Indeterminate | Unknown
	END
	AS gender_concept_id,
	
	p.gender AS gender_source_concept_id,

	/*
	-- Should gender source values be constructed?
	-- p.gender AS gender_source_value,
	0 - Data not entered => UNKNOWN
	1 => MALE
	2 => FEMALE
	3 - Indeterminate => UNKNOWN
	4 - Unknown => UNKNOWN
	*/

	p.yob AS year_of_birth, 
	
	CASE WHEN p.mob=0 THEN NULL 
	ELSE p.mob END
	AS month_of_birth,

	CASE -- Sorted by prevalence
	WHEN hesp.gen_ethnicity = 'White' THEN 8527
	WHEN hesp.gen_ethnicity = 'Unknown' THEN 0 -- was 8552
	WHEN hesp.gen_ethnicity = 'Indian' THEN 38003574
	WHEN hesp.gen_ethnicity = 'Other' THEN 0 -- was 8522
	WHEN hesp.gen_ethnicity = 'Oth_Asian' THEN 8515
	WHEN hesp.gen_ethnicity = 'Pakistani' THEN 38003589
	WHEN hesp.gen_ethnicity = 'Bl_Afric' THEN 38003598
	WHEN hesp.gen_ethnicity = 'Mixed' THEN 0 -- was 8522
	WHEN hesp.gen_ethnicity = 'Bl_Carib' THEN 38003598
	WHEN hesp.gen_ethnicity = 'Bl_Other' THEN 38003598
	WHEN hesp.gen_ethnicity = 'Bangladesi' THEN 38003598
	WHEN hesp.gen_ethnicity = 'Chinese' THEN 38003579
	ELSE 0
	END
	AS race_concept_id,
	
	hesp.gen_ethnicity AS race_source_value,

	0 AS ethnicity_concept_id

FROM patient p 
LEFT JOIN hes_patient hesp 
ON p.patid = hesp.patid
-- Do not insert patients with an accept value of 0
WHERE p.accept = 1

-- TODO exclude patients with observation end dates before observation start date

;
