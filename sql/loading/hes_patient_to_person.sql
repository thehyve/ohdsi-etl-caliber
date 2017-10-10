/*
Join on patient.patid = hes_patient.patid
*/
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
SELECT
 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	person_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	person_source_value,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	care_site_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	gender_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	gender_source_value,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	year_of_birth,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	month_of_birth,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	day_of_birth,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	birth_datetime,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	race_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	race_source_value,

 -- [VALUE   COMMENT] Patient’s ethnicity derived from all HES data (including HES outpatient, HES admitted patient care and HES A&E) 
 -- [MAPPING   LOGIC] See mapping in JNJ CPRD_ETL_OMOP_V5.0  Chinese Chinese 3800357 Bangladesi Bangladeshi 3800357 Pakistani Pakistani 3800358 Unknown Unknown 855 Oth_Asia Asian 851 Mixed Other Rac 852 White Whit 852 Bl_Afric Blac 3800359 Indian Asian India 3800357 Bl_Other Blac 3800359 Bl_Carib Blac 3800359 Other Other Rac 852 
 -- [MAPPING COMMENT] Check: Race or Ethnicity? 
	hes_patient.gen_ethnicity	AS	ethnicity_concept_id,

 -- [VALUE   COMMENT] Patient’s ethnicity derived from all HES data (including HES outpatient, HES admitted patient care and HES A&E) 
	hes_patient.gen_ethnicity	AS	ethnicity_source_value,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	location_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	provider_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	gender_source_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	race_source_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	ethnicity_source_concept_id

FROM hes_patient
;