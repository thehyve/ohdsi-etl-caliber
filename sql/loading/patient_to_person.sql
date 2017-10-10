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
	patient.patid	AS	person_id,

	patient.patid	AS	person_source_value,

	patient.pracid	AS	care_site_id,

 -- [MAPPING   LOGIC] 0 - Data not entered => UNKNOWN 1 => MALE 2 => FEMALE 3 - Indeterminate => UNKNOWN 4 - Unknown => UNKNOWN 
	patient.gender	AS	gender_concept_id,

	patient.gender	AS	gender_source_value,

	patient.yob	AS	year_of_birth,

 -- [VALUE   COMMENT] Only captured when registerd as a baby's 
 -- [MAPPING   LOGIC] 0 => null 
	patient.mob	AS	month_of_birth,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	day_of_birth,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	birth_datetime,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	race_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	race_source_value,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	ethnicity_concept_id,

 -- [!WARNING!] no source column found. See possible comment at the INSERT INTO
	NULL	AS	ethnicity_source_value,

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

FROM patient
;