/*
ICD10 diagnosis codes from episodes.
*/
INSERT INTO cdm5.condition_occurrence
(
	person_id, -- The CPRD patient identifier is already anonymised and can be used.
	condition_start_date,
  condition_start_datetime,
  visit_occurrence_id, -- Join onto visit_occurrence table. If record date in the condition_occurrence file for that patient exists in the visit_occurrence file then create visit_occurrence_id with formula below. If not, visit_occurrrence_id is null  Patid * 10000000000 + (year(eventdate)*10000) +  (month(eventdate)* 100) + day(eventdate)  Or   null
  condition_concept_id,
  condition_source_concept_id,
  condition_source_value,
  condition_type_concept_id,
  provider_id
)
SELECT
  hes_diagnoses.person_id AS person_id,

  hes_diagnoses.date AS condition_start_date,

  hes_diagnoses.date :: TIMESTAMP AS condition_start_datetime,

  hes_diagnoses.visit_occurrence_id AS visit_occurrence_id,

  coalesce(hes_diagnoses.target_concept_id, 0) AS condition_concept_id,

  hes_diagnoses.source_concept_id AS condition_source_concept_id,

  hes_diagnoses.icd_code AS condition_source_value,

  -- TODO: inpatient vs outpatient distinction from hes_diagnoses.provenance
  CASE diagnosis_position
    WHEN  1 THEN 38000184
    WHEN  2 THEN 38000185
    WHEN  3 THEN 38000186
    WHEN  4 THEN 38000187
    WHEN  5 THEN 38000188
    WHEN  6 THEN 38000189
    WHEN  7 THEN 38000190
    WHEN  8 THEN 38000191
    WHEN  9 THEN 38000192
    WHEN 10 THEN 38000193
    WHEN 11 THEN 38000194
    WHEN 12 THEN 38000195
    WHEN 13 THEN 38000196
    WHEN 14 THEN 38000197
    WHEN 15 THEN 38000198
    WHEN 16 THEN 44818709
    WHEN 17 THEN 44818710
    WHEN 18 THEN 44818711
    WHEN 19 THEN 44818712
    ELSE 44818713
  END AS condition_type_concept_id,

  hes_diagnoses.provider_id AS provider_id

FROM public.hes_diagnoses_intermediate AS hes_diagnoses
WHERE target_domain_id = 'Condition'
;