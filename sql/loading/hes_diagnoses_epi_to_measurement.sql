/*
ICD10 diagnosis codes from episodes.
*/
INSERT INTO cdm5.measurement
(
  person_id,
  measurement_date,
  measurement_datetime,
  visit_occurrence_id,
  measurement_concept_id,
  measurement_source_concept_id,
  measurement_source_value,
  measurement_type_concept_id,
  provider_id
)
SELECT
  hes_diagnoses.person_id AS person_id,

  hes_diagnoses.date AS measurement_start_date,

  hes_diagnoses.date :: TIMESTAMP AS measurement_start_datetime,

  hes_diagnoses.visit_occurrence_id AS visit_occurrence_id,

  coalesce(hes_diagnoses.target_concept_id, 0) AS measurement_concept_id,

  hes_diagnoses.source_concept_id AS measurement_source_concept_id,

  hes_diagnoses.icd_code AS measurement_source_value,

  -- EHR problem list entry
  38000245 measurement_type_concept_id,

  hes_diagnoses.provider_id AS provider_id

FROM public.hes_diagnoses_intermediate AS hes_diagnoses
WHERE target_domain_id = 'Measurement'
;
