/*
ICD10 diagnosis codes from episodes.
*/
INSERT INTO cdm5.observation
(
  person_id,
  observation_date,
  observation_datetime,
  visit_occurrence_id,
  observation_concept_id,
  observation_source_concept_id,
  observation_source_value,
  observation_type_concept_id,
  provider_id,
  value_as_concept_id
)
SELECT
  hes_diagnoses.person_id AS person_id,

  hes_diagnoses.date AS observation_start_date,

  hes_diagnoses.date :: TIMESTAMP AS observation_start_datetime,

  -- Null if id does not exist in visit_occurrence
  visit_occurrence.visit_occurrence_id AS visit_occurrence_id,

  coalesce(hes_diagnoses.target_concept_id, 0) AS observation_concept_id,

  hes_diagnoses.source_concept_id AS observation_source_concept_id,

  hes_diagnoses.icd_code AS observation_source_value,

  -- EHR problem list entry
  38000245 observation_type_concept_id,

  hes_diagnoses.provider_id AS provider_id,

  -- Yes
  4188539 AS value_as_concept_id

FROM public.hes_diagnoses_intermediate AS hes_diagnoses
  LEFT JOIN cdm5.visit_occurrence USING (visit_occurrence_id)
-- All other domains
WHERE target_domain_id NOT IN ('Condition','Measurement','Procedure')
;
