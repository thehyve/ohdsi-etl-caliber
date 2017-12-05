/*
ICD10 diagnosis codes from episodes.
*/
INSERT INTO cdm5.procedure_occurrence
(
  person_id,
  procedure_date,
  procedure_datetime,
  visit_occurrence_id,
  procedure_concept_id,
  procedure_source_concept_id,
  procedure_source_value,
  procedure_type_concept_id,
  provider_id
)
SELECT
  hes_diagnoses.person_id AS person_id,

  hes_diagnoses.date AS procedure_start_date,

  hes_diagnoses.date :: TIMESTAMP AS procedure_start_datetime,

  -- Null if id does not exist in visit_occurrence
  visit_occurrence.visit_occurrence_id AS visit_occurrence_id,

  coalesce(hes_diagnoses.target_concept_id, 0) AS procedure_concept_id,

  hes_diagnoses.source_concept_id AS procedure_source_concept_id,

  hes_diagnoses.icd_code AS procedure_source_value,

  CASE hes_diagnoses.diagnosis_position
    WHEN 1 THEN 44786630 -- Primary Procedure
    ELSE 44786631 -- Secondary Procedure
  END AS procedure_type_concept_id,

  hes_diagnoses.provider_id AS provider_id

FROM public.hes_diagnoses_intermediate AS hes_diagnoses
  LEFT JOIN cdm5.visit_occurrence USING (visit_occurrence_id)
WHERE target_domain_id = 'Procedure'
;