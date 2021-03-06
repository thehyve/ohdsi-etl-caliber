/**
Intermediate table for all (non-empty) hes diagnosis codes (icd).
Combines diagnoses from two hes tables: hes_op_clinical and hes_diag_epi (episodes)
Pre-processes the mapping from icd10 to a standard target concept id
 */
DROP TABLE IF EXISTS public.hes_diagnoses_intermediate;

WITH hes_diagnoses(
  person_id,
  date,
  visit_occurrence_id,
  icd_code,
  diagnosis_position,
  provider_id,
  provenance
) AS (
  SELECT
    hes_op_clinical_diag.patid AS person_id,

    hes_op_appt.apptdate AS date,

    createHesApptVisitId(hes_op_appt.attendkey, hes_op_clinical_diag.patid) AS visit_occurrence_id,

    -- newicd is the cleaned icd and contains a dot
    hes_op_clinical_diag.newicd AS icd_code,

    hes_op_clinical_diag.n AS diagnosis_position,

    NULLIF(NULLIF(hes_op_clinical.tretspef, '&'), '&amp;') :: INTEGER AS provider_id,

    'outpatient' AS provenance

  FROM @source_schema.hes_op_clinical_diag AS hes_op_clinical_diag
    JOIN @source_schema.hes_op_clinical AS hes_op_clinical USING (patid, attendkey)
    JOIN @source_schema.hes_op_appt AS hes_op_appt USING (patid, attendkey)

  UNION ALL

  SELECT
    hes_diag_epi.patid AS person_id,

    hes_diag_epi.epistart AS date,

    hes_diag_epi.spno AS visit_occurrence_id,

    hes_diag_epi.icd AS icd_code,

    hes_diag_epi.d_order AS diagnosis_position,

    NULL AS provider_id,

    'inpatient' AS provenance

  FROM @source_schema.hes_diag_epi AS hes_diag_epi
),
hes_diagnoses_icd_matched AS (
  -- Map ICD code to concept_id. Same subquery also used in death.sql
  SELECT hes_diagnoses.*,
         coalesce(icd_concept.concept_id, mapicdcode(hes_diagnoses.icd_code)) AS icd_concept_id
  FROM hes_diagnoses
    LEFT JOIN cdm5.concept AS icd_concept
      ON hes_diagnoses.icd_code = icd_concept.concept_code
         AND icd_concept.vocabulary_id IN ('ICD10', 'ICD9CM')
  WHERE hes_diagnoses.icd_code IS NOT NULL
)
SELECT
  hes_diagnoses.*,
  icd_concept_id AS source_concept_id,
  coalesce(icd_map.target_concept_id, 0) AS target_concept_id,
  coalesce(target_domain_id, source_domain_id) AS target_domain_id

INTO public.hes_diagnoses_intermediate

FROM hes_diagnoses_icd_matched AS hes_diagnoses
  LEFT JOIN cdm5.source_to_target AS icd_map
    ON hes_diagnoses.icd_concept_id = icd_map.source_concept_id
;