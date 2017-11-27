/**
Intermediate table for all (non-empty) hes diagnosis codes (icd).
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

    CASE
      WHEN createHesApptVisitId(hes_op_appt.attendkey, hes_op_clinical_diag.patid) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
        THEN createHesApptVisitId(hes_op_appt.attendkey, hes_op_clinical_diag.patid)
      ELSE NULL
    END AS visit_occurrence_id,

    hes_op_clinical_diag.newicd AS icd_code, -- is code with dot

    hes_op_clinical_diag.n AS diagnosis_position,

    NULLIF(NULLIF(hes_op_clinical.tretspef, '&'), '&amp;') :: INTEGER AS provider_id,

    'outpatient' AS provenance

  FROM caliber.hes_op_clinical_diag AS hes_op_clinical_diag
    JOIN caliber.hes_op_clinical AS hes_op_clinical USING (patid, attendkey)
    JOIN caliber.hes_op_appt AS hes_op_appt USING (patid, attendkey)

  UNION ALL

  SELECT
    hes_diag_epi.patid AS person_id,

    hes_diag_epi.epistart AS date,

    hes_diag_epi.spno AS visit_occurrence_id,

    hes_diag_epi.icd AS icd_code,

    hes_diag_epi.d_order AS diagnosis_position,

    NULL AS provider_id,

    'inpatient' AS provenance

  FROM caliber.hes_diag_epi AS hes_diag_epi
),
hes_diagnoses_icd_matched AS (
  SELECT hes_diagnoses.*,
    coalesce(icd_concept.concept_id, mapicdcode(hes_diagnoses.icd_code)) AS icd_match
  FROM hes_diagnoses
    LEFT JOIN cdm5.concept AS icd_concept
      ON hes_diagnoses.icd_code = icd_concept.concept_code
         AND icd_concept.vocabulary_id IN ('ICD10', 'ICD9CM')
  WHERE hes_diagnoses.icd_code IS NOT NULL
)
SELECT
  hes_diagnoses_icd_matched.*,
  icd_match as source_concept_id,
  coalesce(icd_map.concept_id_2, 0) AS target_concept_id,
  concept.domain_id                 AS target_domain_id

INTO public.hes_diagnoses_intermediate

FROM hes_diagnoses_icd_matched
  LEFT JOIN cdm5.concept_relationship AS icd_map
    ON hes_diagnoses_icd_matched.icd_match = icd_map.concept_id_1 AND
       icd_map.relationship_id = 'Maps to' AND
       icd_map.invalid_reason IS NULL
  LEFT JOIN cdm5.concept AS concept
    ON icd_map.concept_id_2 = concept.concept_id
