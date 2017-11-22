/*
OPCS procedure codes from outpatient records
TODO: remove checks for &amp; in production (for provider_id and where clause). This is a translation error in the dev database.
*/
INSERT INTO cdm5.procedure_occurrence
(
  person_id,
  procedure_date,
  procedure_datetime,
  visit_occurrence_id,
  procedure_concept_id,
  procedure_source_value,
  procedure_type_concept_id,
  provider_id
)
  SELECT
    hes_op_clinical.patid AS person_id,

    hes_op_appt.apptdate AS procedure_date,

    hes_op_appt.apptdate :: TIMESTAMP AS procedure_datetime,

    -- TODO
    NULL AS visit_occurrence_id,

    COALESCE(target_concept.concept_id,0) AS procedure_concept_id,

    hes_op_clinical_proc.opcs AS procedure_source_value,

    -- Note: specific place is lost here. Only primary/secondary differentiation
    CASE hes_op_clinical_proc.n
      WHEN 1 THEN 44786630 -- Primary Procedure
      ELSE 44786631 -- Secondary Procedure
    END AS procedure_type_concept_id,

    NULLIF(NULLIF(hes_op_clinical.tretspef, '&'), '&amp;') :: INTEGER AS provider_id

  FROM caliber.hes_op_clinical_proc AS hes_op_clinical_proc
    JOIN caliber.hes_op_clinical AS hes_op_clinical USING (patid, attendkey)
    JOIN caliber.hes_op_appt AS hes_op_appt USING (patid, attendkey)
    LEFT JOIN cdm5.concept AS target_concept
      ON hes_op_clinical_proc.opcs = replace(concept_code, '.', '')
         AND vocabulary_id = 'OPCS4'
         AND standard_concept = 'S'
  WHERE hes_op_clinical_proc.opcs IS NOT NULL
        -- '-', 'X999' = No procedure carried out
        -- '&', 'X997' = Not known
        AND hes_op_clinical_proc.opcs NOT IN ('X997', 'X999', '-', '&', '&amp;', '0000')
;