/*
OPCS procedure codes from episodes
*/
INSERT INTO cdm5.procedure_occurrence
(
  visit_occurrence_id,
  person_id,
  procedure_date,
  procedure_datetime,
  procedure_concept_id,
  procedure_source_value,
  procedure_type_concept_id,
  provider_id
)
  SELECT
    CASE
      WHEN hes_proc_epi.spno IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
      THEN hes_proc_epi.spno
      ELSE NULL
    END AS visit_occurrence_id,

    hes_proc_epi.patid AS person_id,

    COALESCE(hes_proc_epi.evdate, hes_proc_epi.epistart) AS procedure_date,

    COALESCE(hes_proc_epi.evdate, hes_proc_epi.epistart) :: TIMESTAMP AS procedure_datetime,

    COALESCE(target_concept.concept_id,0) AS procedure_concept_id,

    hes_proc_epi.opcs AS procedure_source_value,

    -- Note: specific place is lost here. Only primary/secondary differentiation
    CASE hes_proc_epi.p_order
      WHEN 1 THEN 44786630 -- Primary Procedure
      ELSE 44786631 -- Secondary Procedure
    END AS procedure_type_concept_id,

    -- TODO? According to the documentation, get this from pconsult field in the hes_episodes table (which is not present in dev environment)
    NULL AS provider_id

  FROM @source_schema.hes_proc_epi as hes_proc_epi
    LEFT JOIN cdm5.concept AS target_concept
      ON hes_proc_epi.opcs = replace(concept_code, '.', '')
         AND vocabulary_id = 'OPCS4'
         AND standard_concept = 'S'
  WHERE hes_proc_epi.opcs IS NOT NULL
        AND hes_proc_epi.opcs NOT IN ('&', '&amp;')
;