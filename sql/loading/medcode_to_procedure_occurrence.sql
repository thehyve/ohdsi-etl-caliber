/*
Load from the medcode_merge: the union of the clinical, referral, test and immunisation tables.
Only include rows with a concept that maps to the Procedure domain or where the source maps to the procedure domain.
*/

INSERT INTO cdm5.procedure_occurrence
(
  person_id,
  procedure_date,
  procedure_datetime,
  visit_occurrence_id,
  provider_id,
  procedure_concept_id,
  procedure_source_concept_id,
  procedure_source_value,
  procedure_type_concept_id
)
  SELECT
    medcode_merge.person_id,

    medcode_merge._start_date,

    medcode_merge._start_datetime,

    medcode_merge.visit_occurrence_id,

    medcode_merge.provider_id,

    medcode_merge._concept_id,

    medcode_merge._source_concept_id,

    medcode_merge._source_value,

    -- procedure specific type codes
    CASE medcode_merge.source_table
      WHEN 'clinical' THEN 38000275 -- EHR order list entry
      WHEN 'referral' THEN 42898141 -- Referral record
      WHEN 'test' THEN 38003621 -- Procedure recorded as lab test
      WHEN 'immunisation' THEN 43542354 -- Physician administered drug (identified as procedure)
      ELSE 0
    END AS procedure_type_concept_id

  FROM medcode_merge AS medcode_merge
  WHERE target_domain_id = 'Procedure' OR (target_domain_id ISNULL AND source_domain_id = 'Procedure')
;
