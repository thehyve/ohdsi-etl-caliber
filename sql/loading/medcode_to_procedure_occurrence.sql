/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
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
    medcode_intermediate.person_id,

    medcode_intermediate._start_date,

    medcode_intermediate._start_datetime,

    medcode_intermediate.visit_occurrence_id,

    medcode_intermediate.provider_id,

    medcode_intermediate._concept_id,

    medcode_intermediate._source_concept_id,

    medcode_intermediate._source_value,

    -- procedure specific type codes
    CASE medcode_intermediate.source_table
      WHEN 'clinical' THEN 38000275 -- EHR order list entry
      WHEN 'referral' THEN 42898141 -- Referral record
      WHEN 'test' THEN 38003621 -- Procedure recorded as lab test
      WHEN 'immunisation' THEN 43542354 -- Physician administered drug (identified as procedure)
      ELSE 0
    END AS procedure_type_concept_id

  FROM medcode_intermediate AS medcode_intermediate
  -- If from immunisation table, the immunisation status has to be 'Given'
  WHERE target_domain_id = 'Procedure' AND (
    medcode_intermediate.immunisation_status = 'Given' OR
    lower(medcode_intermediate.source_table) NOT LIKE 'immunisation'
  )
;
