/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
Only include rows with a concept that maps to the Measurement domain or where the source maps to the Measurement domain.
*/

INSERT INTO cdm5.measurement
(
  person_id,
  measurement_date,
  measurement_datetime,
  visit_occurrence_id,
  provider_id,
  measurement_concept_id,
  measurement_source_concept_id,
  measurement_source_value,
  measurement_type_concept_id
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

    -- measurement specific type codes. Note: these types belong to the 'Observation Type' vocabulary
    CASE medcode_intermediate.source_table
      WHEN 'clinical' THEN 38000245 -- EHR problem list entry
      WHEN 'referral' THEN 42898140 -- Referral record
      WHEN 'test' THEN 38000280 -- Observation recorded from EHR
      WHEN 'immunisation' THEN 38000280 -- Observation recorded from EHR
      ELSE 0
    END AS measurement_type_concept_id

  FROM medcode_intermediate AS medcode_intermediate
  WHERE target_domain_id = 'Measurement'
;