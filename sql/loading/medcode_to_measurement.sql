/*
Load from the medcode_merge: the union of the clinical, referral, test and immunisation tables.
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
    medcode_merge.person_id,

    medcode_merge._start_date,

    medcode_merge._start_datetime,

    medcode_merge.visit_occurrence_id,

    medcode_merge.provider_id,

    medcode_merge._concept_id,

    medcode_merge._source_concept_id,

    medcode_merge._source_value,

    -- measurement specific type codes. Note: these types belong to the 'Observation Type' vocabulary
    CASE medcode_merge.source_table
      WHEN 'clinical' THEN 38000245 -- EHR problem list entry
      WHEN 'referral' THEN 42898140 -- Referral record
      WHEN 'test' THEN 38000280 -- Observation recorded from EHR
      WHEN 'immunisation' THEN 38000280 -- Observation recorded from EHR
      ELSE 0
    END AS measurement_type_concept_id

  FROM medcode_merge AS medcode_merge
  WHERE target_domain_id = 'Measurement' OR (target_domain_id ISNULL AND source_domain_id = 'Measurement')
;