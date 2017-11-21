/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
Only include rows with a concept that maps to the Condition domain or where the source maps to the condition domain.
*/

INSERT INTO cdm5.condition_occurrence
(
  person_id,
  condition_start_date,
  condition_start_datetime,
  visit_occurrence_id,
  provider_id,
  condition_concept_id,
  condition_source_concept_id,
  condition_source_value,
  condition_type_concept_id
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

    -- condition  specific codes
    CASE medcode_intermediate.source_table
      WHEN 'clinical' THEN 38000245 -- EHR problem list entry
      WHEN 'referral' THEN 42898140 -- Referral record
      WHEN 'test' THEN 38000276 -- Problem list from EHR
      WHEN 'immunisation' THEN 38000276 -- Problem list from EHR
      ELSE 0
    END AS condition_type_concept_id

  FROM medcode_intermediate AS medcode_intermediate
  WHERE target_domain_id = 'Condition' OR (target_domain_id ISNULL AND source_domain_id = 'Condition')
;
