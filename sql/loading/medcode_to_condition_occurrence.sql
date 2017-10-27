/*
Load from the medcode_merge: the union of the clinical, referral, test and immunisation tables.
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
    medcode_merge.person_id,

    medcode_merge._start_date,

    medcode_merge._start_datetime,

    medcode_merge.visit_occurrence_id,

    medcode_merge.provider_id,

    medcode_merge._concept_id,

    medcode_merge._source_concept_id,

    medcode_merge._source_value,

    -- condition  specific codes
    CASE medcode_merge.source_table
      WHEN 'clinical' THEN 38000245 -- EHR problem list entry
      WHEN 'referral' THEN 42898140 -- Referral record
      WHEN 'test' THEN 38000276 -- Problem list from EHR
      WHEN 'immunisation' THEN 38000276 -- Problem list from EHR
      ELSE 0
    END AS condition_type_concept_id

  FROM medcode_merge AS medcode_merge
  WHERE target_domain_id = 'Condition' OR (target_domain_id ISNULL AND source_domain_id = 'Condition')
;
