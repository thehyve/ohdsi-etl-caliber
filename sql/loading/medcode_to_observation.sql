/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
Include rows with a concept that maps to the Observation domain or where the source maps to the Observation domain.
Also include unmapped concepts, as most read codes map to an observation.
*/

INSERT INTO cdm5.observation
(
  person_id,
  observation_date,
  observation_datetime,
  visit_occurrence_id,
  provider_id,
  observation_concept_id,
  observation_source_concept_id,
  observation_source_value,
  observation_type_concept_id,
  value_as_concept_id
)
  SELECT
    medcode_intermediate.person_id,

    medcode_intermediate._start_date,

    medcode_intermediate._start_datetime,

    -- Null if id does not exist in visit_occurrence
    visit_occurrence.visit_occurrence_id,

    provider.provider_id,

    medcode_intermediate._concept_id,

    medcode_intermediate._source_concept_id,

    medcode_intermediate._source_value,

    -- observation specific type codes
    CASE medcode_intermediate.source_table
      WHEN 'clinical' THEN 38000245 -- EHR problem list entry
      WHEN 'referral' THEN 42898140 -- Referral record
      WHEN 'test' THEN 38000280 -- Observation recorded from EHR
      WHEN 'immunisation' THEN 38000280 -- Observation recorded from EHR
      ELSE 0
    END AS observation_type_concept_id,

    -- Yes
    4188539 AS value_as_concept_id

  FROM public.medcode_intermediate AS medcode_intermediate
    LEFT JOIN cdm5.visit_occurrence USING (visit_occurrence_id)
    LEFT JOIN cdm5.provider ON medcode_intermediate.provider_id = provider.provider_id
  -- All 'others'
  WHERE target_domain_id NOT IN ('Condition','Device','Drug','Measurement', 'Procedure') OR target_domain_id IS NULL
;