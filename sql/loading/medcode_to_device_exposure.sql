/*
Load from the medcode_merge: the union of the clinical, referral, test and immunisation tables.
Only include rows with a concept that maps to the Device domain or where the source maps to the Device domain.
*/

INSERT INTO cdm5.device_exposure
(
  person_id,
  device_exposure_start_date,
  device_exposure_start_datetime,
  visit_occurrence_id,
  provider_id,
  device_concept_id,
  device_source_concept_id,
  device_source_value,
  device_type_concept_id
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

    -- 'EHR Detail'
    44818707 AS device_type_concept_id

  FROM medcode_merge AS medcode_merge
  WHERE target_domain_id = 'Device' OR (target_domain_id ISNULL AND source_domain_id = 'Device')
;
