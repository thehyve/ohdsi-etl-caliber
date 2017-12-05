/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
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
    medcode_intermediate.person_id,

    medcode_intermediate._start_date,

    medcode_intermediate._start_datetime,

    -- Null if id does not exist in visit_occurrence
    visit_occurrence.visit_occurrence_id,

    medcode_intermediate.provider_id,

    medcode_intermediate._concept_id,

    medcode_intermediate._source_concept_id,

    medcode_intermediate._source_value,

    -- 'EHR Detail'
    44818707 AS device_type_concept_id

  FROM public.medcode_intermediate AS medcode_intermediate
    LEFT JOIN cdm5.visit_occurrence USING (visit_occurrence_id)
  WHERE target_domain_id = 'Device'
;
