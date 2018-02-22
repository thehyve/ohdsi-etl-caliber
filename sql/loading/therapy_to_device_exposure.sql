/*
Only rows where product code maps to a device
*/
INSERT INTO cdm5.device_exposure
(
	person_id,
	device_exposure_start_date,
  device_exposure_start_datetime,
	device_concept_id,
  device_source_value,
  device_type_concept_id,
  provider_id,
  visit_occurrence_id
)
SELECT
	therapy.patid AS person_id,

	therapy.eventdate AS device_exposure_start_date,

	therapy.eventdate :: TIMESTAMP AS device_exposure_start_datetime,

	product_map.target_concept_id AS device_concept_id,

  therapy.prodcode AS device_source_value,

  -- EHR Detail
	44818707 AS device_type_concept_id,

  -- Identifier of the practice staff member entering the data. A value of 0 indicates that the staffid is unknown
	therapy.staffid AS provider_id,

  -- Visit id only assigned If record date for this patient exists in visit_occurrence table
	createvisitid(therapy.patid, therapy.eventdate)
--   CASE
--     WHEN createvisitid(therapy.patid, therapy.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
--     THEN createvisitid(therapy.patid, therapy.eventdate)
--     ELSE NULL
--   END AS visit_occurrence_id

FROM @source_schema.therapy AS therapy
	LEFT JOIN cdm5.source_to_target AS product_map
		ON therapy.prodcode :: TEXT = product_map.source_code
			 AND product_map.source_vocabulary_id = 'CPRD_PRODUCT'
WHERE
	therapy.eventdate IS NOT NULL
	AND therapy.prodcode > 1 -- 1 is an invalid prodcode
	AND product_map.target_domain_id = 'Device'
;