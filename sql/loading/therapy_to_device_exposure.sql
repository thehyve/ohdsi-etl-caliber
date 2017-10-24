/*
Only rows where gemscript maps to a device
*/
INSERT INTO cdm5.device_exposure
(
	person_id,
	device_exposure_start_date,
  device_exposure_start_datetime,
	device_concept_id,
	device_type_concept_id,
  provider_id,
	visit_occurrence_id,
	device_source_value
)
SELECT
	therapy.patid AS person_id,

	therapy.eventdate AS device_exposure_start_date,
	therapy.eventdate AS device_exposure_start_datetime,

	product_map.target_concept_id AS device_concept_id,

	44818707 AS device_type_concept_id, -- EHR Detail

 -- Identifier of the practice staff member entering the data. A value of 0 indicates that the staffid is unknown
	therapy.staffid AS provider_id,

 -- If record date for this patient exists in visit_occurrence:     Patid * 10000000000 + (year(eventdate)*10000) +  (month(eventdate)* 100) + day(eventdate) else:     Null
	createVisitId(therapy.patid, therapy.eventdate) AS visit_occurrence_id,

 -- [VALUE   COMMENT] CPRD unique code for the treatment selected by the GP
	therapy.prodcode AS device_source_value

FROM caliber.therapy AS therapy
	LEFT JOIN cdm5.source_to_concept_map AS product_map
		ON therapy.prodcode = CAST(product_map.source_code AS INT)
			 AND product_map.source_vocabulary_id = 'CPRD_PRODUCT'
	LEFT JOIN cdm5.concept AS concept
		ON product_map.target_concept_id = concept.concept_id
WHERE
	therapy.eventdate IS NOT NULL
	AND therapy.prodcode > 1 -- 1 is an invalid prodcode
	AND concept.domain_id = 'Device'
  AND createvisitid(therapy.patid, therapy.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
;