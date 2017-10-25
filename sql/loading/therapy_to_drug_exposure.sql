-- Note: this query is slow.. What is the main cause? Cast source code to int? Existing indices?
INSERT INTO cdm5.drug_exposure
(
	person_id,
	drug_exposure_start_date,
  drug_exposure_start_datetime,
	drug_exposure_end_date,
	days_supply,
	drug_concept_id,
	drug_source_value,
	provider_id,
	drug_type_concept_id,
	quantity,
	sig,
	refills,
	visit_occurrence_id
)
SELECT
	therapy.patid AS person_id,

	therapy.eventdate AS drug_exposure_start_date,
	therapy.eventdate AS drug_exposure_start_datetime, -- TODO: what is the convention for the time?

 	-- [MAPPING   LOGIC] if 0<numdays<365: numdays = numdays as is else if imputed: numdays = most common numdays for this product/ndd/qty/numpack else:       numdays = 1  eventdate + numdays -1
  -- TODO: implement most common numdays logic. Preprocess in separate table and write sql-procedure?
	CASE
		WHEN (therapy.numdays > 0 AND therapy.numdays < 365)
			THEN therapy.eventdate + (therapy.numdays - 1) * INTERVAL '1 day'
		ELSE therapy.eventdate
	END AS drug_exposure_end_date,

	CASE
		WHEN (therapy.numdays = 0 OR therapy.numdays > 365) THEN NULL
		ELSE therapy.numdays
	END AS days_supply,

	COALESCE(target_concept_id,0) AS drug_concept_id,

	therapy.prodcode AS drug_source_value,

  -- Identifier of the practice staff member entering the data. A value of 0 indicates that the staffid is unknown
	therapy.staffid AS provider_id,

	38000177 AS drug_type_concept_id, -- 'Prescription written'

  -- Total quantity is quantity per pack times number of packs
	therapy.qty * therapy.numpacks AS quantity,

  -- Numeric daily dose prescribed for the event. Derived using a CPRD algorithm on common dosage strings (represented by textid < 100,000). Value is set to 0 for all dosage strings represented by a non-numeric textid
	therapy.ndd	AS	sig,

  -- Number to indicate whether the event is associated with a repeat schedule. Value of 0 implies the event is not part of a repeat prescription. A value >= 1 denotes the issue number for the prescription within a repeat schedule
	therapy.issueseq	AS	refills,

  -- Visit id only assigned If record date for this patient exists in visit_occurrence table
  CASE
    WHEN createvisitid(therapy.patid, therapy.eventdate) IN (SELECT visit_occurrence_id FROM cdm5.visit_occurrence)
    THEN createvisitid(therapy.patid, therapy.eventdate)
    ELSE NULL
  END AS visit_occurrence_id

FROM caliber.therapy AS therapy
	LEFT JOIN cdm5.source_to_concept_map AS product_map
		ON therapy.prodcode = CAST(product_map.source_code AS INT)
	  AND product_map.source_vocabulary_id = 'CPRD_PRODUCT'
	LEFT JOIN cdm5.concept AS concept
		ON product_map.target_concept_id = concept.concept_id
WHERE
	therapy.eventdate IS NOT NULL
	AND therapy.prodcode > 1 -- 1 is an invalid prodcode
	AND (concept.domain_id = 'Drug' OR concept.concept_id = 0 ) -- Note: also include unmapped concepts here, assuming most are drugs
;