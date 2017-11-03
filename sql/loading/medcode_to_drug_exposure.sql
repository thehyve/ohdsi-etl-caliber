/*
Load from the medcode_merge: the union of the clinical, referral, test and immunisation tables.
Only include rows with a concept that maps to the Drug domain or where the source maps to the Drug domain.
If record from the immunisation table, then the immunisation status should be 'given'.
*/

INSERT INTO cdm5.drug_exposure
(
  person_id,
  drug_exposure_start_date,
  drug_exposure_start_datetime,
  drug_exposure_end_date,
  visit_occurrence_id,
  provider_id,
  drug_concept_id,
  drug_source_concept_id,
  drug_source_value,
  drug_type_concept_id
)
  SELECT
    medcode_merge.person_id,

    medcode_merge._start_date,

    medcode_merge._start_datetime,

    -- start date == end date
    medcode_merge._start_date AS drug_exposure_end_date,

    medcode_merge.visit_occurrence_id,

    medcode_merge.provider_id,

    medcode_merge._concept_id,

    medcode_merge._source_concept_id,

    medcode_merge._source_value,

    -- condition  specific codes
    CASE medcode_merge.source_table
      WHEN 'clinical' THEN 38000178 -- Medication list entry
      WHEN 'referral' THEN 38000178 -- Medication list entry
      WHEN 'test' THEN 38000178 -- Medication list entry
      WHEN 'immunisation' THEN 38000179 -- Physician administered drug (identified as procedure)
      ELSE 0
    END AS drug_type_concept_id

  FROM medcode_merge AS medcode_merge
  WHERE (
    target_domain_id = 'Drug'
    OR (target_domain_id ISNULL AND source_domain_id = 'Drug')
  ) AND (
    medcode_merge.immunisation_status = 'Given'
    OR lower(medcode_merge.source_table) NOT LIKE 'immunisation'
  )
;
