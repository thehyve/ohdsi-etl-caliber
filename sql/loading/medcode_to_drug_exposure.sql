/*
Load from the medcode_intermediate: the union of the clinical, referral, test and immunisation tables.
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
    medcode_intermediate.person_id,

    medcode_intermediate._start_date,

    medcode_intermediate._start_datetime,

    -- start date == end date
    medcode_intermediate._start_date AS drug_exposure_end_date,

    -- Null if id does not exist in visit_occurrence
    visit_occurrence.visit_occurrence_id,

    medcode_intermediate.provider_id,

    medcode_intermediate._concept_id,

    medcode_intermediate._source_concept_id,

    medcode_intermediate._source_value,

    -- condition  specific codes
    CASE medcode_intermediate.source_table
      WHEN 'clinical' THEN 38000178 -- Medication list entry
      WHEN 'referral' THEN 38000178 -- Medication list entry
      WHEN 'test' THEN 38000178 -- Medication list entry
      WHEN 'immunisation' THEN 38000179 -- Physician administered drug (identified as procedure)
      ELSE 0
    END                              AS drug_type_concept_id

  FROM public.medcode_intermediate AS medcode_intermediate
    LEFT JOIN cdm5.visit_occurrence USING (visit_occurrence_id)
  -- If from immunisation table, the immunisation status has to be 'Given'
  WHERE target_domain_id = 'Drug' AND (
    medcode_intermediate.immunisation_status = 'Given' OR
    lower(medcode_intermediate.source_table) NOT LIKE 'immunisation'
  )
;
