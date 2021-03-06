
INSERT INTO cdm5.death
(
  person_id,
  death_date,
  death_type_concept_id,
  cause_concept_id,
  cause_source_value,
  cause_source_concept_id
)

  WITH best_icd_match AS (
      SELECT DISTINCT ON (ons_death.patid)
        ons_death.patid,
        coalesce(icd10.concept_id, icd9cm.concept_id, mapicdcode(ons_death.cause)) AS icd_match
      FROM @source_schema.ons_death ons_death
        LEFT JOIN cdm5.concept icd9cm
          ON ons_death.cause = icd9cm.concept_code AND icd9cm.vocabulary_id = 'ICD9CM'
        LEFT JOIN cdm5.concept icd10
          ON ons_death.cause = icd10.concept_code AND icd10.vocabulary_id = 'ICD10'
  )

  SELECT DISTINCT ON (patient.patid)

    patient.patid                              AS person_id,
    obs_period_validity.death_date              AS death_date,

    -- EHR record
    38003569                                   AS death_type_concept_id,

    snomed_code.concept_id_2                   AS cause_concept_id,
    ons_death.cause                            AS cause_source_value,
    icd_concept_code.icd_match                 AS cause_source_concept_id

  FROM @source_schema.patient AS patient
    LEFT JOIN @source_schema.ons_death AS ons_death
      ON patient.patid = ons_death.patid
    LEFT JOIN public.obs_period_validity AS obs_period_validity
      ON patient.patid = obs_period_validity.patid
    LEFT JOIN best_icd_match AS icd_concept_code
      ON patient.patid = icd_concept_code.patid
    LEFT JOIN cdm5.concept_relationship AS snomed_code
      ON icd_concept_code.icd_match = snomed_code.concept_id_1 AND
         snomed_code.relationship_id = 'Maps to' AND
         snomed_code.invalid_reason IS NULL
  WHERE obs_period_validity.valid_obs_period IS NOT FALSE -- also allow NULL
        AND obs_period_validity.death_date IS NOT NULL;
