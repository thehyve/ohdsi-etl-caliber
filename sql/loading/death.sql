INSERT INTO cdm5.death
(
  person_id,
  death_date,
  death_type_concept_id,
  cause_concept_id,
  cause_source_value
)
  SELECT DISTINCT ON (patient.patid)

    patient.patid                              AS person_id,
    coalesce(ons_death.dod, patient.deathdate) AS death_date,
    38003569                                   AS death_type_concept_id, -- EHR record
    concept.concept_id                         AS cause_concept_id,
    ons_death.cause                            AS cause_source_value

  FROM caliber.patient patient
    LEFT JOIN caliber.ons_death ons_death
      ON patient.patid = ons_death.patid
    LEFT JOIN caliber.obs_period_validity obs_period_validity
      ON patient.patid = obs_period_validity.patid
    LEFT JOIN cdm5.concept concept
      ON ons_death.cause = concept.concept_code AND vocabulary_id = 'ICD10CM'
  WHERE obs_period_validity.valid_obs_period IS NOT FALSE
        AND (ons_death.dod IS NOT NULL OR patient.deathdate IS NOT NULL);