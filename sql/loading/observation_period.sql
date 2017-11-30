INSERT INTO cdm5.observation_period
(
  person_id,
  observation_period_start_date,
  observation_period_start_datetime,
  observation_period_end_date,
  observation_period_end_datetime,
  period_type_concept_id
)
  SELECT DISTINCT ON (pat.patid)

    pat.patid                                       AS person_id,
    obs_validity.obs_period_start_date              AS observation_period_start_date,
    obs_validity.obs_period_start_date :: TIMESTAMP AS observation_period_start_datetime,
    obs_validity.obs_period_end_date                AS observation_period_end_date,
    obs_validity.obs_period_end_date :: TIMESTAMP   AS observation_period_end_datetime,

    -- Period inferred by algorithm
    44814725                                        AS period_type_concept_id

  FROM @source_schema.patient AS pat
    LEFT JOIN @source_schema.practice AS prac
      ON pat.pracid = prac.pracid
    LEFT JOIN @source_schema.ons_death AS od
      ON pat.patid = od.patid
    LEFT JOIN public.obs_period_validity AS obs_validity
      ON pat.patid = obs_validity.patid
  WHERE obs_validity.valid_obs_period IS NOT FALSE;
