-- Script to fill the person table with data from the patient and hes_patient CALIBER tables

INSERT INTO cdm5.person
(
  person_id,
  person_source_value,
  care_site_id,
  gender_concept_id,
  gender_source_value,
  year_of_birth,
  month_of_birth,
  race_concept_id,
  race_source_value,
  ethnicity_concept_id
)
  SELECT
    DISTINCT ON (p.patid)
    p.patid                   AS person_id,
    p.patid                   AS person_source_value,
    createCareSiteId(p.patid) AS care_site_id,

    CASE p.gender
    WHEN 1
      THEN 8507 -- MALE
    WHEN 2
      THEN 8532 -- FEMALE
    ELSE 0 -- Data not entered | Indeterminate | Unknown
    END                AS gender_concept_id,

    p.gender           AS gender_source_value,

    -- Accept two yob formats; as years after 1800 or actual year.
    CASE WHEN p.yob < 300
      THEN p.yob + 1800
      ELSE p.yob
    END                AS year_of_birth,

    CASE WHEN p.mob = 0
      THEN NULL
    ELSE p.mob END     AS month_of_birth,

    CASE hesp.gen_ethnicity -- Sorted by prevalence
    WHEN 'White'
      THEN 8527
    WHEN 'Unknown'
      THEN 0 -- was 8552
    WHEN 'Indian'
      THEN 38003574
    WHEN 'Other'
      THEN 0 -- was 8522
    WHEN 'Oth_Asian'
      THEN 8515
    WHEN 'Pakistani'
      THEN 38003589
    WHEN 'Bl_Afric'
      THEN 38003598
    WHEN 'Mixed'
      THEN 0 -- was 8522
    WHEN 'Bl_Carib'
      THEN 38003598
    WHEN 'Bl_Other'
      THEN 38003598
    WHEN 'Bangladesi'
      THEN 38003598
    WHEN 'Chinese'
      THEN 38003579
    ELSE 0
    END                AS race_concept_id,

    hesp.gen_ethnicity AS race_source_value,

    0                  AS ethnicity_concept_id


  FROM @source_schema.patient p
    LEFT JOIN @source_schema.hes_patient hesp
      ON p.patid = hesp.patid
    LEFT JOIN public.obs_period_validity obs_validity
      ON p.patid = obs_validity.patid
  -- Do not insert patients with an accept value of 0
  WHERE p.accept = 1
        -- Exclude patients born before 1850 or born after 2100 (for both yob formats)
        AND (p.yob BETWEEN 1850 AND 2100 OR p.yob BETWEEN 50 AND 300)
        -- Exclude patients with observation end dates before observation start date
        AND obs_validity.valid_obs_period IS NOT FALSE
;
