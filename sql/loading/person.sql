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
    p.patid            AS person_id,
    p.patid            AS person_source_value,
    p.pracid           AS care_site_id,

    CASE p.gender
      WHEN 1 THEN 8507 -- MALE
      WHEN 2 THEN 8532 -- FEMALE
      ELSE 0 -- Data not entered | Indeterminate | Unknown
    END                AS gender_concept_id,

    p.gender           AS gender_source_value,

    p.yob              AS year_of_birth,

    CASE WHEN p.mob = 0
      THEN NULL
    ELSE p.mob END
                       AS month_of_birth,

    CASE -- Sorted by prevalence
    WHEN hesp.gen_ethnicity = 'White'
      THEN 8527
    WHEN hesp.gen_ethnicity = 'Unknown'
      THEN 0 -- was 8552
    WHEN hesp.gen_ethnicity = 'Indian'
      THEN 38003574
    WHEN hesp.gen_ethnicity = 'Other'
      THEN 0 -- was 8522
    WHEN hesp.gen_ethnicity = 'Oth_Asian'
      THEN 8515
    WHEN hesp.gen_ethnicity = 'Pakistani'
      THEN 38003589
    WHEN hesp.gen_ethnicity = 'Bl_Afric'
      THEN 38003598
    WHEN hesp.gen_ethnicity = 'Mixed'
      THEN 0 -- was 8522
    WHEN hesp.gen_ethnicity = 'Bl_Carib'
      THEN 38003598
    WHEN hesp.gen_ethnicity = 'Bl_Other'
      THEN 38003598
    WHEN hesp.gen_ethnicity = 'Bangladesi'
      THEN 38003598
    WHEN hesp.gen_ethnicity = 'Chinese'
      THEN 38003579
    ELSE 0
    END                AS race_concept_id,

    hesp.gen_ethnicity AS race_source_value,

    0                  AS ethnicity_concept_id


  FROM caliber.patient p
    LEFT JOIN caliber.hes_patient hesp
      ON p.patid = hesp.patid
    LEFT JOIN (
                SELECT
                  p.patid,
                  greatest(p.crd, prac.uts) < least(p.tod, prac.lcd, opd.ons_pat_death) AS valid_obs_period

                FROM
                  caliber.patient p
                  LEFT JOIN caliber.practice prac
                    ON p.pracid = prac.pracid
                  LEFT JOIN caliber.ons_death od
                    ON p.patid = od.patid
                  LEFT JOIN (
                              SELECT
                                p.patid,
                                CASE
                                WHEN od.dod IS NOT NULL
                                  THEN od.dod
                                ELSE p.deathdate
                                END
                                  AS ons_pat_death
                              FROM
                                caliber.patient AS p,
                                caliber.ons_death AS od
                              WHERE p.patid = od.patid
                            ) opd
                    ON p.patid = opd.patid
              ) p_valid_period
      ON p.patid = p_valid_period.patid
  -- Do not insert patients with an accept value of 0
  WHERE p.accept = 1
        -- Exclude patients born before 1900
        AND p.yob >= 1900
        -- Exclude patients with observation end dates before observation start date
        AND valid_obs_period != FALSE;
