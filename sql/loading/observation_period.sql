/*
Join on patient.pracid = practice.pracid
and patient.patid = ons_death.patid
and linkage_coverage table
*/
INSERT INTO cdm5.observation_period
(
  person_id,
  observation_period_start_date,
  observation_period_end_date,
  period_type_concept_id
)
  SELECT

    p.patid                  AS person_id,

    -- [VALUE   COMMENT] Date the patient’s current period of registration with the practice began
    -- (date of the first ‘permanent’ record after the latest transferred out period).
    -- If there are no ‘transferred out periods’, the date is equal to ‘frd’
    -- [MAPPING   LOGIC] Greatest of Patient.crd and Practice.uts and capture start date of HES,
    -- ONS and CPRD from linkage_coverage table.
    -- [MAPPING COMMENT] Crd and uts are never null in the source data
    obs_dates.obs_start_date AS observation_period_start_date,

    -- [VALUE   COMMENT] Date the patient transferred out of the practice, if relevant. Empty for patients who
    -- have not transferred out
    -- [MAPPING   LOGIC] Least of Patient.todate, Practice.lcd, Patient.deathdate,  and ons_death.dod
    -- capture end date of HES, ONS and CPRD from Linkage_coverage table.
    -- [MAPPING COMMENT] Deathdate and/or todate can be null in the data
    obs_dates.obs_end_date   AS observation_period_end_date,

    44814725                 AS period_type_concept_id -- Period inferred by algorithm

  FROM caliber.patient p
    LEFT JOIN caliber.practice prac ON p.pracid = prac.pracid
    LEFT JOIN caliber.ons_death od ON p.patid = od.patid
    -- LEFT JOIN caliber.linkage_coverage
    LEFT JOIN (
                SELECT
                  p.patid,
                  greatest(p.crd, prac.uts)                                             AS obs_start_date,
                  least(p.tod, prac.lcd, opd.ons_pat_death)                             AS obs_end_date,
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
                                COALESCE(od.dod, p.deathdate) AS ons_pat_death
                              FROM
                                caliber.patient AS p,
                                caliber.ons_death AS od
                              WHERE p.patid = od.patid
                            ) opd
                    ON p.patid = opd.patid
              ) obs_dates
      ON p.patid = obs_dates.patid
  WHERE obs_dates.valid_obs_period IS NOT FALSE;
