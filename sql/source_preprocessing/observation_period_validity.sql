DROP TABLE IF EXISTS public.obs_period_validity;

WITH death AS (
    SELECT DISTINCT ON (pat.patid, pat_ons.patid)
      pat_ons.patid,
      COALESCE(pat_ons.dod, pat.deathdate) AS ons_pat_death
    FROM @source_schema.patient AS pat
      FULL OUTER JOIN @source_schema.ons_death AS pat_ons
        ON pat.patid = pat_ons.patid
), obs_period_dates AS (
    SELECT DISTINCT ON (patient.patid, prac.pracid)
      patient.patid,
      greatest(patient.crd, prac.uts)                   AS obs_period_start_date,
      death.ons_pat_death                               AS death_date,
      least(patient.tod, prac.lcd, death.ons_pat_death) AS obs_period_end_date

    FROM @source_schema.patient AS patient
      LEFT JOIN @source_schema.practice AS prac
        ON createCareSiteId(patient.patid) = prac.pracid
      LEFT JOIN death AS death
        ON patient.patid = death.patid
)

SELECT
  patid,

  obs_period_start_date,

  -- End date with fallback on 2016-03-08 TODO: set as variable
  coalesce(obs_period_end_date, to_date('20160308', 'yyyymmdd')) AS obs_period_end_date,

  obs_period_start_date < obs_period_end_date                    AS valid_obs_period,
  death_date                                                     AS death_date

INTO public.obs_period_validity
FROM obs_period_dates
WHERE obs_period_start_date IS NOT NULL AND patid IS NOT NULL;
