DROP TABLE IF EXISTS public.obs_period_validity;

WITH death AS (
    SELECT DISTINCT ON (pat.patid, pat_ons.patid)
      pat_ons.patid,
      COALESCE(pat_ons.dod, pat.deathdate) AS ons_pat_death
    FROM @source_schema.patient AS pat
      FULL OUTER JOIN @source_schema.ons_death AS pat_ons
        ON pat.patid = pat_ons.patid
), obs_period_dates AS (
    SELECT DISTINCT ON (p.patid, prac.pracid)
      p.patid,
      greatest(p.crd, prac.uts)                   AS obs_period_start_date,
      least(p.tod, prac.lcd, death.ons_pat_death) AS obs_period_end_date

    FROM @source_schema.patient AS p
      LEFT JOIN @source_schema.practice AS prac
        ON createCareSiteId(p.patid) = prac.pracid
      LEFT JOIN death AS death
        ON p.patid = death.patid
)

SELECT
  patid,
  obs_period_start_date,
  coalesce(obs_period_end_date, to_date('20160308', 'yyyymmdd')) AS obs_period_end_date, -- Fallback date
  obs_period_start_date < obs_period_end_date AS valid_obs_period
INTO public.obs_period_validity
FROM obs_period_dates;
