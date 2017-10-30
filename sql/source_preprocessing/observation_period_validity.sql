DROP TABLE IF EXISTS caliber.obs_period_validity;

WITH death AS
(SELECT DISTINCT ON (pat.patid, pat_ons.patid)
   pat_ons.patid,
   COALESCE(pat_ons.dod, pat.deathdate) AS ons_pat_death
 FROM
   caliber.patient AS pat
   FULL OUTER JOIN
   caliber.ons_death AS pat_ons
     ON pat.patid = pat_ons.patid
),

    obs_period_dates AS
  (
      SELECT DISTINCT ON (p.patid, prac.pracid)
        p.patid,
        greatest(p.crd, prac.uts)                   AS obs_period_start_date,
        least(p.tod, prac.lcd, death.ons_pat_death) AS obs_period_end_date

      FROM
        caliber.patient p
        LEFT JOIN caliber.practice prac
          ON p.pracid = prac.pracid
        LEFT JOIN death
          ON p.patid = death.patid
  )

SELECT
  patid,
  obs_period_start_date,
  obs_period_end_date,
  obs_period_start_date < obs_period_end_date AS valid_obs_period
INTO caliber.obs_period_validity
FROM obs_period_dates;
