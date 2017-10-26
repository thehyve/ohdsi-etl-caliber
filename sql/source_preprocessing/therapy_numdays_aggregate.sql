/*
Most common numdays per prodcode
 */
DROP TABLE IF EXISTS public.numdays_aggregate_prodcode;

WITH occurrence AS (
    SELECT
      prodcode,
      numdays,
      count(*) AS frequency
    FROM caliber.therapy
    WHERE numdays > 0 AND numdays < 365
    GROUP BY prodcode, numdays
), max_occurrence AS (
    SELECT
      prodcode,
      max(frequency) AS frequency
    FROM occurrence
    GROUP BY prodcode
), avg_max_occurrence AS (
  /*
  Rejoin occurrence to get numdays belonging to this prodcode and count
  If two numdays with the same frequency, choose minimum
   */
    SELECT
      prodcode,
      min(numdays)   AS numdays,
      -- include frequency, which should be the same
      avg(frequency) AS frequency
    FROM max_occurrence
      JOIN occurrence USING (prodcode, frequency)
    GROUP BY prodcode
)
SELECT *
INTO public.numdays_aggregate_prodcode
FROM avg_max_occurrence
ORDER BY prodcode;

/*
Most common numdays per unique combination of prodcode, ndd, qty and numpacks
Same procedure as above, but with additional stratification groups
 */
DROP TABLE IF EXISTS public.numdays_aggregate_full;

WITH occurrence AS (
    SELECT
      prodcode,
      ndd,
      qty,
      numpacks,
      numdays,
      count(*) AS frequency
    FROM caliber.therapy
    WHERE numdays > 0 AND numdays < 365
    GROUP BY prodcode, ndd, qty, numpacks, numdays
    HAVING count(*) > 1
), max_occurrence AS (
    SELECT
      prodcode,
      ndd,
      qty,
      numpacks,
      max(frequency) AS frequency
    FROM occurrence
    GROUP BY prodcode, ndd, qty, numpacks
), avg_max_occurrence AS (
    SELECT
      prodcode,
      ndd,
      qty,
      numpacks,
      min(numdays)   AS numdays,
      -- include frequency, which should be the same
      avg(frequency) AS frequency
    FROM max_occurrence
      JOIN occurrence USING (prodcode, ndd, qty, numpacks, frequency)
    GROUP BY prodcode, ndd, qty, numpacks
)
SELECT *
INTO public.numdays_aggregate_full
FROM avg_max_occurrence
ORDER BY prodcode, ndd, qty, numpacks;
