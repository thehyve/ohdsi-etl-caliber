/*
Creates an end date from a start date and a duration in days.
End date is defined as `startDate + (durationDays - 1)`
A duration of 1 day returns the startDate.
*/
CREATE OR REPLACE FUNCTION createEndDate(startDate date, durationDays int)
  RETURNS date AS
$$
BEGIN
  RETURN startDate + (durationDays - 1) * INTERVAL '1 day';
END;
$$ LANGUAGE plpgsql;

/*
Unit tests
 */
WITH cases (expected, actual) AS (
  VALUES
    (to_date('20120409', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 1))
    ,(to_date('20120410', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 2))
    ,(to_date('20120418', 'yyyymmdd'), createEndDate(to_date('20120409', 'yyyymmdd'), 10))
    ,(to_date('20160407', 'yyyymmdd'), createEndDate(to_date('20150409', 'yyyymmdd'), 365))
)
SELECT
  CASE
  WHEN expected = actual THEN 'passed'
  WHEN expected ISNULL AND actual ISNULL THEN 'passed'
  ELSE 'FAILED'
  END,
  expected,
  actual
FROM cases
;