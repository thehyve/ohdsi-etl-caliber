/*
Test createEndDate
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