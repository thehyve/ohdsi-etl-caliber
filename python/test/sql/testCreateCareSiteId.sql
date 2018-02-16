/*
Test createCareSiteId
 */
WITH cases (expected, actual) AS (
  VALUES
     (1, createCareSiteId(1001))
    ,(20, createCareSiteId(2020))
    ,(19, createCareSiteId(250019))
    ,(15, createCareSiteId(242015))
    ,(456, createCareSiteId(123456))
    ,(456, createCareSiteId(CAST(123456 AS BIGINT)))
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