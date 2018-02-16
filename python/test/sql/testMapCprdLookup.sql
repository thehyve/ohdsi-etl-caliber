/*
Test mapCprdLookup
 */
WITH cases (expected, actual) AS (
  VALUES
     (4069590, mapCprdLookup('Normal'))
    ,(4082948, mapCprdLookup('A+'))
    ,(4328749, mapCprdLookup('High'))
    ,(0, mapCprdLookup('not a match'))
    ,(0, mapCprdLookup(''))
    ,(NULL, mapCprdLookup(NULL))
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