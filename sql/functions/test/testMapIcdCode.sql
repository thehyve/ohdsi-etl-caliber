/*
Test mapIcdCode
 */
WITH cases (expected, actual) AS (
  VALUES
    (45601158, mapicdcode('K25.5x')), -- full ICD10 map
    (44833450, mapicdcode('349x')), -- full ICD9 map
    (45756398, mapicdcode('X42.0x')), -- Maps to X42
    (42616305, mapicdcode('M00.0000000')), -- Maps to M00.00
    (NULL, mapicdcode('QWERTY')), -- No map
    (NULL, mapicdcode('')), -- No map
    (NULL, mapicdcode(NULL)) -- No map
)
SELECT
  CASE
  WHEN expected :: INT = actual
    THEN 'passed'
  WHEN expected ISNULL AND actual ISNULL
    THEN 'passed'
  ELSE 'FAILED'
  END,
  expected,
  actual
FROM cases;
