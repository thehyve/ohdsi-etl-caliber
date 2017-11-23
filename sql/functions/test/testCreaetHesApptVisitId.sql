/*
Test creation of HES Appointment visit occurrence id
 */
WITH cases (expected, actual) AS (
  VALUES
     (503923813416656503, createHesApptVisitId('503923813416',6565030))
    ,(503923813416006196, createHesApptVisitId('503923813416',6196))
    ,(502981580400110971, createHesApptVisitId('502981580400',11097168))
    ,(502981580400743198, createHesApptVisitId('502981580400',743198))
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