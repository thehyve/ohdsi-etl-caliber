/*
Test creation of HES Appointment visit occurrence id
 */
WITH cases (expected, actual) AS (
  VALUES
     (503923813416656503, createHesApptVisitId('503923813416',6565030))
    ,(503923813416006196, createHesApptVisitId('503923813416',6196))
    ,(502981580400110971, createHesApptVisitId('502981580400',11097168))
    ,(502981580400743198, createHesApptVisitId('502981580400',743198))
    ,(309757565441178413, createHesApptVisitId('309757565441',17841337))
    -- clashes with the above because last two patid digits not used
    ,(309757565441178413, createHesApptVisitId('309757565441',17841389))
    ,(309757565458841389, createHesApptVisitId('309757565458',841389))
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