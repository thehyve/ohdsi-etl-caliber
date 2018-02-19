/*
Test createVisitId
 */
WITH cases (expected, actual) AS (
  VALUES
     (100567920120409, createVisitId(1005679, to_date('20120409', 'yyyymmdd')))
    ,(3264838220120409, createVisitId(32648382, to_date('20120409', 'yyyymmdd')))
    ,(1920120409, createVisitId(19, to_date('20120409', 'yyyymmdd')))
    ,(44340920070101, createVisitId(443409, to_date('20070101', 'yyyymmdd')))
    ,(920000229, createVisitId(9, to_date('20000229', 'yyyymmdd')))
    ,(NULL, createVisitId(9, to_date(NULL, 'yyyymmdd')))
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