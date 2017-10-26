/*
Creates visit identifier from a patient id and visit date.
Result is the concatenation of patientId, year, month and day.
*/
CREATE OR REPLACE FUNCTION createVisitId(patientId bigint, visitDate date)
  RETURNS bigint AS
$$
BEGIN
  RETURN CAST(patientId AS BIGINT) * 100000000
         + EXTRACT(YEAR FROM visitDate) * 10000
         + EXTRACT(MONTH FROM visitDate) * 100
         + EXTRACT(DAY FROM visitDate);
END;
$$ LANGUAGE plpgsql;

/*
Unit tests
 */
WITH cases (expected, actual) AS (
  VALUES
     (100567920120409, createVisitId(1005679, to_date('20120409', 'yyyymmdd')))
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