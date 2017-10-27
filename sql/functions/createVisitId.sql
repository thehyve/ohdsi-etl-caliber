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