/*
Creates visit identifier for the consultation table from a patient id and visit date.
Result is the concatenation of patientId, year, month and day.
The resulting number is always larger than 1e8
*/
CREATE OR REPLACE FUNCTION createVisitId(patientId BIGINT, visitDate date)
  RETURNS BIGINT AS
$$
BEGIN
  RETURN CAST(patientId AS BIGINT) * 100000000
         + EXTRACT(YEAR FROM visitDate) * 10000
         + EXTRACT(MONTH FROM visitDate) * 100
         + EXTRACT(DAY FROM visitDate);
END;
$$ LANGUAGE plpgsql;