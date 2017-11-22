/*
Creates HES Appointment visit identifier from attendkey and patient id
Result is a integer of length 18, consisting of attendkey with (part of) patientId concatenated.
NOTE: This id is not guaranteed to be unique for each attendkey/patientid combination.
However, incorporating the whole patientId is not possible (bigint max 19 places) and this is very unlikely to give clashes.
*/
CREATE OR REPLACE FUNCTION createHesApptVisitId(attendkey TEXT, patientId INTEGER)
  RETURNS BIGINT AS
$$
BEGIN
  RETURN CAST(rpad(attendkey, 18, patientId :: TEXT) AS BIGINT);
END;
$$ LANGUAGE plpgsql;