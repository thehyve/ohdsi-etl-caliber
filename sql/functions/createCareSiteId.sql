/*
Gets the practice id from the person id
*/
CREATE OR REPLACE FUNCTION createCareSiteId(patid INTEGER)
  RETURNS INTEGER AS
$$
BEGIN
  RETURN right(patid::text,3) :: INTEGER;
END;
$$ LANGUAGE plpgsql;