/*
Creates an end date from a start date and a duration in days.
End date is defined as `startDate + (durationDays - 1)`
A duration of 1 day returns the startDate.
*/
CREATE OR REPLACE FUNCTION createEndDate(startDate date, durationDays BIGINT)
  RETURNS date AS
$$
BEGIN
  RETURN startDate + (durationDays - 1) * INTERVAL '1 day';
END;
$$ LANGUAGE plpgsql;