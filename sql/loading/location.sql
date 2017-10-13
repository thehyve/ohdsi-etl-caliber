-- Take all distinct region values from the practice table and insert them into location.location_id

INSERT INTO cdm5.location
(
  location_id
)
  SELECT DISTINCT (region)
  FROM caliber.practice;