-- Take all distinct region values from the practice table and insert them into location.location_id

INSERT INTO cdm5.location
(
  location_id,
  address_1,
  location_source_value
)
  SELECT DISTINCT
    (region),

    CASE
    WHEN prac.region = 1
      THEN 'North East'
    WHEN prac.region = 2
      THEN 'North West'
    WHEN prac.region = 3
      THEN 'Yorkshire & The Humber'
    WHEN prac.region = 4
      THEN 'East Midlands'
    WHEN prac.region = 5
      THEN 'West Midlands'
    WHEN prac.region = 6
      THEN 'East of England'
    WHEN prac.region = 7
      THEN 'South West'
    WHEN prac.region = 8
      THEN 'South Central'
    WHEN prac.region = 9
      THEN 'London'
    WHEN prac.region = 10
      THEN 'South East Coast'
    WHEN prac.region = 11
      THEN 'Northern Ireland'
    WHEN prac.region = 12
      THEN 'Scotland'
    WHEN prac.region = 13
      THEN 'Wales'
    END
      AS address_1,

    region AS location_source_value

  FROM caliber.practice prac;