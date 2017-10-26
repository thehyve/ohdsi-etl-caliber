-- Take all distinct region values from the practice table and insert them into location.location_id

INSERT INTO cdm5.location
(
  location_id,
  address_1,
  location_source_value
)
  SELECT DISTINCT
    practice.region AS location_id,

    CASE practice.region
    WHEN 1 THEN 'North East'
    WHEN 2 THEN 'North West'
    WHEN 3 THEN 'Yorkshire & The Humber'
    WHEN 4 THEN 'East Midlands'
    WHEN 5 THEN 'West Midlands'
    WHEN 6 THEN 'East of England'
    WHEN 7 THEN 'South West'
    WHEN 8 THEN 'South Central'
    WHEN 9 THEN 'London'
    WHEN 10 THEN 'South East Coast'
    WHEN 11 THEN 'Northern Ireland'
    WHEN 12 THEN 'Scotland'
    WHEN 13 THEN 'Wales'
    ELSE 'Unknown'
    END AS address_1,

    practice.region AS location_source_value

  FROM caliber.practice AS practice;