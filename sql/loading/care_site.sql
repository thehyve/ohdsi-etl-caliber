INSERT INTO cdm5.care_site
(
  care_site_id,
  care_site_source_value,
  location_id
  --care_site_name,
  --place_of_service_concept_id,
  --place_of_service_source_value
)
  SELECT
    practice.pracid AS care_site_id,
    practice.pracid AS care_site_source_value,

    --Value to indicate where in the UK the practice is based.
    -- The region denotes the Strategic Health Authority for practices within England,
    -- and the country i.e. Wales, Scotland, or Northern Ireland for the rest
    practice.region AS location_id

  FROM caliber.practice;