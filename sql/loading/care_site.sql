INSERT INTO cdm5.care_site
(
  care_site_id,
  care_site_source_value,
  location_id
)
  SELECT DISTINCT ON (practice.pracid)
    practice.pracid AS care_site_id,
    practice.pracid AS care_site_source_value,
    practice.region AS location_id

  FROM @source_schema.practice;