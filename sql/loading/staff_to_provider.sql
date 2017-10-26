-- TRUNCATE TABLE cdm5.provider;

INSERT INTO cdm5.provider (
  provider_id,
  gender_concept_id,
  gender_source_value,
  specialty_concept_id,
  specialty_source_value
)
SELECT
  DISTINCT ON (staff.staffid)
  staff.staffid as provider_id,

  CASE staff.gender
    WHEN 1 THEN 8507 -- MALE
    WHEN 2 THEN 8532 -- FEMALE
    ELSE 0 -- Data not entered | Indeterminate | Unknown
  END as gender_concept_id,

  staff.gender as gender_source_value,

  coalesce(specialty_map.target_concept_id, 0) AS specialty_concept_id,

  staff.role AS specialty_source_value

FROM caliber.staff AS staff
LEFT JOIN cdm5.source_to_concept_map AS specialty_map
  ON staff.role = CAST(specialty_map.source_code AS INT)
  AND specialty_map.source_vocabulary_id = 'JNJ_CPRD_PROV_SPEC'
