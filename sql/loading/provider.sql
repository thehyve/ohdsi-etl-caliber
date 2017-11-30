INSERT INTO cdm5.provider (
  provider_id,
  gender_concept_id,
  gender_source_value,
  specialty_concept_id,
  specialty_source_value
)
  SELECT
    DISTINCT ON (staff.staffid)
    staff.staffid                                       AS provider_id,

    CASE staff.gender
    WHEN 0 -- Data not entered
      THEN NULL
    WHEN 1
      THEN 8507 -- MALE
    WHEN 2
      THEN 8532 -- FEMALE
    ELSE 0 -- Indeterminate | Unknown
    END                                                 AS gender_concept_id,

    staff.gender                                        AS gender_source_value,

    coalesce(specialty_map.target_concept_id, 38004514) AS specialty_concept_id,

    staff.role :: VARCHAR                               AS specialty_source_value

  FROM @source_schema.staff AS staff
    LEFT JOIN cdm5.source_to_concept_map AS specialty_map
      ON staff.role = (specialty_map.source_code :: INT)
         AND specialty_map.source_vocabulary_id = 'JNJ_CPRD_PROV_SPEC'

  UNION -- ensure a constraint violation will occur in case of duplicates

  SELECT DISTINCT ON (hoc.tretspef)

    CASE tretspef
    WHEN '&amp;'
      THEN 2000000000
    ELSE 2000000000 + (tretspef :: INT)
    END
                                          AS provider_id,

    NULL                                  AS gender_concept_id,
    NULL                                  AS gender_source_value,
    coalesce(target_concept_id, 38004514) AS specialty_concept_id,
    tretspef                              AS specialty_source_value

  FROM @source_schema.hes_op_clinical AS hoc
    LEFT JOIN cdm5.source_to_concept_map AS cms2_map
      ON hoc.tretspef = cms2_map.source_code
         AND cms2_map.source_vocabulary_id = 'JNJ_CPRD_PROV_CMS2';
