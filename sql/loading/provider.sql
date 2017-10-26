SELECT
  staff.staffid,
  staff.gender,
  prov.source_code,
  prov.source_code_description,
  prov.target_concept_id,
  prov.target_vocabulary_id
FROM caliber.staff AS staff
  LEFT JOIN (
              SELECT
                source_code,
                source_code_description,
                target_concept_id,
                target_vocabulary_id
              FROM cdm5.source_to_concept_map
              WHERE source_vocabulary_id = 'JNJ_CPRD_PROV_SPEC'
            ) prov ON prov.source_code :: INT = staff.role;



SELECT tretspef FROM caliber.hes_op_clinical
LEFT JOIN (
    SELECT * FROM cdm5.source_to_concept_map WHERE source_vocabulary_id = 'JNJ_CPRD_PROV_CMS2'
    )



          --tretspef='&amp;' set specialty_concept_id=38004514

