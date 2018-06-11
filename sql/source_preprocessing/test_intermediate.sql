/*
Create intermediate test table, to determine which fields need mapping.
Filter records with qualifier 0 ('Data not entered') for 4 data fields and without a value for 7 or 8 data fields.
In general, if the lookup code is '0' for the qualifier, operator or unit, then leave the cell empty (NULL).
 */
DROP TABLE IF EXISTS public.test_intermediate;

WITH test_with_entity AS (
  SELECT
    test.*,
    entity.data_fields::INTEGER AS data_fields
  FROM @source_schema.test AS test
    JOIN @source_schema.entity AS entity USING (enttype)
), test_intermediate AS (
  /* Test entities with 4 data fields */
  SELECT
    patid,
    eventdate,
    staffid,
    enttype :: TEXT  AS enttype_string,
    NULL             AS value,
    data1            AS qualifier_tqu,
    data2 :: NUMERIC AS range_from,
    data3 :: NUMERIC AS range_to,
    NULL             AS operator_opr,
    NULL             AS unit_sum,
    NULL             AS alternative_source_value
  FROM test_with_entity
  WHERE data_fields = 4 AND data1 != '0'

  UNION ALL

  /* Test entities with 7 or 8 data fields */
  SELECT
    patid,
    eventdate,
    staffid,
    enttype :: TEXT  AS enttype_string,
    data2 :: NUMERIC AS value,
    data4            AS qualifier_tqu,
    data5 :: NUMERIC AS range_from,
    data6 :: NUMERIC AS range_to,
    data1            AS operator_opr,
    data3            AS unit_sum,
    NULL             AS alternative_source_value
  FROM test_with_entity
  WHERE data_fields > 4

  UNION ALL

  /* Additional row for 7th data column for entity 311 */
  SELECT
    patid,
    eventdate,
    staffid,
    enttype :: TEXT || '-7',
    NULL       AS value,
    NULL       AS qualifier_tqu,
    NULL       AS range_from,
    NULL       AS range_to,
    NULL       AS operator_opr,
    NULL       AS unit_sum,
    data7 AS alternative_source_value
  FROM test_with_entity
  WHERE enttype IN (311) AND data7 IS NOT NULL AND data7 != '0'

  UNION ALL

  /* Additional row for 8th data column for entities 154 and 284 */
  SELECT
    patid,
    eventdate,
    staffid,
    enttype :: TEXT || '-8',
    NULL       AS value,
    NULL       AS qualifier_tqu,
    NULL       AS range_from,
    NULL       AS range_to,
    NULL       AS operator_opr,
    NULL       AS unit_sum,
    data8      AS alternative_source_value
  FROM test_with_entity
  WHERE enttype IN (154, 284) AND data_fields = 8 AND data8 IS NOT NULL
)
SELECT
  test_intermediate.patid                    AS patid,
  test_intermediate.eventdate                AS eventdate,
  test_intermediate.staffid                  AS staffid,
  test_intermediate.enttype_string           AS enttype_string,
  enttype_map.target_concept_id              AS _concept_id,
  test_intermediate.value                    AS value,
  test_intermediate.range_from               AS range_from,
  test_intermediate.range_to                 AS range_to,
  NULLIF(test_intermediate.qualifier_tqu,'0')AS qualifier_source_value,
  tqu_map.target_concept_id                  AS qualifier_concept_id,
  NULLIF(test_intermediate.operator_opr,'0') AS operator_source_value,
  opr_map.target_concept_id                  AS operator_concept_id,
  NULLIF(test_intermediate.unit_sum,'0')     AS unit_source_value,
  unit_map.target_concept_id                 AS unit_concept_id,
  test_intermediate.alternative_source_value AS alternative_source_value,
  enttype_map.target_domain_id               AS target_domain_id
INTO public.test_intermediate
FROM test_intermediate
  LEFT JOIN cdm5.source_to_target AS enttype_map
    ON enttype_map.source_code = test_intermediate.enttype_string AND
       enttype_map.source_vocabulary_id = 'JNJ_CPRD_T_ET_LOINC'
  LEFT JOIN cdm5.source_to_target AS tqu_map
    ON tqu_map.source_code = test_intermediate.qualifier_tqu AND
       tqu_map.source_vocabulary_id = 'CPRD_QUALIFIER'
  LEFT JOIN cdm5.source_to_target AS opr_map
    ON opr_map.source_code = test_intermediate.operator_opr AND
       opr_map.source_vocabulary_id = 'CPRD_QUALIFIER'
  LEFT JOIN cdm5.source_to_target AS unit_map
    ON unit_map.source_code = test_intermediate.unit_sum AND
       unit_map.source_vocabulary_id = 'CPRD_UNIT'
WHERE test_intermediate.eventdate IS NOT NULL
;