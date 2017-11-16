/*
Create intermediate test table, to determine which fields need mapping.
Also filters records with qualifier 0 ('Data not entered') and without a value.
TODO: replace schema caliber_real
 */
DROP TABLE IF EXISTS public.test_intermediate;

SELECT
  test_intermediate.patid                    AS patid,
  test_intermediate.eventdate                AS eventdate,
  test_intermediate.staffid                  AS staffid,
  test_intermediate.enttype_string           AS enttype_string,
  enttype_map.target_concept_id              AS _concept_id,
  test_intermediate.value                    AS value,
  test_intermediate.range_from               AS range_from,
  test_intermediate.range_to                 AS range_to,
  test_intermediate.qualifier_tqu            AS qualifier_source_value,
  tqu_map.target_concept_id                  AS qualifier_concept_id,
  test_intermediate.operator_opr             AS operator_source_value,
  opr_map.target_concept_id                  AS operator_concept_id,
  test_intermediate.unit_sum                 AS unit_source_value,
  unit_map.target_concept_id                 AS unit_concept_id,
  test_intermediate.alternative_source_value AS alternative_source_value,
  target_enttype_concept.domain_id           AS target_domain_id
INTO public.test_intermediate
FROM (
       /* Test entities with 4 data fields */
       SELECT
         patid,
         eventdate,
         staffid,
         enttype :: TEXT       AS enttype_string,
         NULL                  AS value,
         test.data1            AS qualifier_tqu,
         test.data2 :: NUMERIC AS range_from,
         test.data3 :: NUMERIC AS range_to,
         NULL                  AS operator_opr,
         NULL                  AS unit_sum,
         NULL                  AS alternative_source_value
       FROM caliber_real.test AS test
         JOIN caliber.entity USING (enttype)
       WHERE data_fields = 4 AND test.data1 != '0'

       UNION ALL

       /* Test entities with 7 or 8 data fields */
       SELECT
         patid,
         eventdate,
         staffid,
         enttype :: TEXT       AS enttype_string,
         test.data2 :: NUMERIC AS value,
         test.data4            AS qualifier_tqu,
         test.data5 :: NUMERIC AS range_from,
         test.data6 :: NUMERIC AS range_to,
         test.data1            AS operator_opr,
         test.data3            AS unit_sum,
         NULL                  AS alternative_source_value
       FROM caliber_real.test AS test
         JOIN caliber.entity USING (enttype)
       WHERE data_fields > 4 AND test.data2 IS NOT NULL

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
         test.data7 AS alternative_source_value
       FROM caliber_real.test AS test
         JOIN caliber.entity USING (enttype)
       WHERE enttype IN (311) AND test.data7 IS NOT NULL AND test.data7 != '0'

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
         test.data8 AS alternative_source_value
       FROM caliber_real.test AS test
         JOIN caliber.entity USING (enttype)
       WHERE enttype IN (154, 284) AND data_fields = 8 AND test.data8 IS NOT NULL

     ) AS test_intermediate
  LEFT JOIN cdm5.source_to_concept_map AS enttype_map
    ON enttype_map.source_code = test_intermediate.enttype_string AND
       enttype_map.source_vocabulary_id = 'JNJ_CPRD_T_ET_LOINC'
  LEFT JOIN cdm5.concept AS target_enttype_concept
    ON enttype_map.target_concept_id = target_enttype_concept.concept_id
  LEFT JOIN cdm5.source_to_concept_map AS tqu_map
    ON tqu_map.source_code = test_intermediate.qualifier_tqu AND
       tqu_map.source_vocabulary_id = 'CPRD_QUALIFIER'
  LEFT JOIN cdm5.source_to_concept_map AS opr_map
    ON opr_map.source_code = test_intermediate.operator_opr AND
       opr_map.source_vocabulary_id = 'CPRD_QUALIFIER'
  LEFT JOIN cdm5.source_to_concept_map AS unit_map
    ON unit_map.source_code = test_intermediate.unit_sum AND
       unit_map.source_vocabulary_id = 'CPRD_UNIT';