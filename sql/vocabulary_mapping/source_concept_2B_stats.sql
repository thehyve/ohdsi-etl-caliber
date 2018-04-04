/*** BEFORE ***/
-- Total count
SELECT
  CASE WHEN target_concept_id > 0 THEN TRUE ELSE FALSE END AS is_mapped,
  count(DISTINCT CONCAT(source_code, source_vocabulary_id))
FROM cdm5.source_to_concept_map
GROUP BY is_mapped
;
-- To non-standard before
SELECT
  source_vocabulary_id,
  COUNT(*) AS frequency
FROM cdm5.source_to_concept_map
  JOIN cdm5.concept AS target ON target_concept_id = concept_id
WHERE target_concept_id > 0 AND standard_concept IS NULL
GROUP BY source_vocabulary_id
ORDER BY frequency DESC;

/*** AFTER ***/
-- Total count
SELECT
  CASE WHEN concept_id_2 > 0 THEN TRUE ELSE FALSE END AS is_mapped,
  count(*)
FROM cdm5.concept
  LEFT JOIN cdm5.concept_relationship ON concept_id_1 = concept_id
WHERE concept_id > 2000000000
GROUP BY is_mapped
;
-- To non-standard after
SELECT
  source.vocabulary_id,
  count(*) AS frequency
FROM cdm5.concept_relationship
  JOIN cdm5.concept AS source ON concept_id_1 = source.concept_id
  JOIN cdm5.concept AS target ON concept_id_2 = target.concept_id
WHERE target.standard_concept IS NULL AND concept_id_1 > 2000000000
GROUP BY source.vocabulary_id
ORDER BY frequency DESC;