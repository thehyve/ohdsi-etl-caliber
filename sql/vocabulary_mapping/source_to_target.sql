/*
* Intermediate table of all source code to standard concept mappings in OMOP v5.
* Combines the 'Maps to' relationship between default concepts and the custom source_to_concept_map.
* Adapted from the OHDSI community.
* Goals:
*  - Performance boost: preselection of source vocabularies
*  - Performance boost: using less joins in loading queries
*  - Better readability: less joins at loading queries
*  - Better readability: one syntax for all source concept mappings
*  - Add source_domain and target_domain info for choosing target cdm table
*  - Central overview of used mappings.
* NOTE: all required source vocabulary_ids have to be inputted here (!)
*/
DROP TABLE IF EXISTS cdm5.source_to_target;

CREATE TABLE cdm5.source_to_target AS
  WITH maps_to AS (
      SELECT
        source.concept_code     AS source_code,
        source.concept_id       AS source_concept_id,
        source.vocabulary_id    AS source_vocabulary_id,
        source.domain_id        AS source_domain_id,
        source.concept_class_id AS source_concept_class_id,
        target.concept_id       AS target_concept_id,
        target.vocabulary_id    AS target_vocabualry_id,
        target.domain_id        AS target_domain_id,
        target.concept_class_id AS target_concept_class_id,
        target.invalid_reason   AS target_invalid_reason,
        target.standard_concept AS target_standard_concept
      FROM cdm5.concept AS source
        LEFT JOIN cdm5.concept_relationship AS rel
          ON source.concept_id = concept_id_1
             AND relationship_id = 'Maps to'
        JOIN cdm5.concept AS target
          ON concept_id_2 = target.concept_id
      WHERE source.vocabulary_id IN ('Read', 'ICD10', 'ICD9CM', 'OPCS4', 'dm+d')
  ), stcm AS (
      SELECT
        source_code                                  AS source_code,
        source_concept_id                            AS source_concept_id,
        NULLIF(source_vocabulary_id, 'None')         AS source_vocabulary_id,
        NULLIF(source.domain_id, 'Metadata')         AS source_domain_id,
        NULLIF(source.concept_class_id, 'Undefined') AS source_concept_class_id,
        target_concept_id                            AS target_concept_id,
        NULLIF(target_vocabulary_id, 'None')         AS target_vocabulary_id,
        NULLIF(target.domain_id, 'Metadata')         AS target_domain_id,
        NULLIF(target.concept_class_id, 'Undefined') AS target_concept_class_id,
        target.invalid_reason                        AS target_invalid_reason,
        target.standard_concept                      AS target_standard_concept
      FROM cdm5.source_to_concept_map AS stcm
        LEFT JOIN cdm5.concept AS source
          ON source.concept_id = stcm.source_concept_id
        LEFT JOIN cdm5.concept AS target
          ON target.concept_id = stcm.target_concept_id
  )
  SELECT *
  FROM maps_to
  UNION
  SELECT *
  FROM stcm;

CREATE INDEX idx_source
  ON cdm5.source_to_target (source_vocabulary_id, source_code);
