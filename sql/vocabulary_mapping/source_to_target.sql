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
  WHERE source.vocabulary_id IN (
         'Read'
        ,'ICD10'
        ,'ICD9CM'
        ,'OPCS4'
        ,'HES Specialty'
        ,'CPRD_PRODUCT'
        ,'CPRD_QUALIFIER'
        ,'CPRD_UNIT'
        ,'JNJ_CPRD_ET_LOINC'
        ,'JNJ_CPRD_T_ET_LOINC'
        ,'JNJ_CPRD_HES_LOINC'
        ,'JNJ_CPRD_PROV_CMS2'
        ,'JNJ_CPRD_PROV_SPEC'
        ,'JNJ_CPRD_SCORE_LOINC'
      )
;
