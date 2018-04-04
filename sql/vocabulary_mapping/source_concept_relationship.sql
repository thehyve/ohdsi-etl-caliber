-- Insert maps to and mapped from relationships,
-- Also looks for new standard mappings of non-standard target concepts in the source_to_concept_map
INSERT INTO cdm5.concept_relationship (
  concept_id_1,
  relationship_id,
  concept_id_2,
  valid_start_date,
  valid_end_date
)
  WITH stcm_remapped AS (
      SELECT
        stcm.source_code,
        stcm.source_concept_id,
        stcm.source_vocabulary_id,
        stcm.source_code_description,
        COALESCE(concept_id_2, target_concept_id) AS target_concept_id,
        stcm.valid_start_date,
        stcm.valid_end_date
      FROM cdm5.source_to_concept_map AS stcm
        LEFT JOIN cdm5.concept_relationship
          ON target_concept_id = concept_id_1
             AND relationship_id LIKE 'Maps to'
  ), stcm_selection AS (
      SELECT
        source_concept.concept_id  AS source_concept_id,
        target_concept_id,
        CAST('1980-01-01' AS DATE) AS valid_start_date,
        CAST('2099-12-31' AS DATE) AS valid_end_date
      FROM stcm_remapped
        JOIN cdm5.concept AS source_concept
          ON source_code = concept_code
             AND source_vocabulary_id = vocabulary_id
      WHERE
        target_concept_id > 0
  )
  SELECT
    source_concept_id AS concept_id_1,
    'Maps to'         AS relationship_id,
    target_concept_id AS concept_id_2,
    valid_start_date,
    valid_end_date
  FROM stcm_selection

  UNION

  SELECT
    target_concept_id AS concept_id_1,
    'Mapped from'     AS relationship_id,
    source_concept_id AS concept_id_2,
    valid_start_date,
    valid_end_date
  FROM stcm_selection

ON CONFLICT (concept_id_1, relationship_id, concept_id_2)
  DO UPDATE
    SET concept_id_1   = excluded.concept_id_1,
      relationship_id  = excluded.relationship_id,
      concept_id_2     = excluded.concept_id_2,
      valid_start_date = excluded.valid_start_date,
      valid_end_date   = excluded.valid_end_date
    -- Note: excluded refers to row proposed for insertion
;