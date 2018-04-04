-- To 2B concepts
-- Replaces any conflicting existing 2B concepts.
-- TODO: infer domain and concept_class
INSERT INTO cdm5.concept (
  concept_id,
  concept_code,
  vocabulary_id,
  concept_name,
  valid_start_date,
  valid_end_date,
  domain_id,
  concept_class_id
)
  SELECT
    ROW_NUMBER() OVER (
      ORDER BY
        source_vocabulary_id,
        source_code
      ) + 2000000000                         AS concept_id,
    source_code                              AS concept_code,
    source_vocabulary_id                     AS vocabulary_id,
    STRING_AGG(source_code_description, ';') AS concept_name,
    MIN(valid_start_date)                    AS valid_start_date,
    MAX(valid_end_date)                      AS valid_end_date,
    'Metadata'                               AS domain_id, -- TODO
    'Undefined'                              AS concept_class_id -- TODO
  FROM cdm5.source_to_concept_map
  GROUP BY
    source_code,
    source_vocabulary_id
ON CONFLICT (concept_id)
  DO UPDATE
    SET concept_code   = excluded.concept_code,
      vocabulary_id    = excluded.vocabulary_id,
      concept_name     = excluded.concept_name,
      valid_start_date = excluded.valid_start_date,
      valid_end_date   = excluded.valid_end_date,
      domain_id        = excluded.domain_id,
      concept_class_id = excluded.concept_class_id
    -- Note: excluded refers to row proposed for insertion
;
