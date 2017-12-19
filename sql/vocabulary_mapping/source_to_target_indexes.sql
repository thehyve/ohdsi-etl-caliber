CREATE INDEX IF NOT EXISTS idx_source
  ON cdm5.source_to_target (source_vocabulary_id, source_code);