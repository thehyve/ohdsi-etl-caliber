DROP TABLE IF EXISTS medical;
CREATE TABLE medical (
  medcode  INTEGER NOT NULL,
  readcode VARCHAR(8),
  readterm TEXT
);

\copy medical FROM 'resources/cprd_lookups/medical.txt' WITH HEADER CSV DELIMITER E'\t';

CREATE INDEX IF NOT EXISTS idx_lookup_medical_medcode
  ON medical (medcode); -- originally called lookup_medical