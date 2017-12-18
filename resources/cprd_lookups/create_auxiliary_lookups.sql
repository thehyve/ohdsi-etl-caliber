/* Small lookup files combined in one table */
DROP TABLE IF EXISTS auxiliary_lookups;
CREATE TABLE auxiliary_lookups (
  lookup_type CHAR(3),
  lookup_name TEXT,
  code        TEXT,
  description TEXT
);

\copy auxiliary_lookups FROM 'resources/cprd_lookups/auxiliary_lookups.txt';

CREATE UNIQUE INDEX IF NOT EXISTS idx_auxiliary_lookups_type
  ON auxiliary_lookups (lookup_type, code);