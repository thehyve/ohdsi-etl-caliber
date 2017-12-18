DROP TABLE IF EXISTS entity;
CREATE TABLE entity (
  enttype     INTEGER     NOT NULL,
  description TEXT        NOT NULL,
  filetype    VARCHAR(10) NOT NULL,
  category    TEXT,
  data_fields INTEGER     NOT NULL,
  data1       TEXT,
  data1_lkup  TEXT,
  data2       TEXT,
  data2_lkup  TEXT,
  data3       TEXT,
  data3_lkup  TEXT,
  data4       TEXT,
  data4_lkup  TEXT,
  data5       TEXT,
  data5_lkup  TEXT,
  data6       TEXT,
  data6_lkup  TEXT,
  data7       TEXT,
  data7_lkup  TEXT,
  data8       TEXT,
  data8_lkup  TEXT
);

\copy entity FROM 'resources/cprd_lookups/entity.txt' WITH HEADER CSV DELIMITER E'\t';

CREATE INDEX IF NOT EXISTS idx_lookup_entity_enttype
  ON entity (enttype); -- originally called lookup_entity
