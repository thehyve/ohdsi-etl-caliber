DROP TABLE IF EXISTS product;
CREATE TABLE product (
  prodcode      INTEGER NOT NULL,
  gemscriptcode VARCHAR(8),
  productname   TEXT,
  drugsubstance TEXT,
  strength      TEXT,
  formulation   TEXT,
  route         TEXT,
  bnfcode       TEXT,
  bnfchapter    TEXT
);

\copy product FROM 'resources/cprd_lookups/product.txt' WITH HEADER CSV DELIMITER E'\t';

CREATE INDEX IF NOT EXISTS idx_lookup_product_prodcode
  ON product (prodcode); -- originally called lookup_product