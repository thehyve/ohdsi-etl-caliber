\copy entity FROM 'resources/cprd_lookups/entity.txt' WITH HEADER CSV DELIMITER E'\t';

\copy medical FROM 'resources/cprd_lookups/medical.txt' WITH HEADER CSV DELIMITER E'\t';

\copy product FROM 'resources/cprd_lookups/product.txt' WITH HEADER CSV DELIMITER E'\t';

-- TODO: export as sql insert and combine with ddl_lookups to load with one sql script.
