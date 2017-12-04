\copy entity FROM 'resources/entity.txt' WITH HEADER CSV DELIMITER E'\t';

\copy medical FROM 'resources/medical.txt' WITH HEADER CSV DELIMITER E'\t';

\copy product FROM 'resources/product.txt' WITH HEADER CSV DELIMITER E'\t';

-- TODO: export as sql insert and combine with ddl_lookups to load with one sql script.
