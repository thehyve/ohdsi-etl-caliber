\copy caliber.entity FROM '/resources/entity.txt' WITH HEADER CSV DELIMITER E'\t';

\copy caliber.medical FROM '/resources/medical.txt' WITH HEADER CSV DELIMITER E'\t';

\copy caliber.product FROM '/resources/product.txt' WITH HEADER CSV DELIMITER E'\t';

-- /Users/Maxim/Develop/OHDSI/ohdsi-etl-caliber/