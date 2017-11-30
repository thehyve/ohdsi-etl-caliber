ALTER TABLE @source_schema.medical ADD CONSTRAINT medical_medcode_pk PRIMARY KEY (medcode);
CREATE INDEX medical_readcode_index ON @source_schema.medical (readcode);

ALTER TABLE @source_schema.test ALTER COLUMN data1 TYPE INTEGER USING data1::INTEGER;
ALTER TABLE @source_schema.test ALTER COLUMN data2 TYPE NUMERIC USING data2::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data3 TYPE NUMERIC USING data3::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data4 TYPE NUMERIC USING data4::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data5 TYPE NUMERIC USING data5::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data6 TYPE NUMERIC USING data6::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data7 TYPE NUMERIC USING data7::NUMERIC;
ALTER TABLE @source_schema.test ALTER COLUMN data8 TYPE NUMERIC USING data8::NUMERIC;