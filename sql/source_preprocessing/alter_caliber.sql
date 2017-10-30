ALTER TABLE caliber.medical ADD CONSTRAINT medical_medcode_pk PRIMARY KEY (medcode);
CREATE INDEX medical_readcode_index ON caliber.medical (readcode);
