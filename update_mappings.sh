sudo -u postgres psql -d caliber_dev -c 'TRUNCATE cdm5.source_to_concept_map'
sudo -u postgres psql -d caliber_dev -c 'DELETE FROM cdm5.vocabulary WHERE vocabulary_concept_id = 0'

sudo -u postgres psql -d caliber_dev -f 'sql/mapping/load_mapping_tables.sql'