## 

Assumption: postgresql runs on default host `127.0.0.1` and port `5432`

# Postgres
Create database with three schemas (including public) and the python extension.
```sql
CREATE DATABASE caliber_acc;
CREATE SCHEMA cdm5;
CREATE SCHEMA caliber;
CREATE EXTENSION plpython3u;
```

Note: if the files for `plpython3u` are not available, install these with:
```bash
apt install postgresql-plpython3-<postgresql-version>
```

## Initialize the OMOP Common Data Model
First, download the latest v5 vocabulary here:
http://athena.ohdsi.org/vocabulary/list 

Keep all the default selected vocabularies.
In addition, make sure that the following vocabularies are selected:
* dm+d
* OPCS4
* Meddra
* Read
* ICD10
* ICD9CM

Move this file to the server and unpack to `~/omop_vocabulary/`.

Then, use the code blow to clone the OMOP CDM v5.2 DDL from github (`OHDSI-CommonDataModel`). 
The specified branch of The Hyve also includes an initialization script.
This script creates all CDM tables, loads the vocabulary and applies indices and constraints.
```bash
git clone -b cdm-auto-create https://github.com/thehyve/OHDSI-CommonDataModel.git
cd OHDSI-CommonDataModel/PostgreSQL
sh "Initialize - PostgreSQL.sh" 127.0.0.1 5432 caliber_acc cdm5 postgres heartdatabig ~/omop_vocabulary/
```

_Note: this step creates both the vocabulary tables and non-vocabulary tables.
However, this step is only essential for initialization of the **vocabulary** tables.
The non-vocabulary tables, containing the actual EHR data, will be dropped and recreated by the ETL script._

## Initialize CALIBER
Load the CALIBER data set into the caliber schema.

If not included already, also include the `medical`, `product` and `entity` lookup tables in this schema.
See the section on ETL/Lookups describes how to load them from the ETL repository.

The caliber schema should contain the following tables:
* hes_diag_epi
* hes_diag_hosp
* hes_op_clinical
* hes_patient
* hes_proc_epi
* ons_death
* ons_imd
* patient
* practice
* referral
* test
* therapy
* additional
* clinical
* consultation
* immunisation
* lookup_linkage_eligibility
* hes_op_clinical_diag
* hes_op_clinical_proc
* hes_op_appt
* staff
* hes_op_patient
* medical
* product
* entity

### Lookup tables
If the `medical`, `product` and `entity` lookup tables are not yet included in the caliber schema,
 they can be created and loaded from the ETL repository.

First clone from git or download source code
```bash
git clone https://github.com/thehyve/ohdsi-etl-caliber.git
cd ohdsi-etl-caliber
```

Then execute:
```bash
sudo -u postgres psql -d caliber_acc -f sql/source_preprocessing/ddl_lookups.sql
sudo -u postgres psql -d caliber_acc -f sql/source_preprocessing/load_lookups.sql
```



