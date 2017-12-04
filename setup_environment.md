# Database Environment setup

Assumption: PostgreSQL runs on default host `127.0.0.1`, port `5432` and user `postgres`

## Database creation
Create database with three schemas (target schema, source schema and public) and the python language extension.
Note that the OMOP target schema should be called `cdm5`.
```sql
CREATE DATABASE <database_name>;
CREATE SCHEMA cdm5;
CREATE SCHEMA <source_schema_name>;
CREATE EXTENSION plpython3u;
```

Note: if the files for `plpython3u` are not available, install these with:
```bash
apt install postgresql-plpython3-<postgresql-version>
```

## Initialize the OMOP Common Data Model
Download the latest v5 vocabulary from [Athena](http://athena.ohdsi.org/vocabulary/list). An account has to be created first.
Keep all the default selected vocabularies.
In addition, make sure that also the following vocabularies are selected:
```
 2. ICD9CM
17. Read
34. ICD10
55. OPCS4
57. HES Specialty
75. dm+d
```
Move this file to the server and unpack to `~/omop_vocabulary/`.

Then, use the code blow to clone the OMOP CDM v5.2 DDL from github (`OHDSI-CommonDataModel`). 
The specified branch of The Hyve also includes an initialization script.
This script creates all CDM tables, loads the vocabulary and applies indices and constraints.
```bash
git clone -b cdm-auto-create https://github.com/thehyve/OHDSI-CommonDataModel.git
cd OHDSI-CommonDataModel/PostgreSQL
sh "Initialize - PostgreSQL.sh" 127.0.0.1 5432 <database_name> cdm5 <user> <password> ~/omop_vocabulary/
```

_Note: this step creates both the vocabulary tables and non-vocabulary tables.
However, this step is only essential for initialization of the **vocabulary** tables.
The non-vocabulary tables, containing the actual EHR data, will be dropped and recreated by the ETL script._

## Initialize CALIBER
Load the CALIBER data set into the source data schema.

If not included already, also include the `medical`, `product` and `entity` lookup tables in this schema.
See the section on ETL/Lookups describes how to load them from the ETL repository.

The source schema should contain the following tables:
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

Then create and load lookup tables with:
```bash
sudo -u postgres psql -c "ALTER DATABASE <database_name> SET search_path TO <source_schema_name>;"
sudo -u postgres psql -d <database_name> -f sql/source_preprocessing/ddl_lookups.sql
sudo -u postgres psql -d <database_name> -f sql/source_preprocessing/load_lookups.sql
```
