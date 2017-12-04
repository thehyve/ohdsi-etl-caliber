# ohdsi-etl-caliber
ETL scripts to convert CALIBER data to OMOP CDM.
The script are mainly sql queries, that are executed from Python.

## Dependencies
- Postgres (9.5)
  - Pl/Python
  ```bash
  select version();
  sudo apt-get install postgresql-plpython3-<postgresql version>
  CREATE EXTENSION plpython3u;
  ```
  - cdm5 schema with OMOP vocabulary pre-loaded
- Python 3, with the following installed:
  - pip `apt install python3-pip`
  - click `pip3 install click`
  - sqlalchemy `pip3 install sqlalchemy`
  - psycopg2 `pip3 install psycopg2`

## Setup
See [this document](setup_environment.md) describing the initial setup. 
In this setup the dependencies are installed and the target `cdm5` schema is initialized with the OMOP vocabulary tables.

## Run ETL
```bash
python3 main.py -h 127.0.0.1 -d <database-name> -p <port> -u <user> -w <password>  -s <source-schema>
```

Supply `--debug` to apply constraints before loading. This gives direct feedback if any constraints fail, but is less optimal for loading speed.

A log file will be created in the current working directory.

## Acknowledgements
This work builds on the CPRD mapping efforts made by Janssen, led by Amy Matcho. The original ETL documentation can be found [here](https://github.com/OHDSI/ETL-CDMBuilder/tree/master/man/CPRD)
