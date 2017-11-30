# ohdsi-etl-caliber

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

## Acknowledgements
This work builds on the CPRD mapping efforts made by Janssen, led by Amy Matcho. The original ETL documentation can be found [here](https://github.com/OHDSI/ETL-CDMBuilder/tree/master/man/CPRD)
