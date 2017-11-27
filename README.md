# ohdsi-etl-caliber

## Dependencies
- Postgres (9.5.9)
  - Pl/Python
  ```bash
  select version();
  sudo apt-get install postgresql-plpython3-<postgresql version>
  CREATE EXTENSION plpython3u;
  ```
- Python 3, with the following installed using `pip install <package_name>`:
  - click
  - sqlalchemy
  - psycopg2

## Acknowledgements
This work builds on the CPRD mapping efforts made by Janssen, led by Amy Matcho. The original ETL documentation can be found [here](https://github.com/OHDSI/ETL-CDMBuilder/tree/master/man/CPRD)
