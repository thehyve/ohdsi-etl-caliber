import sys
from python.EtlProcessor import EtlProcessor
from sqlalchemy import create_engine
import click


@click.command()
@click.option('--database', '-d', default='caliber_dev', metavar='<database>',
              help='Database name to connect to ')
@click.option('--hostname', '-h', default='localhost', metavar='<host>',
              help='Database server host or socket directory ')
@click.option('--username', '-u', default='postgres', metavar='<username>',
              help='Database user name ')
@click.option('--password', '-w', default='', metavar='<pw>',
              help='User password')
@click.option('--port', '-p', default='6000', metavar='<port>',
              help='Database server port ')
@click.option('--source_schema', '-s', default='caliber', metavar='<schema_name>',
              help='Source schema containing with CALIBER tables')
@click.option('--target_schema', '-t', default='cdm5', metavar='<schema_name>',
              help='Target schema where OMOP CDM tables are created')
def main(database, username, password, hostname, port, source_schema, target_schema):
    """Execute the ETL procedure
    Dependencies: sqlalchemy and click
    """
    # Connect to database
    eng = create_engine('postgresql://%s:%s@%s:%s/%s' % (username, password, hostname, port, database))
    with eng.connect() as connection:
        etl = EtlProcessor(connection, source_schema, target_schema)
        etl.execute()

if __name__ == "__main__":
    sys.exit(main())