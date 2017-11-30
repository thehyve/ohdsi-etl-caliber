import sys
import time
from python.EtlWrapper import EtlWrapper
from sqlalchemy import create_engine
import click


@click.command()
@click.option('--hostname', '-h', default='localhost', metavar='<host>',
              help='Database server host or socket directory (localhost)')
@click.option('--port', '-p', default='6000', metavar='<port>',
              help='Database server port (6000)')
@click.option('--database', '-d', default='caliber_dev', metavar='<database>',
              help='Database name to connect to (caliber_dev)')
@click.option('--username', '-u', default='postgres', metavar='<username>',
              help='Database user name (postgres)')
@click.option('--password', '-w', default='', metavar='<pw>',
              help='User password ()')
@click.option('--source', '-s', default='caliber', metavar='<schema_name>',
              help='Source schema containing with CALIBER tables (caliber)')
@click.option('--target', '-t', default='cdm5', metavar='<schema_name>',
              help='**DEPRECATED** Target schema where OMOP CDM tables are created')
@click.option('--debug', default=False, metavar='<debug_mode>', is_flag=True,
              help='In debug mode, the table constraints are applied before loading')
@click.option('--logger', '-l', default='', metavar='<file_name>',
              help='Filename of the file where the log will be written (log_<timestamp>.txt)')
def main(database, username, password, hostname, port, source, target, debug, logger):
    """CALIBER to OMOP CDM, an ETL procedure
    """
    if not logger:
        logger = "log_%s.txt" % time.strftime('%Y-%m-%dT%H%M%S')

    # Connect to database
    eng = create_engine('postgresql://%s:%s@%s:%s/%s' % (username, password, hostname, port, database))
    with eng.connect() as connection, open(logger, 'w') as f_log:
        etl = EtlWrapper(connection, source, target, debug)
        etl.set_log_file(f_log)
        etl.execute()

if __name__ == "__main__":
    sys.exit(main())
