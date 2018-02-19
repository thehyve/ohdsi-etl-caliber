import sys
import time
from python.EtlWrapper import EtlWrapper
from sqlalchemy import create_engine
import click

__version__ = '0.1.2-SNAPSHOT'


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
@click.option('--debug', default=False, metavar='<debug_mode>', is_flag=True,
              help='In debug mode, the table constraints are applied before loading')
@click.option('--skipvocab', default=False, metavar='<skip_vocab>', is_flag=True,
              help='When provided, the time consuming loading and pre-processing '
                   'of source to target vocabularies is skipped')
@click.option('--logger', '-l', default='', metavar='<file_name>',
              help='Filename of the file where the log will be written (log_<timestamp>.txt)')
def main(database, username, password, hostname, port, source, debug, skipvocab, logger):
    """CALIBER to OMOP CDM, an ETL procedure
    """
    if not logger:
        logger = "log_%s.txt" % time.strftime('%Y-%m-%dT%H%M%S')

    # Connect to database
    eng = create_engine('postgresql://%s:%s@%s:%s/%s' % (username, password, hostname, port, database))
    with eng.connect() as connection, open(logger, 'w') as f_log:
        etl = EtlWrapper(connection, source, debug, skipvocab)
        etl.set_log_file(f_log)
        etl.log("ETL version " + __version__)
        etl.execute()


if __name__ == "__main__":
    if sys.version_info > (3, 0):
        # Python 3, that's good
        sys.exit(main())
    else:
        # Python 2, not supported
        sys.exit('ERROR: Python version <2 NOT supported. Please use Python 3.x')
