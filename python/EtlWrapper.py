import re
import time
from python.process_additional import process_additional


def create_insert_message(sql_command, row_count, execution_time=None):
    """ Create message on how many lines inserted into which table """
    if row_count >= 0:
        table_into = '?'

        # TODO: multiple into's? => rowcount only about last
        match_into = re.search(r'INTO (.+?)\s', sql_command)
        if match_into:
            table_into = match_into.group(1)

        # table_from = "?"
        # match_from = re.search(r'FROM (.+?)\s', sql_command).group(1)
        # if match_from:
        #     table_from = match_from.group(1)

        return create_message(table_into, row_count, execution_time)

    return 'Nothing inserted'


def create_message(table_into, row_count, execution_time):
    return 'Inserted into {:<35} {:>9,} [{:>8.2f} s]'.format(table_into, row_count, execution_time)


class EtlWrapper(object):
    """ This module coordinates the execution of the sql files """

    def __init__(self, connection, source_schema, target_schema):
        self.connection = connection
        self.source_schema = source_schema
        self.target_schema = target_schema
        self.n_queries_executed = 0
        self.n_queries_failed = 0
        self.total_rows_inserted = 0

    def reset_summary_stats(self):
        self.n_queries_executed = 0
        self.n_queries_failed = 0
        self.total_rows_inserted = 0

    def execute(self):
        """Run Caliber to OMOP ETL procedure"""

        # Create functions
        self._create_functions()

        # Preparing the database
        self._prepare_cdm()
        # self._load_mappings()

        # Source preparation
        self._prepare_source()
        self.print_summary_message()
        self.reset_summary_stats()

        # Loading
        self._load()

        self.print_summary_message()

    def _create_functions(self):
        self.execute_sql_file('./sql/functions/createEndDate.sql')
        self.execute_sql_file('./sql/functions/createVisitId.sql')
        print("Sql functions created or replaced")

    def _prepare_cdm(self):
        """ Prepare OMOP CDM """
        self.execute_sql_file('./sql/cdm_prepare/drop_cdm_tables.sql')
        print("CDM tables dropped")

        self.execute_sql_file('./sql/cdm_prepare/OMOP CDM ddl - NonVocabulary.sql')
        print("CDMv5.2 tables created")

        self.execute_sql_file('./sql/cdm_prepare/alter_cdm.sql')
        self.execute_sql_file('./sql/cdm_prepare/create_id_sequence.sql')
        print("CDMv5.2 tables altered and id auto increment created")

    def _load_vocabulary_mappings(self):
        self.execute_sql_file('./sql/vocabulary_mapping/load_mapping_tables.sql')

    def _prepare_source(self):
        self.execute_sql_file('./sql/source_preprocessing/medcode_merge.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/test_intermediate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/therapy_numdays_aggregate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/observation_period_validity.sql', True)
        t1 = time.time()
        row_count = process_additional(self.connection, target_table='additional_intermediate', target_schema='public')
        self.total_rows_inserted += row_count
        print(create_message('public.additional_intermediate', row_count, time.time()-t1))

    def _load(self):
        # TODO: is this the correct order?
        self.execute_sql_file('./sql/loading/location.sql', True)
        self.execute_sql_file('./sql/loading/care_site.sql', True)
        self.execute_sql_file('./sql/loading/provider.sql', True)
        self.execute_sql_file('./sql/loading/person.sql', True)
        self.execute_sql_file('./sql/loading/observation_period.sql', True)
        self.execute_sql_file('./sql/loading/visit_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/consultation_to_visit_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_condition_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_procedure_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_drug_exposure.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/medcode_to_device_exposure.sql', True)
        self.execute_sql_file('./sql/loading/test_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/test_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/additional_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/additional_to_observation.sql', True)

    def print_summary_message(self):
        print()
        print("Queries succesfully executed: %d" % self.n_queries_executed)
        print("Queries failed: %d" % self.n_queries_failed)
        print("Rows inserted: {:,}".format(self.total_rows_inserted))

    def execute_sql_file(self, filename, verbose=False):
        # Open and read the file as a single buffer
        fd = open(filename, 'r')
        sql_file = fd.read()
        fd.close()
    
        # Execute all sql commands in the file (commands are ; separated)
        # Note: returns result for last command?
        try:
            t1 = time.time()
            result = self.connection.execute(sql_file)
            time_delta = time.time() - t1
        except Exception as msg:
            print("Command in '%s' failed: %s" % (filename, msg))
            self.n_queries_failed += 1
            return
    
        if verbose:
            message = create_insert_message(sql_file, result.rowcount, time_delta)
            print(message)

        # Note: only tracks row count correctly if 1 insert per file and no update/delete scripts
        if result.rowcount > 0:
            self.total_rows_inserted += result.rowcount

        self.n_queries_executed += 1
