import os
import time
from sqlalchemy import text
from python.process_additional import process_additional
from python.wrapper_util import *


class EtlWrapper(object):
    """ This module coordinates the execution of the sql files
        If debug mode is on, the primary key constraints are applied before loading
        to get direct feedback if there are issues. This does make loading slower.
    """

    def __init__(self, connection, source_schema, target_schema, debug):
        self.connection = connection
        self.source_schema = source_schema
        self.target_schema = target_schema
        self.debug = debug
        self.n_queries_executed = 0
        self.n_queries_failed = 0
        self.total_rows_inserted = 0
        self.is_constraints_applied = False
        self.log_file = None
        self.t1 = None
        self.cwd = os.getcwd()

    def set_log_file(self, f):
        self.log_file = f

    def reset_summary_stats(self):
        self.n_queries_executed = 0
        self.n_queries_failed = 0
        self.total_rows_inserted = 0

    def log(self, message='', end='\n'):
        """Write to standard output and the log file (if set)"""
        print(message, end=end, flush=True)
        if self.log_file:
            self.log_file.write(message)
            self.log_file.write(end)
            self.log_file.flush()

    def print_run_time(self):
        """Prints timestamp and starts timer on first call. Prints total execution time on subsequent calls"""
        self.log(time.strftime('\n%a %Y-%m-%d %H:%M:%S'))
        if not self.t1:
            self.log('=== ETL started ===')
            self.t1 = time.time()
        else:
            self.log('Total run time: {:>20.1f} seconds'.format(time.time() - self.t1))

    def print_summary_message(self):
        self.log()
        self.log("Queries successfully executed: %d" % self.n_queries_executed)
        self.log("Queries failed: %d" % self.n_queries_failed)
        self.log("Rows inserted: {:,}".format(self.total_rows_inserted))

    def execute(self):
        """Run Caliber to OMOP ETL procedure"""
        self.print_run_time()

        # Create functions
        self._create_functions()

        # Preparing the database
        self._prepare_cdm()
        self._load_vocabulary_mappings()

        # Source preparation
        self._prepare_source()
        self.print_summary_message()
        self.reset_summary_stats()

        self.print_run_time()

        # Loading
        self._load()

        self.print_summary_message()

        # Derived tables
        self._derive_era()

        # Constraints and Indices
        self._apply_constraints()
        self._apply_indexes()

        self.print_run_time()

    def _create_functions(self):
        self.execute_sql_file('./sql/functions/createEndDate.sql')
        self.execute_sql_file('./sql/functions/createVisitId.sql')
        self.execute_sql_file('./sql/functions/createHesApptVisitId.sql')
        self.execute_sql_file('./sql/functions/createCareSiteId.sql')
        self.execute_sql_file('./sql/functions/mapCprdLookup.sql')
        self.execute_sql_file('./sql/functions/mapIcdCode.sql')
        # TODO: execute unit tests?
        self.log("Sql functions created or replaced")

    def _prepare_cdm(self):
        """ Prepare OMOP CDM """
        self.execute_sql_file('./sql/cdm_prepare/drop_cdm_tables.sql')
        self.log("CDM tables dropped")

        self.execute_sql_file('./sql/cdm_prepare/OMOP CDM ddl - NonVocabulary.sql')
        self.is_constraints_applied = False
        self.log("CDMv5.2 tables created")

        self.execute_sql_file('./sql/cdm_prepare/alter_cdm.sql')
        self.execute_sql_file('./sql/cdm_prepare/create_id_sequence.sql')
        self.log("CDMv5.2 tables altered and id auto increment created")

        if self.debug:
            self._apply_constraints()

    def _load_vocabulary_mappings(self):
        self.execute_sql_file('./sql/vocabulary_mapping/load_mapping_tables.sql')
        self.execute_sql_file('./resources/CPRD_Lookups.sql')

    def _prepare_source(self):
        self.log("\nIntermediate tables and aggregates...")
        self.execute_sql_file('./sql/source_preprocessing/medcode_intermediate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/test_intermediate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/hes_diagnoses_intermediate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/therapy_numdays_aggregate.sql', True)
        self.execute_sql_file('./sql/source_preprocessing/observation_period_validity.sql', True)
        self.execute_process_additional()

    def _load(self):
        self.log("\nMain loading queries...")
        self.execute_sql_file('./sql/loading/location.sql', True)
        self.execute_sql_file('./sql/loading/care_site.sql', True)
        self.execute_sql_file('./sql/loading/provider.sql', True)

        self.execute_sql_file('./sql/loading/person.sql', True)
        self.execute_sql_file('./sql/loading/death.sql', True)
        self.execute_sql_file('./sql/loading/observation_period.sql', True)

        self.execute_sql_file('./sql/loading/visit_occurrence.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_condition_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/hes_diagnoses_to_condition_occurrence.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_procedure_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/hes_proc_epi_to_procedure.sql', True)
        self.execute_sql_file('./sql/loading/hes_op_clinical_proc_to_procedure_occurrence.sql', True)
        self.execute_sql_file('./sql/loading/hes_diagnoses_to_procedure_occurrence.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_drug_exposure.sql', True)
        self.execute_sql_file('./sql/loading/therapy_to_drug_exposure.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_device_exposure.sql', True)
        self.execute_sql_file('./sql/loading/therapy_to_device_exposure.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/test_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/additional_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/hes_diagnoses_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/ons_imd_to_measurement.sql', True)
        self.execute_sql_file('./sql/loading/patient_famnum_to_measurement.sql', True)

        self.execute_sql_file('./sql/loading/medcode_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/test_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/additional_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/hes_diagnoses_to_observation.sql', True)
        self.execute_sql_file('./sql/loading/patient_marital_to_observation.sql', True)

    def _derive_era(self):
        self.execute_sql_file('./sql/drug_era_stockpile.sql', True)

    def _apply_constraints(self):
        if self.is_constraints_applied:
            return

        self.log("Applying constraints...")
        self.execute_sql_file('./sql/cdm_prepare/OMOP CDM constraints - PK - NonVocabulary.sql', False)
        self.execute_sql_file('./sql/cdm_prepare/OMOP CDM constraints - FK - NonVocabulary.sql', False)
        self.is_constraints_applied = True

    def _apply_indexes(self):
        self.log("Applying indexes...")
        self.execute_sql_file('./sql/cdm_prepare/OMOP CDM indexes required - NonVocabulary.sql', False)

    def execute_sql_file(self, filename, verbose=False):
        # Open and read the file as a single buffer
        f = open(filename, 'r')
        query = f.read().strip()
        f.close()

        # Execute all sql commands in the file (commands are ; separated)
        # Note: returns result for last command?
        if verbose:
            self.log(create_current_file_message(filename), end='')

        query = query.replace('@absPath', self.cwd)
        query = query.replace("@source_schema", self.source_schema)

        try:
            t1 = time.time()
            statement = text(query)
            result = self.connection.execute(statement)
            time_delta = time.time() - t1
        except Exception as msg:
            error = msg.args[0].split('\n')[0]
            if verbose:
                self.log("###")  # newline before error
            self.log("Query in '%s' failed:" % filename)
            self.log("\t%s" % error)
            # TODO: create log of this error
            self.n_queries_failed += 1
            return

        if verbose:
            message = create_insert_message(query, result.rowcount, time_delta)
            self.log(message)

        # Note: only tracks row count correctly if 1 insert per file and no update/delete scripts
        if result.rowcount > 0:
            self.total_rows_inserted += result.rowcount

        self.n_queries_executed += 1

    def execute_process_additional(self):
        t1 = time.time()
        self.log(create_current_file_message('additional_intermediate'), end='')
        row_count = process_additional(self.connection, source_schema=self.source_schema, target_schema='public')
        self.total_rows_inserted += row_count
        self.log(create_message('public.additional_intermediate', row_count, time.time() - t1))
