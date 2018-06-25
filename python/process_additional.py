# Copyright 2018 The Hyve
#
# Licensed under the GNU General Public License, version 3,
# or (at your option) any later version (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# https://www.gnu.org/licenses/
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

#!/usr/bin/env python3
from datetime import datetime
from multiprocessing import Pool
from sqlalchemy import text

MAX_DATA_COLUMNS = 7


def sql_string_param(s):
    if not s:
        return "NULL"
    s = s.replace("'", "''")
    return "'%s'" % s


def sql_date_param(d):
    if d is None:
        return "NULL"
    return sql_string_param(d.strftime('%Y-%m-%d'))


def sql_numeric_param(i):
    if i is None:
        return "NULL"
    try:
        float(i)
    except ValueError as error:
        print(error, end='. ')
        print("The value is set to NULL")
        return "NULL"
    return str(i)


def sql_create_values_string(*args):
    return '(' + ",".join(args) + ')'


def create_intermediate_table(connection, schema, table_name):
    ddl_sql = ("DROP TABLE IF EXISTS {0}.{1};"
               "CREATE TABLE {0}.{1} ("
               "    patid INTEGER,"
               "    adid INTEGER,"
               "    enttype_string VARCHAR,"
               "    datafield_name VARCHAR,"
               "    data_value NUMERIC,"
               "    data_code TEXT,"
               "    data_date DATE,"
               "    lookup_type VARCHAR,"
               "    unit_code VARCHAR"
               ")").format(schema, table_name)
    connection.execute(ddl_sql)


def get_additional(connection, schema):
    return connection.execute(
        ("SELECT patid,enttype,adid,entity.data_fields,"
         "additional.data1,additional.data2,additional.data3,"
         "additional.data4,additional.data5,additional.data6,additional.data7,"
         "entity.data1,entity.data2,entity.data3,"
         "entity.data4,entity.data5,entity.data6,entity.data7,"
         "entity.data1_lkup,entity.data2_lkup,entity.data3_lkup,"
         "entity.data4_lkup,entity.data5_lkup,entity.data6_lkup,entity.data7_lkup\n"
         "FROM {0}.additional AS additional\n" 
         "JOIN {0}.entity USING (enttype)"
         ).format(schema)
    )


def process_row(row):
    """
    Outputs a sql insert values for a row from the additional table.
    Contains all logic to split a row with data values in wide to long format.
    Also filters data values without information.
    """
    patid = row[0]
    enttype = row[1]
    adid = row[2]
    n_data_columns = int(row[3])

    data_values = row[4:4+MAX_DATA_COLUMNS]
    data_names = row[4+MAX_DATA_COLUMNS:4+MAX_DATA_COLUMNS*2]
    data_lookups = row[4+MAX_DATA_COLUMNS*2:]

    # Special case for score
    if enttype == 372:
        try:
            value = create_score_value(patid, adid, enttype, data_values, data_names)
            return [value]
        except Exception as error:
            print(error, end='. ')
            print("The value is skipped")

        raise StopIteration

    sql_insert_values = []
    for i in range(n_data_columns):
        # If next column is a unit, add that to this value. Unit is skipped in next iteration
        unit_code = None
        if i+1 < n_data_columns and data_lookups[i+1] == 'SUM':
            unit_code = data_values[i+1]

        try:
            value = create_value(
                patid,
                adid,
                str(enttype) + '-' + str(i + 1),
                data_values[i],
                data_names[i],
                data_lookups[i],
                unit_code
            )
        except Exception as error:
            # TODO: capture this message also in the log file
            print(error, end='. ')
            print("The value is skipped")
            continue

        if value:
            sql_insert_values.append(value)

    return sql_insert_values


def create_score_value(patid, adid, enttype, data_values, data_names):
    # Nones to '0'
    scoring_method = str(data_values[2] or '0')
    condition = str(data_values[1] or '0')

    # Concatenate enttype-data2-data3
    enttype_string = '-'.join([str(enttype), scoring_method, condition])

    data_numeric = data_values[0]
    data_name = data_names[0]

    return sql_create_values_string(
        sql_numeric_param(patid),
        sql_numeric_param(adid),
        sql_string_param(enttype_string),
        sql_string_param(data_name),
        sql_numeric_param(data_numeric),
        sql_numeric_param(None),
        sql_date_param(None),
        sql_string_param(None),
        sql_string_param(None)
    )


def create_value(patid, adid, enttype_string, data_value, data_name, data_lookup, unit_code):
    # Skip if data field is a lookup and value translates to 'Data not entered'
    if data_lookup and data_value == '0':
        return

    # Skip unit data
    if data_lookup == 'SUM':
        return

    # Depending on lookup, the data value can be numeric, a code or a date
    data_date = None
    data_code = None
    data_numeric = None
    if not data_lookup:
        data_numeric = data_value
    elif data_lookup == 'dd/mm/yyyy':
        # support two date formats
        if not data_value:
            data_date = None
        elif '/' in data_value:
            data_date = datetime.strptime(data_value, '%d/%m/%Y')
        else:
            data_date = datetime.strptime(data_value, '%Y%m%d')
    else:
        # All other values with a lookup
        data_code = data_value

    # Exclude unit lookup code '0'
    if unit_code == '0':
        unit_code = None

    return sql_create_values_string(
        sql_numeric_param(patid),
        sql_numeric_param(adid),
        sql_string_param(enttype_string),
        sql_string_param(data_name),
        sql_numeric_param(data_numeric),
        sql_string_param(data_code),
        sql_date_param(data_date),
        sql_string_param(data_lookup),
        sql_string_param(unit_code)
    )


def process_additional(connection, source_schema, target_schema, target_table='additional_intermediate'):
    """
    Takes additional table and inserts rows per data field.
    """

    # Create the intermediate table
    create_intermediate_table(connection, target_schema, target_table)

    # This is slow if ETL run remotely (data has to be send from server to local environment)
    additional_table = get_additional(connection, source_schema)

    # Memory and processor usage param
    BLOCK_SIZE = 50000
    PROCESSES = 4
    SQL_INSERT_BASE = "INSERT INTO {0}.{1} " \
                      "(patid, adid, enttype_string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code) " \
                      "VALUES".format(target_schema, target_table)

    # Process per block. Create values and do a batch insert.
    total_rows_inserted = 0
    pool = Pool(processes=PROCESSES)
    while True:
        # Load a part of the result in memory
        rows = additional_table.fetchmany(BLOCK_SIZE)

        # Perform the row processing in parallel
        # Note: process_row has to be a simple function, it cannot be a class method
        result = pool.map(process_row, rows)
        sql_insert_values = [value for values in result for value in values]

        sql_insert = SQL_INSERT_BASE + ' ' + ','.join(sql_insert_values)
        insert_result = connection.execute(text(sql_insert).execution_options(autocommit=True))

        total_rows_inserted += insert_result.rowcount

        # Stop condition if all rows retrieved
        if len(rows) < BLOCK_SIZE:
            break

    return total_rows_inserted
