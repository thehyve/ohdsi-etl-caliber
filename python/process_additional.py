from datetime import datetime
from sqlalchemy import text


def sql_string_param(s):
    if s is None:
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
               "    data_value INTEGER,"
               "    data_code INTEGER,"
               "    data_date DATE,"
               "    lookup_type VARCHAR,"
               "    unit_code VARCHAR"
               ")").format(schema, table_name)
    connection.execute(ddl_sql)


def get_additional(connection, schema):
    #TODO, replace 'caliber_real' by schema parameter
    return connection.execute(
        ("SELECT patid,enttype,adid,"
         "additional.data1,additional.data2,additional.data3,"
         "additional.data4,additional.data5,additional.data6,additional.data7,"
         "entity.data1,entity.data2,entity.data3,"
         "entity.data4,entity.data5,entity.data6,entity.data7,"
         "entity.data1_lkup,entity.data2_lkup,entity.data3_lkup,"
         "entity.data4_lkup,entity.data5_lkup,entity.data6_lkup,entity.data7_lkup\n"
         "FROM caliber_real.additional AS additional\n" 
         "JOIN {0}.entity USING (enttype)"
         ).format(schema)
    )


def process_row(row):
    """
    Contains all logic to split a row with data values in wide to long format.
    Also filters data values without information.
    """
    # TODO: create unit tests

    N_DATA_COLUMNS = 7
    patid = row[0]
    enttype = row[1]
    adid = row[2]
    data_values = row[3:3+N_DATA_COLUMNS]
    data_names = row[3+N_DATA_COLUMNS:3+N_DATA_COLUMNS*2]
    data_lookups = row[3+N_DATA_COLUMNS*2:]
    for i in range(N_DATA_COLUMNS):
        data_value = data_values[i]
        data_name = data_names[i]
        data_lookup = data_lookups[i]

        # Empty data value, skip
        if data_value is None:
            continue

        # data field is a lookup and value translates to 'Data not entered'
        if data_lookup is not None and data_value == '0':
            continue

        # Skip units data
        if data_lookup == 'SUM':
            continue

        # Dependig on lookup, the data value can be numeric, a code or a date
        data_date = None
        data_code = None
        data_numeric = None
        if data_lookup is None:
            data_numeric = data_value
        elif data_lookup == 'dd/mm/yyyy':
            data_date = datetime.strptime(data_value, '%d/%m/%Y')
        else:
            # All other values with a lookup
            data_code = data_value

        # If next column is a unit, add that to this value. Unit is skipped in next iteration
        # Exclude unit of zero
        unit_code = None
        if i+1 < N_DATA_COLUMNS and data_lookups[i+1] == 'SUM' and str(data_values[i+1]) != '0':
            unit_code = data_values[i+1]

        enttype_string = str(enttype) + '-' + str(i + 1)

        sql_value = sql_create_values_string(
            sql_numeric_param(patid),
            sql_numeric_param(adid),
            sql_string_param(enttype_string),
            sql_string_param(data_name),
            sql_numeric_param(data_numeric),
            sql_numeric_param(data_code),
            sql_date_param(data_date),
            sql_string_param(data_lookup),
            sql_string_param(unit_code)
        )
        yield sql_value


def process_additional(connection, target_table='additional_intermediate', source_schema='caliber', target_schema='public'):
    """
    Takes additional table and generates rows per data field.
    Yields AdditionalIntermediate ORM objects
    """

    # Create the intermediate table
    create_intermediate_table(connection, target_schema, target_table)

    #
    additional_table = get_additional(connection, source_schema)

    BLOCK_SIZE = 50000
    SQL_INSERT_BASE = "INSERT INTO {0}.{1} " \
                      "(patid, adid, enttype_string, datafield_name, data_value, data_code, data_date, lookup_type, unit_code) " \
                      "VALUES".format(target_schema, target_table)

    total_rows_inserted = 0
    while True:
        # Load 1000 in memory
        sql_insert_values = []
        rows = additional_table.fetchmany(BLOCK_SIZE)
        for row in rows:
            for value in process_row(row):
                sql_insert_values.append(value)
                total_rows_inserted += 1

        sql_insert = SQL_INSERT_BASE + ' ' + ','.join(sql_insert_values)
        insert_result = connection.execute(text(sql_insert))
        print('Inserted:', insert_result.rowcount)

        # Stop condition if all rows retrieved
        if len(rows) < BLOCK_SIZE:
            break

    return total_rows_inserted
