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


def create_intermediate_table(connection, schema, table_name):
    ddl_sql = ("DROP TABLE IF EXISTS {0}.{1};"
               "CREATE TABLE {0}.{1} ("
               "    patid INTEGER,"
               "    adid INTEGER,"
               "    enttype_string VARCHAR,"
               "    datafield_name VARCHAR,"
               "    data_value INTEGER,"
               "    data_date DATE,"
               "    lookup_type VARCHAR"
               ")").format(schema, table_name)
    connection.execute(ddl_sql)


def get_additional(connection, schema):
    return connection.execute(
        ("SELECT patid,enttype,adid,"
         "additional.data1,additional.data2,additional.data3,"
         "additional.data4,additional.data5,additional.data6,additional.data7,"
         "entity.data1,entity.data2,entity.data3,"
         "entity.data4,entity.data5,entity.data6,entity.data7,"
         "entity.data1_lkup,entity.data2_lkup,entity.data3_lkup,"
         "entity.data4_lkup,entity.data5_lkup,entity.data6_lkup,entity.data7_lkup\n"
         "FROM caliber_real.additional AS additional\n" #TODO, replace by schema
         "JOIN {0}.entity USING (enttype)"
         ).format(schema)
    )


def process_row(row):
    patid = row[0]
    enttype = row[1]
    adid = row[2]
    data_values = row[3:10]
    data_names = row[10:17]
    data_lookups = row[17:]
    for i in range(7):
        data_value = data_values[i]
        data_name = data_names[i]
        data_lookup = data_lookups[i]

        # Empty data value, skip
        if data_value is None:
            continue

        # data field is a lookup and value translates to 'Data not entered'
        if data_lookup is not None and data_value == '0':
            continue

        # If it is a date, insert
        data_date = None
        if data_lookup == 'dd/mm/yyyy':
            data_date = datetime.strptime(data_value, '%d/%m/%Y')
            data_value = None

        enttype_string = str(enttype) + '-' + str(i + 1)

        sql_value = "({},{},{},{},{},{},{})".format(
            sql_numeric_param(patid),
            sql_numeric_param(adid),
            sql_string_param(enttype_string),
            sql_string_param(data_name),
            sql_numeric_param(data_value),
            sql_date_param(data_date),
            sql_string_param(data_lookup)
        )
        yield sql_value


def process_additional(connection, source_schema='caliber', target_schema='public'):
    """
    Takes additional table and generates rows per data field.
    Yields AdditionalIntermediate ORM objects
    """
    TARGET_TABLE = 'additional_intermediate'

    # Create the intermediate table
    create_intermediate_table(connection, target_schema, TARGET_TABLE)

    #
    additional_table = get_additional(connection, source_schema)

    BLOCK_SIZE = 10000
    SQL_INSERT_BASE = "INSERT INTO {0}.{1} " \
                      "(patid, adid, enttype_string, datafield_name, data_value, data_date, lookup_type) " \
                      "VALUES".format(target_schema, TARGET_TABLE)

    while True:
        # Load 1000 in memory
        sql_insert_values = []
        rows = additional_table.fetchmany(BLOCK_SIZE)
        for row in rows:
            for value in process_row(row):
                sql_insert_values.append(value)

        sql_insert = SQL_INSERT_BASE + ' ' + ','.join(sql_insert_values)
        insert_result = connection.execute(text(sql_insert))
        print('Inserted:', insert_result.rowcount)

        # Stop condition if all rows retrieved
        if len(rows) < BLOCK_SIZE:
            break
