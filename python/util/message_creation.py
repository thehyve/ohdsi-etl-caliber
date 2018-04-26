import os
import re


def create_current_file_message(filename):
    basename = os.path.basename(filename)
    return "{:<30.30} => ".format(basename)


def create_insert_message(sql_query, row_count, execution_time=None):
    """ Create message on how many lines inserted into which table """
    if row_count >= 0:
        # NOTE: if multiple queries, then rowcount only last number of inserted/updated rows
        match = re.search(r'^\s*((?:INSERT )?INTO|CREATE TABLE|DELETE\s+FROM|UPDATE)\s+(.+?)\s',
                          sql_query,
                          re.IGNORECASE | re.MULTILINE
                          )
        if match:
            statement = match.group(1).upper()
            table_into = match.group(2)
        else:
            statement = ''
            table_into = '?'

        if statement == 'INTO':
            prefix = 'INTO'
        elif 'DELETE' in statement:
            prefix = 'DELETE'
        elif 'CREATE' in statement:
            prefix = 'CREATE'
        elif 'UPDATE' in statement:
            prefix = 'UPDATE'
        else:
            prefix = ''

        return create_message(table_into, row_count, execution_time, prefix)

    return create_message(None, row_count, execution_time)


def create_message(table_into, row_count, execution_time, prefix='INTO'):
    if table_into:
        table_into_message = prefix + ' ' + table_into
    else:
        table_into_message = 'Nothing inserted'

    message = '{:<40.40} {:>9,} [{:>8.2f} s'.format(table_into_message, row_count, execution_time)

    if row_count > 0:
        message += '| {:>.1e} s/#]'.format(execution_time/row_count)
    else:
        message += ']'

    return message
