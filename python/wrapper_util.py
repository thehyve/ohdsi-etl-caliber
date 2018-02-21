import os
import re


def create_insert_message(sql_command, row_count, execution_time=None):
    """ Create message on how many lines inserted into which table """
    if row_count >= 0:
        table_into = '?'

        # NOTE: if multiple queries, then rowcount only last number of inserted/updated rows
        match_into = re.search(r'INTO (.+?)\s', sql_command)
        if match_into:
            table_into = match_into.group(1)

        return create_message(table_into, row_count, execution_time)

    return create_message(None, row_count, execution_time)


def create_message(table_into, row_count, execution_time):
    if table_into:
        table_into = 'Into ' + table_into
    else:
        table_into = 'Nothing inserted'

    message = '{:<40.40} {:>9,} [{:>8.2f} s'.format(table_into, row_count, execution_time)

    if row_count > 0:
        message += '| {:>.1e} s/#]'.format(execution_time/row_count)
    else:
        message += ']'

    return message


def create_current_file_message(filename):
    basename = os.path.basename(filename)
    return "{:<30.30} => ".format(basename)
