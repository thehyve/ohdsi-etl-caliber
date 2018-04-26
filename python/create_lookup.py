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
import csv
import sys
from os import linesep, path, listdir

import click


def write_output(result, output_file):
    if output_file:
        is_first = False
        if not path.exists(output_file):
            with open(output_file, 'w') as out:
                out.write("CREATE TABLE public.cprd_lookup (lookup_type char(3), lookup_name text, code integer, description text);")
                out.write(linesep*2)
                out.write("INSERT INTO public.cprd_lookup VALUES")
                out.write(linesep)
            is_first = True

        with open(output_file, 'a') as out:
            for line in result:
                if not is_first:
                    out.write(",")
                out.write("(" + ",".join(line) + ")" + linesep)
                is_first = False
    else:
        print('\n'.join([','.join(x) for x in result]))


def check_type(value):
    try:
        int(value)
    except ValueError:
        value = '\'' + value + '\''
    return value


def process_file(file_path, source, output_file):
    # result = ['CASE ' + source]
    result = []

    lookup_name = path.basename(file_path).split('.')[0]

    with open(file_path, 'r') as lookup:
        reader = csv.reader(lookup, delimiter='\t')
        try:
            header = next(reader)
            print('Header:', '\t'.join(header) + linesep)  # Show header
        except StopIteration:
            print('File has no lines:', file_path)

        for row in reader:
            if len(row) == 0:  # Skip blank lines
                continue
            elif len(row) != 2:
                print('Unexpected number of values: {0}'.format(row))
                print('\nRow will be ignored')
            source_value = check_type(row[0])
            new_value = check_type(row[1])
            # new_line = 'WHEN ' + source_value + ' THEN ' + new_value
            if new_value == "''":
                new_value = ""

            new_line = ["'%s'" % lookup_name, "'%s'" % header[1], source_value, "'%s'" % new_value.replace("''", '"').strip("'").replace("'", "''").replace('"', "''")]
            result.append(new_line)
    # result.append('END')

    write_output(result, output_file)


@click.command()
@click.argument('folder_path', nargs=1, type=click.Path(exists=True, dir_okay=True), metavar='<path>')
@click.option('--source', '-s', default='<source>', metavar='<str>',
              help='The (alias) name of the source column that requires translation')
@click.option('--output_file', '-o', default=None, metavar='<path>',
              help='Write output to file instead of print to console')
def main(folder_path, source, output_file):
    """This script creates formatted sql code for CASE translations."""
    print(linesep + 'file_path:', folder_path)
    print('Source column:', source)

    for file_name in listdir(folder_path):
        if not file_name.endswith('.txt'):
            continue

        file_path = path.join(folder_path, file_name)
        process_file(file_path, source, output_file)

if __name__ == "__main__":
    sys.exit(main())
