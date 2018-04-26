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
from os import linesep

import click


def write_output(result, output_file):
    if output_file:
        with open(output_file, 'w') as out:
            for line in result:
                out.write(line + linesep)
    else:
        print('\n'.join(result))


def check_type(value):
    try:
        int(value)
    except ValueError:
        value = '\'' + value + '\''
    return value


def process_file(file_path, source, output_file):
    result = ['CASE ' + source]

    with open(file_path, 'r') as lookup:
        reader = csv.reader(lookup, delimiter='\t')
        print('Header:', '\t'.join(next(reader)) + linesep)  # Show header
        for row in reader:
            if len(row) == 0:  # Skip blank lines
                continue
            elif len(row) != 2:
                print('Unexpected number of values: {0}'.format(row))
                print('\nRow will be ignored')
            source_value = check_type(row[0])
            new_value = check_type(row[1])
            new_line = 'WHEN ' + source_value + ' THEN ' + new_value
            result.append(new_line)
    result.append('END')

    write_output(result, output_file)


@click.command()
@click.argument('file_path', nargs=1, type=click.Path(exists=True, dir_okay=False), metavar='<path>')
@click.option('--source', '-s', default='<source>', metavar='<str>',
              help='The (alias) name of the source column that requires translation')
@click.option('--output_file', '-o', default=None, metavar='<path>',
              help='Write output to file instead of print to console')
def main(file_path, source, output_file):
    """This script creates formatted sql code for CASE translations."""
    print(linesep + 'file_path:', file_path)
    print('Source column:', source)

    process_file(file_path, source, output_file)


if __name__ == "__main__":
    sys.exit(main())
