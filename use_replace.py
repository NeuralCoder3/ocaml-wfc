from asyncore import file_dispatcher
import os
import sys
import optparse
import re

# call python use_replace.py main_file output_file
# use positional arguments

optparser = optparse.OptionParser()
optparser.add_option("-f", "--file", dest="filename", default="", help="read data from FILENAME")
optparser.add_option("-o", "--output", dest="output", default="", help="write output to FILENAME")

options, args = optparser.parse_args()

input_file=options.filename
output_file=options.output

print("input file: " + input_file)
print("output file: " + output_file)

with open(input_file, 'r') as f:
    data = f.read()

replace_pattern_str = "#use \"(.*?)\""
replace_pattern = re.compile(replace_pattern_str)

while True:
    matche = replace_pattern.search(data)
    if matche is None:
        break
    else:
        with open(matche.group(1), 'r') as f:
            data = data.replace(matche.group(0), f.read())

with open(output_file, 'w') as f:
    f.write(data)
