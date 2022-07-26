
import os
import sys
import re

graph="""
digraph G {
"""

filePattern = ".ml"
replace_pattern_str = "#use \"(.*?).ml\""
replace_pattern = re.compile(replace_pattern_str)
    
for f in os.listdir(os.getcwd()):
    if not os.path.isfile(f):
        continue
    if not f.endswith(filePattern):
        continue
    name = f[:-len(filePattern)]
    with open(f, 'r') as f:
        data = f.read()
    matches = re.findall(replace_pattern, data)
    for match in matches:
        graph += '"' + match + '" -> "' + name + '";\n'

graph+="}"

with open("graph.dot", 'w') as f:
    f.write(graph)

os.system("dot -Tpng graph.dot -o graph.png")