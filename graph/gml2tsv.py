#!/bin/python

# small file that turns GML files into tsv files
# gml graph description language.

import networkx as nx
import sys
g = nx.read_gml(sys.argv[1])

nodes = [n for n in g]
names = [str(n) for n in nodes]
print("\t".join(names))
for i in nodes:
    adj = ["1" if g.has_edge(i, j) else "0" for j in nodes]
    print("\t".join(adj))
