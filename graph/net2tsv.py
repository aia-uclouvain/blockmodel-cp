#!/bin/python

# small file that turns pajek .net files into tsv files

import networkx as nx
import sys
g = nx.read_pajek(sys.argv[1])

nodes = [n for n in g]
names = [str(n) for n in nodes]
print("\t".join(names))
for i in nodes:
    adj = ["1" if g.has_edge(i, j) else "0" for j in nodes]
    print("\t".join(adj))
