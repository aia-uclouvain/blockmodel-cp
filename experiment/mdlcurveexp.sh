#!/bin/bash
declare -a files=("graph/net/tinamatr.net.tsv" "graph/net/Ragusa18.net.tsv" "graph/net/Ragusa16.net.tsv" "graph/karate/karate.gml.tsv" "graph/polbooks/polbooks.gml.tsv" "graph/lesmis/lesmis.gml.tsv" "graph/football/football.gml.tsv" "graph/dolphins/dolphins.gml.tsv" "graph/celegansneural/celegansneural.gml.tsv" "graph/PoliticalActor.tsv" "graph/LittleLeagueSharpstone.tsv" "graph/LittleLeaguetransatlantic.tsv")

java -jar mdlcurve.jar ../${files[$1]} 10 ../${files[$1]}.data
