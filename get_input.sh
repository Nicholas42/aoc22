#!/bin/sh

set -ex

DAY=${1:-$(date +%d)}
DAY_UNPADDED=$(echo $DAY | sed 's/^0//')
FNAME="inputs/dec$DAY.txt"

mkdir -p inputs
wget -o /dev/null --load-cookies aoc_cookie.txt https://adventofcode.com/2022/day/$DAY_UNPADDED/input -O - > $FNAME
