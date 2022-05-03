#!/bin/bash

set -euo pipefail

# Burn old folder
test -d "../folder-sync" && {
  rm -rf ../folder-sync
}

# Create the folder again
mkdir ../folder-sync

# Copy our config file
cp config.dhall ../folder-sync

# Run
time stack run

# mapM_ -> 
  # real    1m17.786s
  # user    0m5.237s
  # sys     0m3.131s

# concurrentMapM_
# real    0m20.179s
# user    0m4.287s
# sys     0m2.253s