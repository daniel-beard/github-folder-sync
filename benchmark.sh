#!/bin/bash

set -xeuo pipefail

# Burn old folder
test -d "../folder-sync" && {
  rm -rf ../folder-sync
}

# Create the folder again
mkdir ../folder-sync

# Copy our config file (from the folder above us)
# Setup this way for now to avoid me leaking any URLs or creds.
cp ../config.dhall ../folder-sync

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

# concurrentMapM_ 8 10 - enterprise test 1 - all repos
# real	11m51.088s
# user	9m31.721s
# sys	2m39.496s

# concurrentMapM_ 8 10 - enterprise test 1 - ignoring 2 large repos
# real	2m11.017s
# user	1m25.165s
# sys	0m37.500s
