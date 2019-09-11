#!/bin/bash

cd "$( dirname "${BASH_SOURCE[0]}" )/.."

adlc haskell \
  --outputdir src \
  --package ADL \
  --rtpackage ADL.Core \
  --include-rt \
  --searchdir adl \
  adl/*.adl
  
  
