#!/bin/bash

ROOTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"

ADLC=$ROOTDIR/scripts/adlc

cd $ROOTDIR

$ADLC haskell \
  --outputdir haskell/src \
  --package ADL \
  --rtpackage ADL.Core \
  --include-rt \
  --searchdir adl \
  adl/*.adl
  
  
