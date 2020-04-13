#!/bin/bash

ROOTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"

ADLC=$ROOTDIR/scripts/adlc

cd $ROOTDIR

$ADLC haskell \
  --searchdir adl \
  --outputdir haskell/src \
  --manifest haskell/src/.adl-manifest \
  --package ADL \
  --rtpackage ADL.Core \
  --include-rt \
  adl/*.adl
  
$ADLC typescript \
  --searchdir adl \
  --outputdir typescript/src/adl \
  --manifest typescript/src/.adl-manifest \
  --include-rt \
  --include-resolver \
  --runtime-dir runtime \
  --generate-transitive \
  adl/*.adl
  
