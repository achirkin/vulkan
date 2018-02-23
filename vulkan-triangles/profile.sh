#!/bin/bash
#
# # profile this program
#
# Requires ps2pdf program that is not shipped together with stack's GHC.
#
# You can provide some +RTS arguments, e.g.
#
#  -p -hc -i1.0
#  -p -hy -i1.0
#

PROGNAME=vulkan-triangles

rm -f ${PROGNAME}.ps ${PROGNAME}.hp ${PROGNAME}.aux ${PROGNAME}.prof
stack clean
stack build --profile --ghc-options="-fprof-auto -rtsopts -fprof-cafs -O2"
time stack exec ${PROGNAME} -- +RTS ${@:--p -hc -i1.0}
stack exec hp2ps -- -c ${PROGNAME}.hp
ps2pdf ${PROGNAME}.ps
rm -f ${PROGNAME}.ps ${PROGNAME}.hp ${PROGNAME}.aux
