#!/bin/sh

${SML_110_27:?"Environment variable SML_110_27 unset. Set it to point to the build directory of SML/NJ v.110.27 or higher."}/bin/sml < build.sml
