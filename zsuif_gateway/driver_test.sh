#!/bin/sh
suifdriver <<EOF
import basicnodes
import suifnodes
import cfenodes
import zsuif_gateway
import usefulpasses
load $1
PaddingPass
zsuif_gateway $2
EOF
