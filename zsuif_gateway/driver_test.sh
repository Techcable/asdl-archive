#!/bin/sh
suifdriver <<EOF
import basicnodes
import suifnodes
import cfenodes
import zsuif_gateway
load $1
zsuif_gateway $2
EOF
