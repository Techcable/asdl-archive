#!/bin/sh
suifdriver <<EOF
import suifnodes
import zsuif_gateway
load $1
zsuif_gateway $2
EOF
