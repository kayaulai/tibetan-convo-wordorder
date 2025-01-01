#!/bin/bash
for DIR in "data/01a_r_tables" "data/01b_manual_tables" "data/01c_rezrDF"; do
    find "$DIR" -type f -name '*- Copy*' -exec rm {} \;
done