#!/bin/bash

SCHEMAS='atlas_public_transport atlas_fmd_wrapper atlas_app atlas_registry'

for schema in $SCHEMAS ; do
  diff $schema.old $schema.new | awk '(NR>1)' | cut -d ' ' -f 2 > $schema.diff
done

