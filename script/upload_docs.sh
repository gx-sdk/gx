#!/bin/sh

cd $(git rev-parse --show-toplevel || exit 1)
cargo doc || exit 1
find target/doc -print0 | xargs -0 chmod a+r || exit 1
find target/doc -type d -print0 | xargs -0 chmod a+x || exit 1
rsync -aP target/doc/ gx@gx:gx/target/doc
