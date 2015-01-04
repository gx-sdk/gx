#!/bin/sh

cd $(git rev-parse --show-toplevel || exit 1)
cargo doc || exit 1
chmod -R a+rX target/doc || exit 1
rsync -a --info=progress2 target/doc/ gx@gx:gx/target/doc
