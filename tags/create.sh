#!/bin/sh

## This script generates a new tag page.

mkdir -p $1/feed/
cat > $1/index.html <<EOF
---
title: Posts tagged $1
tag: $1
layout: tag
---
EOF

cat > $1/feed/index.xml <<EOF
---
tag: $1
layout: tagfeed
uuid: $(uuidgen)
---
EOF
