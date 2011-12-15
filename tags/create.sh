#!/bin/bash

## Create a new tag page

mkdir $1
cat > $1/index.html <<EOF
---
title: $1
layout: tag
---
EOF
