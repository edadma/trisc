#!/bin/bash
# Build the Scala.js output and copy to npm package
set -e
cd "$(dirname "$0")/.."
sbt syslCliJS/fullLinkJS
mkdir -p npm/lib
cp sysl-cli/js/target/scala-3.8.2/sysl-cli-opt/main.js npm/lib/main.js
echo "Built npm/lib/main.js"
ls -lh npm/lib/main.js
