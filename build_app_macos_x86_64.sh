#!/bin/bash

# Run the first script
./build_app_bundle_x86_64.sh

# Check if the first script succeeded
if [ $? -eq 0 ]; then
  # Run the second script
  ./build_app_version_macos_x86_64.sh
else
  echo "Error: build_app_bundle_x86_64.sh failed. Aborting."
  exit 1
fi
