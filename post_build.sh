#!/bin/bash

# Run the first script
./build_app_bundle.sh

# Check if the first script succeeded
if [ $? -eq 0 ]; then
  # Run the second script
  ./build_app_version.sh
else
  echo "Error: build_app_bundle.sh failed. Aborting."
  exit 1
fi
