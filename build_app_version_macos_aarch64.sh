#!/bin/bash

# Get the version number from the Lazarus project options using xmllint
VERSION_MAJOR="0"  # Default value since MajorVersionNr is not present in the .lpi file
VERSION_MINOR=$(xmllint --xpath 'string(//VersionInfo/MinorVersionNr/@Value)' *.lpi)
VERSION_RELEASE=$(xmllint --xpath 'string(//VersionInfo/RevisionNr/@Value)' *.lpi)
VERSION_BUILD=$(xmllint --xpath 'string(//VersionInfo/BuildNr/@Value)' *.lpi)

VERSION="${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_RELEASE}.${VERSION_BUILD}"

# Define the original and new executable names
ORIGINAL_EXECUTABLE="OsmosePresets_macOS_aarch64"  # Replace with your actual executable name
NEW_EXECUTABLE="${ORIGINAL_EXECUTABLE}_${VERSION}"

# Rename the compiled executable
if [ -f "${ORIGINAL_EXECUTABLE}" ]; then
    mv "${ORIGINAL_EXECUTABLE}.app" "${NEW_EXECUTABLE}.app"
    echo "Executable renamed to ${NEW_EXECUTABLE}.app"
else
    echo "Error: Original executable not found."
    exit 1
fi
