#!/bin/zsh

APP_NAME="OsmosePresets_macOS_aarch64"
APP_MENU_NAME="Osmose Presets"
APP_BUNDLE="${APP_NAME}.app"
CONTENTS_DIR="${APP_BUNDLE}/Contents"
FRAMEWORKS_DIR="${CONTENTS_DIR}/Frameworks"
RESOURCES_DIR="${CONTENTS_DIR}/Resources"
MACOS_DIR="${CONTENTS_DIR}/MacOS"

rm -rf "${APP_BUNDLE}"

# Create necessary directories
mkdir -p "${MACOS_DIR}"
mkdir -p "${FRAMEWORKS_DIR}" "${RESOURCES_DIR}"

# Copy executable
echo "Copying executable to ${MACOS_DIR}/${APP_NAME}"
cp "${APP_NAME}" "${MACOS_DIR}/${APP_NAME}" 2>/dev/null

# Copy resources (modify as needed)
# cp -R resources/* "${RESOURCES_DIR}/"

# Copy and fix dependencies
LIBS=(${(f)"$(otool -L "${MACOS_DIR}/${APP_NAME}" | grep -v /System | grep -v /usr/lib | grep -v @executable_path | awk '{print $1}')"})

for lib in $LIBS; do
    cp "${lib}" "${FRAMEWORKS_DIR}/" 2>/dev/null
    lib_name=${lib:t}  # zsh way to get basename
    install_name_tool -change "${lib}" "@executable_path/../Frameworks/${lib_name}" "${MACOS_DIR}/${APP_NAME}"
done

# Update Info.plist (modify as needed)
cat > "${CONTENTS_DIR}/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>${APP_NAME}</string>
    <key>CFBundleIdentifier</key>
    <string>com.yourcompany.${APP_NAME}</string>
    <key>CFBundleName</key>
    <string>${APP_MENU_NAME}</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleSignature</key>
    <string>????</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.12</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>
EOF
