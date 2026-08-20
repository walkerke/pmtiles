#!/bin/bash
# Script to build pre-compiled binaries for all platforms
# Run this before submitting to CRAN

set -e

# In a fresh clone the go-pmtiles submodule is empty; fetch it first
if [ ! -f inst/go-pmtiles/go.mod ]; then
    echo "Initializing go-pmtiles submodule..."
    git submodule update --init inst/go-pmtiles
fi

cd inst/go-pmtiles

# Stamp the binaries with the checked-out go-pmtiles version
VERSION=$(git describe --tags 2>/dev/null || echo "dev")
echo "go-pmtiles version: $VERSION"

# Define platforms
PLATFORMS=(
    "darwin/amd64"
    "darwin/arm64"
    "linux/amd64"
    "linux/arm64"
    "windows/amd64"
    "windows/arm64"
)

echo "Building pmtiles binaries for all platforms..."

for platform in "${PLATFORMS[@]}"; do
    IFS='/' read -r -a parts <<< "$platform"
    GOOS="${parts[0]}"
    GOARCH="${parts[1]}"

    OUTPUT_DIR="../bin/${GOOS}_${GOARCH}"
    mkdir -p "$OUTPUT_DIR"

    BINARY_NAME="pmtiles"
    if [ "$GOOS" = "windows" ]; then
        BINARY_NAME="pmtiles.exe"
    fi

    OUTPUT_PATH="$OUTPUT_DIR/$BINARY_NAME"

    echo "Building for $GOOS/$GOARCH..."
    GOOS=$GOOS GOARCH=$GOARCH go build -ldflags="-s -w -X main.version=$VERSION" -o "$OUTPUT_PATH" main.go

    if [ $? -eq 0 ]; then
        echo "  ✓ Successfully built $OUTPUT_PATH"
    else
        echo "  ✗ Failed to build for $GOOS/$GOARCH"
        exit 1
    fi
done

cd ../..

echo ""
echo "All binaries built successfully!"
echo "Binary locations:"
find inst/bin -type f -name "pmtiles*"
