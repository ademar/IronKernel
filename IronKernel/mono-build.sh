#!/usr/bin/env bash
# Legacy helper — prefer: dotnet build
set -euo pipefail
cd "$(dirname "$0")/.."
dotnet build
