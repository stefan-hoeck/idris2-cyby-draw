#!/usr/bin/env sh
mkdir -p draw
MN_CSS="draw/modern_normalize.css"
APP_CSS="draw/app.css"
cp -v "$MN_CSS" "$APP_CSS"
pack --log-level silence exec cyby-css/src/CyBy/UI/CSS/Rules.idr >> "$APP_CSS"
