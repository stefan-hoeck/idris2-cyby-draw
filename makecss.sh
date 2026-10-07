#!/usr/bin/env sh
mkdir -p draw
APP_CSS="draw/app.css"
pack --log-level silence exec cyby-css/src/CyBy/UI/CSS/Rules.idr >> "$APP_CSS"
