#!/usr/bin/env sh
mkdir -p draw
APP_CSS="draw/app.css"
curl https://cdn.jsdelivr.net/npm/modern-normalize/modern-normalize.min.css > "$APP_CSS"
pack --log-level silence exec cyby-css/src/CyBy/UI/CSS/Rules.idr >> "$APP_CSS"
pack build cyby-draw-app
cp app/build/exec/cyby-draw-app.js draw/
