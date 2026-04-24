#!/usr/bin/env sh
APP_CSS="css/app.css"
curl https://raw.githubusercontent.com/sindresorhus/modern-normalize/refs/heads/main/modern-normalize.css > "$APP_CSS"
pack --log-level silence exec cyby-css/src/CyBy/UI/CSS/Rules.idr >> "$APP_CSS"
cat css/cyby-draw.css >> "$APP_CSS"
pack build cyby-draw-app
