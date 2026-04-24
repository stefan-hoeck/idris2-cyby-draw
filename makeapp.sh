#!/usr/bin/env bash
curl https://raw.githubusercontent.com/sindresorhus/modern-normalize/refs/heads/main/modern-normalize.css > css/app.css
cat css/cyby-draw.css >> css/app.css
pack build cyby-draw-app
