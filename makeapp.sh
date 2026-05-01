#!/usr/bin/env sh
./makecss.sh
pack build cyby-draw-app
cp app/build/exec/cyby-draw-app.js draw/
