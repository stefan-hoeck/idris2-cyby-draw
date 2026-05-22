#!/usr/bin/env sh
./makecss.sh
pack --extra-args "--directive minimal" build cyby-draw-app
cp app/build/exec/cyby-draw-app.js draw/
