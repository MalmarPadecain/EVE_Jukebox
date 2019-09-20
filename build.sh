#!/bin/bash

elm make src/Main.elm --optimize --output=main.js

tar -czf Jukebox_full.tar.gz jukebox.html src/controls.js main.js playlists/ css images video music LICENSE README.md
tar -czf Jukebox.tar.gz jukebox.html src/controls.js main.js playlists/ css LICENSE README.md
