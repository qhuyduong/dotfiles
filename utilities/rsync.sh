#!/bin/bash

# Function to sync files using rsync
sync_files() {
    rsync -avz xnu --delete qhuyduong@10.0.2.16:~/xnu-build/
}

# Watch for file changes using inotifywait
inotifywait -m -r -e modify,create,delete,move xnu | while read -r directory event file
do
    echo "Change detected: $directory$file - $event"
    sync_files
done

