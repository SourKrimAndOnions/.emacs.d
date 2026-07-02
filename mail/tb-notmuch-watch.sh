#!/usr/bin/env bash
# Watch Thunderbird/Owl's mail store and re-mirror + re-index on every change.
# Blocks on inotifywait, settles briefly, then syncs; write-bursts during a
# sync collapse into a single follow-up wakeup (natural debounce).
set -uo pipefail

SYNC="$HOME/.local/bin/tb-mbox-sync.py"
SRC="$(python3 "$SYNC" --print-watch-dir)" || { echo "cannot locate mail store"; exit 1; }

run() {
  python3 "$SYNC" >/dev/null 2>&1
  notmuch new >/dev/null 2>&1
}

run   # catch up anything missed while the watcher was down

while true; do
  inotifywait -r -q -q \
    -e close_write -e moved_to -e delete \
    --exclude '\.(msf|dat|tmp)$' \
    "$SRC" >/dev/null 2>&1 || { sleep 5; continue; }
  sleep 3
  run
done
