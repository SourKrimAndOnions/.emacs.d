#!/usr/bin/env python3
"""Mirror Thunderbird/Owl mbox folders into a Maildir tree for notmuch.

Read-only on the Thunderbird side: it never modifies Owl's mbox files.
Incremental + idempotent (dedupes by Message-ID, skips unchanged folders).
Read/unread state is carried across via Thunderbird's X-Mozilla-Status flag.

Portable: auto-detects the Thunderbird profile and the Owl account directory,
and writes everything under $HOME. Override detection with TB_PROFILE=/path.
`--print-watch-dir` prints the directory a file watcher should monitor.
"""

import argparse
import configparser
import email
import glob
import hashlib
import json
import mailbox
import os
import sys

HOME = os.path.expanduser("~")
DEST = os.path.join(HOME, "Mail")
STATE = os.path.join(HOME, ".local/share/tb-notmuch/state.json")

# Thunderbird stores per-message flags in this header (hex). Bit 0 = Read.
MOZ_STATUS_READ = 0x0001


def thunderbird_profile():
    """Locate the default Thunderbird profile directory, portably."""
    override = os.environ.get("TB_PROFILE")
    if override:
        return os.path.expanduser(override)
    base = os.path.join(HOME, ".thunderbird")
    ini = os.path.join(base, "profiles.ini")
    if os.path.exists(ini):
        cp = configparser.ConfigParser()
        cp.read(ini)
        # [InstallXXXX] Default=<relative path> is the profile TB actually launches
        for s in cp.sections():
            if s.startswith("Install") and cp.has_option(s, "Default"):
                return os.path.join(base, cp.get(s, "Default"))
        for s in cp.sections():
            if s.startswith("Profile") and cp.getboolean(s, "Default", fallback=False):
                p = cp.get(s, "Path", fallback="")
                if p:
                    return p if os.path.isabs(p) else os.path.join(base, p)
    for pat in ("*.default-release", "*.default"):
        hits = sorted(glob.glob(os.path.join(base, pat)))
        if hits:
            return hits[0]
    sys.exit("tb-mbox-sync: no Thunderbird profile found under ~/.thunderbird")


def owl_mail_dir(profile):
    """The Owl account dir under webaccountMail (the one holding mbox folders)."""
    base = os.path.join(profile, "webaccountMail")
    best, best_score = None, -1
    for d in sorted(glob.glob(os.path.join(base, "*"))):
        if not os.path.isdir(d):
            continue
        score = 0
        for r, _, fs in os.walk(d):
            for f in fs:
                p = os.path.join(r, f)
                if not f.endswith((".msf", ".dat")) and os.path.getsize(p) > 0:
                    score += 1
        if score > best_score:
            best, best_score = d, score
    if not best:
        sys.exit("tb-mbox-sync: no Owl mail store under %s" % base)
    return best


def load_state():
    try:
        with open(STATE) as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError):
        return {}


def save_state(state):
    os.makedirs(os.path.dirname(STATE), exist_ok=True)
    tmp = STATE + ".tmp"
    with open(tmp, "w") as f:
        json.dump(state, f)
    os.replace(tmp, STATE)


def is_mbox_file(path):
    if not os.path.isfile(path):
        return False
    base = os.path.basename(path)
    if base.endswith((".msf", ".dat", ".tmp")):
        return False
    if os.path.getsize(path) == 0:
        return False
    parts = set(path.split(os.sep))
    if parts & {"cur", "new", "tmp"}:
        return False
    return True


def dest_folder_for(relpath):
    cleaned = relpath.replace(".sbd" + os.sep, os.sep)
    return os.path.join(DEST, cleaned)


def clean_message_bytes(message):
    """Return the raw message starting at its real headers.

    Thunderbird stores an escaped '>From - ...' line ahead of each message and
    Python's mbox parser leaves a blank line; both must go or there are no
    valid headers."""
    raw = message.as_bytes()
    if raw.startswith(b"\r\n"):
        raw = raw[2:]
    elif raw.startswith(b"\n"):
        raw = raw[1:]
    if raw[:6] == b">From " or raw[:5] == b"From ":
        nl = raw.find(b"\n")
        if nl != -1:
            raw = raw[nl + 1:]
    return raw


def msg_key(parsed, raw):
    mid = parsed.get("Message-ID")
    if mid:
        return mid.strip()
    return "sha1:" + hashlib.sha1(raw).hexdigest()


def is_read(parsed):
    raw = parsed.get("X-Mozilla-Status")
    if not raw:
        return False
    try:
        return bool(int(raw, 16) & MOZ_STATUS_READ)
    except ValueError:
        return False


def sync_folder(src_file, relpath, folder_state):
    seen = set(folder_state.get("seen", []))
    dst = mailbox.Maildir(dest_folder_for(relpath), create=True)
    added = 0
    try:
        src = mailbox.mbox(src_file, create=False)
    except Exception as e:
        print(f"  ! cannot open {relpath}: {e}", file=sys.stderr)
        return 0
    for key in src.iterkeys():
        try:
            message = src[key]
        except Exception:
            continue
        raw = clean_message_bytes(message)
        parsed = email.message_from_bytes(raw)
        k = msg_key(parsed, raw)
        if k in seen:
            continue
        md = mailbox.MaildirMessage(raw)
        if is_read(parsed):
            md.set_subdir("cur")
            md.add_flag("S")
        else:
            md.set_subdir("new")
        try:
            dst.add(md)
        except Exception as e:
            print(f"  ! add failed in {relpath}: {e}", file=sys.stderr)
            continue
        seen.add(k)
        added += 1
    folder_state["seen"] = sorted(seen)
    return added


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--print-watch-dir", action="store_true",
                    help="print the dir a watcher should monitor, then exit")
    args = ap.parse_args()

    src_root = owl_mail_dir(thunderbird_profile())
    if args.print_watch_dir:
        print(src_root)
        return

    state = load_state()
    total_new = 0
    for root, _dirs, files in os.walk(src_root):
        for name in files:
            path = os.path.join(root, name)
            if not is_mbox_file(path):
                continue
            relpath = os.path.relpath(path, src_root)
            st = os.stat(path)
            sig = f"{st.st_mtime_ns}:{st.st_size}"
            fstate = state.setdefault(relpath, {})
            if fstate.get("sig") == sig and "seen" in fstate:
                continue
            added = sync_folder(path, relpath, fstate)
            fstate["sig"] = sig
            if added:
                print(f"  + {added:5d}  {relpath}")
            total_new += added
    save_state(state)
    print(f"sync done: {total_new} new message(s)")


if __name__ == "__main__":
    main()
