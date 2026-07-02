#!/usr/bin/env python3
"""Generate an org-agenda file from Thunderbird/Owl's calendar cache.

Reads a *snapshot copy* of calendar-data/cache.sqlite (never the live DB) and
writes ~/.local/share/tb-calendar/exchange.org. Single events convert exactly;
recurring events become org repeaters (+1w/+1m/...). Rules org can't express
(multi-weekday, COUNT, nth-weekday) get a best-effort repeater and are tagged
:complex: with the raw RRULE in the entry.

Portable: auto-detects the Thunderbird profile (override with TB_PROFILE).
`--print-watch-dir` prints the directory a file watcher should monitor.
"""

import argparse
import configparser
import glob
import os
import shutil
import sqlite3
import sys
import tempfile
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo

HOME = os.path.expanduser("~")
OUT = os.path.join(HOME, ".local/share/tb-calendar/exchange.org")

FLAG_ALLDAY = 8
FLAG_HAS_EXCEPTIONS = 32
DAYS = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
FREQ_UNIT = {"DAILY": "d", "WEEKLY": "w", "MONTHLY": "m", "YEARLY": "y"}


def thunderbird_profile():
    override = os.environ.get("TB_PROFILE")
    if override:
        return os.path.expanduser(override)
    base = os.path.join(HOME, ".thunderbird")
    ini = os.path.join(base, "profiles.ini")
    if os.path.exists(ini):
        cp = configparser.ConfigParser()
        cp.read(ini)
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
    sys.exit("tb-cal-sync: no Thunderbird profile found under ~/.thunderbird")


def caldir(profile):
    return os.path.join(profile, "calendar-data")


def prtime_to_dt(us, tzname):
    secs = us / 1_000_000
    try:
        tz = ZoneInfo(tzname) if tzname and tzname not in ("floating",) else None
    except Exception:
        tz = None
    if tz is None:
        return datetime.fromtimestamp(secs)
    return datetime.fromtimestamp(secs, tz)


def org_ts(start, end, allday, repeater):
    def d(dt):
        return f"{dt.strftime('%Y-%m-%d')} {DAYS[dt.weekday()]}"

    rep = f" {repeater}" if repeater else ""
    if allday:
        last = end - timedelta(days=1)
        if last.date() <= start.date():
            return f"<{d(start)}{rep}>"
        return f"<{d(start)}>--<{d(last)}>"
    if start.date() == end.date():
        return f"<{d(start)} {start.strftime('%H:%M')}-{end.strftime('%H:%M')}{rep}>"
    return f"<{d(start)} {start.strftime('%H:%M')}>--<{d(end)} {end.strftime('%H:%M')}>"


def parse_rrule(ical, start_dt):
    line = next((l for l in ical.splitlines() if l.startswith("RRULE:")), None)
    if not line:
        return None, False, None
    raw = line[len("RRULE:"):]
    parts = dict(p.split("=", 1) for p in raw.split(";") if "=" in p)
    freq = parts.get("FREQ")
    unit = FREQ_UNIT.get(freq)
    if not unit:
        return None, True, raw
    interval = int(parts.get("INTERVAL", "1"))
    complex_ = False
    byday = parts.get("BYDAY", "")
    if freq == "WEEKLY" and "," in byday:
        complex_ = True
    if freq == "MONTHLY" and byday:
        complex_ = True
    if "COUNT" in parts:
        complex_ = True
    until = parts.get("UNTIL")
    if until:
        try:
            u = datetime.strptime(until[:8], "%Y%m%d").date()
            if u < datetime.now().date():
                return None, False, raw
        except ValueError:
            complex_ = True
    return f"+{interval}{unit}", complex_, raw


def fetch_props(cur):
    props = {}
    for item_id, key, value in cur.execute(
        "SELECT item_id, key, value FROM cal_properties "
        "WHERE key IN ('LOCATION','X-MICROSOFT-SKYPETEAMSMEETINGURL') "
        "AND (recurrence_id IS NULL OR recurrence_id=0)"
    ):
        if isinstance(value, bytes):
            value = value.decode("utf-8", "replace")
        if value:
            props.setdefault(item_id, {})[key] = value
    return props


def fetch_rrules(cur):
    rr = {}
    for item_id, ical in cur.execute("SELECT item_id, icalString FROM cal_recurrence"):
        rr.setdefault(item_id, ical)
    return rr


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--print-watch-dir", action="store_true",
                    help="print the dir a watcher should monitor, then exit")
    args = ap.parse_args()

    profile = thunderbird_profile()
    if args.print_watch_dir:
        print(caldir(profile))
        return

    db = os.path.join(caldir(profile), "cache.sqlite")
    tmp = tempfile.mkdtemp()
    try:
        for suffix in ("", "-wal", "-shm"):
            src = db + suffix
            if os.path.exists(src):
                shutil.copy2(src, os.path.join(tmp, "cache.sqlite" + suffix))
        con = sqlite3.connect(f"file:{tmp}/cache.sqlite?mode=ro", uri=True)
        cur = con.cursor()
        props = fetch_props(cur)
        rrules = fetch_rrules(cur)

        events = []
        for (iid, title, start, end, stz, etz, flags) in cur.execute(
            "SELECT id, title, event_start, event_end, event_start_tz, "
            "event_end_tz, flags FROM cal_events "
            "WHERE recurrence_id IS NULL OR recurrence_id=0"
        ):
            if start is None:
                continue
            sdt = prtime_to_dt(start, stz)
            edt = prtime_to_dt(end, etz) if end else sdt
            allday = bool((flags or 0) & FLAG_ALLDAY)
            repeater, is_complex, raw_rule = (None, False, None)
            if iid in rrules:
                repeater, is_complex, raw_rule = parse_rrule(rrules[iid], sdt)
            events.append({
                "title": (title or "(no title)").strip(),
                "ts": org_ts(sdt, edt, allday, repeater),
                "sort": sdt,
                "props": props.get(iid, {}),
                "recurring": iid in rrules,
                "complex": is_complex,
                "raw_rule": raw_rule,
                "has_exceptions": bool((flags or 0) & FLAG_HAS_EXCEPTIONS),
            })
        con.close()
    finally:
        shutil.rmtree(tmp, ignore_errors=True)

    events.sort(key=lambda e: e["sort"])
    stamp = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")

    lines = [
        "#+title: Exchange Calendar",
        "#+category: Exchange",
        "#+filetags: :exchange:",
        f"# AUTO-GENERATED {stamp} by tb-cal-sync.py — edits are overwritten.",
        "",
    ]
    for e in events:
        tags = []
        if e["recurring"]:
            tags.append("recurring")
        if e["complex"]:
            tags.append("complex")
        tagstr = f"  :{':'.join(tags)}:" if tags else ""
        lines.append(f"* {e['title']}{tagstr}")
        lines.append(f"  {e['ts']}")
        loc = e["props"].get("LOCATION")
        url = e["props"].get("X-MICROSOFT-SKYPETEAMSMEETINGURL")
        if loc or url:
            lines.append("  :PROPERTIES:")
            if loc:
                lines.append(f"  :LOCATION: {loc}")
            if url:
                lines.append(f"  :TEAMS: {url}")
            lines.append("  :END:")
        if e["complex"] and e["raw_rule"]:
            lines.append(f"  # approx repeater; actual rule: {e['raw_rule']}")
        if e["has_exceptions"]:
            lines.append("  # has modified/cancelled occurrences not shown individually")
        lines.append("")

    content = "\n".join(lines)

    def body(text):
        return "\n".join(l for l in text.splitlines() if not l.startswith("# AUTO-GENERATED"))

    try:
        with open(OUT) as f:
            if body(f.read()) == body(content):
                print(f"no change ({len(events)} events)")
                return
    except FileNotFoundError:
        pass

    os.makedirs(os.path.dirname(OUT), exist_ok=True)
    tmp_out = OUT + ".tmp"
    with open(tmp_out, "w") as f:
        f.write(content)
    os.replace(tmp_out, OUT)
    print(f"wrote {len(events)} events to {OUT}")


if __name__ == "__main__":
    main()
