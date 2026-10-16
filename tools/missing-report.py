#!/usr/bin/env python3
"""Summarize "Missing Exported Functions" warnings from per-constructor
compile logs, as written by tools/missing-survey.sh.

usage: missing-report.py LOGDIR [--json OUT]
"""

import json
import os
import re
import sys


def parse(path):
    """Return (constructor name, [finding, ...]) for one log file."""
    name = None
    found = []
    in_block = False
    finished = False
    with open(path, errors="replace") as f:
        for line in f:
            if "finalizing NRLIB" in line:
                finished = True
            m = re.search(r"initializing NRLIB \S+ for (\S+)", line)
            if m:
                name = m.group(1)
            if "Warning: Missing Exported Functions" in line:
                in_block = True
                continue
            if in_block:
                m = re.match(r"\s+\[\d+\] (.*\S)", line)
                if m:
                    found.append(m.group(1))
                elif line.strip():
                    in_block = False
    return name, found, finished


def main():
    logdir = sys.argv[1]
    report = {}
    failed = []
    for fn in sorted(os.listdir(logdir)):
        if fn.endswith(".log"):
            name, found, finished = parse(os.path.join(logdir, fn))
            if not finished:
                failed.append(fn[:-4])
            if found:
                report[fn[:-4]] = {"name": name, "missing": found}
    n_logs = len([f for f in os.listdir(logdir) if f.endswith(".log")])
    n_find = sum(len(v["missing"]) for v in report.values())
    n_unc = sum(1 for v in report.values() for x in v["missing"] if " if " not in x)
    print(f"{n_logs} constructors, {len(report)} with findings, "
          f"{n_find} findings ({n_unc} unconditional)")
    if failed:
        print(f"{len(failed)} compilations did not finish (see their logs): "
              + " ".join(failed))
    for abbr, v in sorted(report.items(),
                          key=lambda kv: -len(kv[1]["missing"])):
        unc = [x for x in v["missing"] if " if " not in x]
        print(f"{abbr} {v['name']}: {len(unc)} unconditional, "
              f"{len(v['missing']) - len(unc)} conditional")
        for x in v["missing"]:
            print("    " + x)
    if "--json" in sys.argv:
        with open(sys.argv[sys.argv.index("--json") + 1], "w") as f:
            json.dump(report, f, indent=1)


if __name__ == "__main__":
    main()
