"""Remove key-shaped examples before checking in an OpenRouter schema."""

import argparse
import json
from pathlib import Path
import re
import sys


KEY = re.compile(r"sk-or-v1-[A-Za-z0-9_-]+(?:\.\.\.[A-Za-z0-9_-]*)?")
PLACEHOLDER = "OPENROUTER_API_KEY_EXAMPLE"


def sanitize(source):
    """Validate JSON and replace key-shaped text without reformatting it."""
    json.loads(source)
    return KEY.sub(PLACEHOLDER, source)


def key_lines(source):
    """Return matching line numbers without returning credential contents."""
    json.loads(source)
    return [source.count("\n", 0, match.start()) + 1
            for match in KEY.finditer(source)]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true",
                        help="refuse unsanitized input without printing it")
    parser.add_argument("schema", type=Path)
    args = parser.parse_args()
    source = args.schema.read_bytes().decode("utf-8")
    if args.check:
        lines = key_lines(source)
        if lines:
            print(f"{args.schema}: key-shaped examples at lines "
                  + ", ".join(map(str, lines)), file=sys.stderr)
            return 1
    else:
        sys.stdout.write(sanitize(source))
    return 0


if __name__ == "__main__":
    sys.exit(main())
