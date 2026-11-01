"""Correct a known upstream dialect error without changing the snapshot."""

import json
import sys


def normalize(schema):
    bound = schema["components"]["schemas"]["VideoGenerationRequest"][
        "properties"
    ]["upscale_factor"]
    # This 3.1 document contains one 3.0 exclusive bound. Preserve x > 0.
    if bound.get("exclusiveMinimum") is True:
        if bound.get("minimum") != 0:
            raise ValueError("upstream upscale_factor bound has changed")
        bound["exclusiveMinimum"] = bound.pop("minimum")
    return schema


if __name__ == "__main__":
    with open(sys.argv[1], encoding="utf-8") as source:
        json.dump(normalize(json.load(source)), sys.stdout, ensure_ascii=False)
    sys.stdout.write("\n")
