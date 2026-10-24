"""Check test_vision vectors against the offline Qwen3-VL MLX oracle.

Requires mlx-embeddings, transformers, torch, torchvision and numpy. These are
test dependencies only. No download or Python process is used by inference.
"""

import json
import os
import pathlib
import sys

os.environ["HF_HUB_OFFLINE"] = "1"
os.environ["TRANSFORMERS_OFFLINE"] = "1"

import mlx.core as mx
import numpy as np
from mlx_embeddings import load


def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: compare_vision_reference.py MODEL_DIR NATIVE_JSON")
    directory = pathlib.Path(sys.argv[1]).resolve(strict=True)
    if not directory.is_dir():
        raise SystemExit("MODEL_DIR must be a local directory")
    fixtures = pathlib.Path(__file__).resolve().parent / "vision-fixtures"
    native = json.loads(pathlib.Path(sys.argv[2]).read_text())
    model, processor = load(str(directory))
    processor.embedding_max_length = 512
    long = " ".join(["compiler"] * 2000)
    samples = {
        "texts": [{"text": "A red square."}, {"text": "A blue rectangle. 北京 🐫"}],
        "images": [{"image": str(fixtures / p)} for p in ["red.png", "blue.png"]],
        "resized": [{"image": str(fixtures / "pattern.png")}],
        "truncated": [{"text": long}, {"text": long + " apples"}],
    }
    # Pin the full template, including the tokenizer postprocessor's EOS.
    first_tokens = [151644, 8948, 198, 65743, 279, 1196, 594, 1946, 13,
                    151645, 198, 151644, 872, 198, 32, 2518, 9334, 13,
                    151645, 198, 151644, 77091, 198, 151643]
    for key, rows in samples.items():
        reference = []
        for index, row in enumerate(rows):
            inputs = processor.prepare_embedding_inputs([
                dict(row, instruction="Represent the user's input.")
            ])
            tokens = inputs["input_ids"].tolist()[0]
            assert tokens[-1] == 151643, (key, tokens[-8:])
            if key == "texts" and index == 0:
                assert tokens == first_tokens, tokens
            if key == "truncated":
                assert len(tokens) == 512, len(tokens)
            # mlx-embeddings 0.1.0 caches text positions as a 2-D array but
            # slices a reused cache as 3-D. Every fixture is an independent
            # input, so recompute positions just as the native adapter does.
            model.language_model._position_ids = None
            model.language_model._rope_deltas = None
            vector = model(**inputs).pooler_output.astype(mx.float32)
            if key == "truncated":
                vector = vector[:, :256]
            vector = vector / mx.linalg.norm(vector, axis=-1, keepdims=True)
            mx.eval(vector)
            reference.append(np.asarray(vector)[0])
        expected = np.asarray(reference)
        actual = np.asarray(native[key], dtype=np.float32)
        assert actual.shape == expected.shape, (key, actual.shape, expected.shape)
        assert np.isfinite(actual).all(), key
        assert np.all(np.abs(np.linalg.norm(actual, axis=1) - 1) < 1e-5), key
        cosine = np.sum(actual * expected, axis=1)
        # CoreImage and Pillow have different bicubic resize kernels.
        threshold = 0.995 if key == "resized" else 0.999
        assert np.all(cosine > threshold), (key, cosine)
        print(key, "Swift/reference cosine:", cosine.tolist(), flush=True)


if __name__ == "__main__":
    main()
