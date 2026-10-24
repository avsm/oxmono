"""Compare test_inference's synthetic vectors with offline mlx-embeddings."""

import json
import os
import pathlib
import sys

os.environ["HF_HUB_OFFLINE"] = "1"
os.environ["TRANSFORMERS_OFFLINE"] = "1"

import mlx.core as mx
import numpy as np
from mlx_embeddings import generate, load


def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: compare_reference.py MODEL_DIR NATIVE_VECTORS_JSON")
    directory = pathlib.Path(sys.argv[1]).resolve(strict=True)
    if not directory.is_dir():
        raise SystemExit("MODEL_DIR must be a local model directory")
    texts = [
        "Instruct: Given a web search query, retrieve relevant passages that answer "
        "the query\nQuery:What is the capital of China?",
        "The capital of China is Beijing.",
        "Gravity attracts massive objects to each other.",
        "北京是中国的首都。",
        "A compiler translates source code into executable instructions. 🐫",
    ]
    model, tokenizer = load(
        str(directory),
        tokenizer_config={"padding_side": "right", "local_files_only": True},
    )
    reference = generate(model, tokenizer, texts=texts, max_length=512).text_embeds
    reference = reference.astype(mx.float32)
    reference = reference / mx.linalg.norm(reference, axis=-1, keepdims=True)
    mx.eval(reference)
    reference = np.asarray(reference)
    native = np.asarray(json.loads(pathlib.Path(sys.argv[2]).read_text()), dtype=np.float32)
    assert native.shape == reference.shape == (5, 1024)
    assert np.isfinite(native).all()
    cosine = np.sum(native * reference, axis=1)
    assert np.all(cosine > 0.999), cosine
    native_ranking = np.argsort(-(native @ native.T)[0]).tolist()
    reference_ranking = np.argsort(-(reference @ reference.T)[0]).tolist()
    assert native_ranking == reference_ranking, (native_ranking, reference_ranking)
    print("Swift/reference cosine:", cosine.tolist())
    print("Matching ranking:", native_ranking)


if __name__ == "__main__":
    main()
