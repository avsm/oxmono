
// Oxmono extension to mlx-swift-lm 3.32.3, Qwen3VL.swift.
// The upstream private vision/deepstack/M-RoPE implementation is shared by
// generation and embeddings. Embeddings require the normalized final hidden
// states before the vocabulary projection, and do not have an LM-head weight.
extension Qwen3VL {
    public func prepareForEmbeddingInference() {
        languageModel.lmHead = nil
        train(false)
    }

    public func embeddingHiddenStates(_ input: LMInput) throws -> MLXArray {
        let vision = try visionInputs(input)
        let (positions, _) = Qwen3VLLanguage.getRopeIndex(
            inputIds: input.text.tokens,
            imageGridTHW: vision.imageFrames,
            videoGridTHW: nil,
            spatialMergeSize: config.visionConfiguration.spatialMergeSize,
            imageTokenId: config.imageTokenIndex,
            videoTokenId: config.videoTokenIndex,
            visionStartTokenId: config.visionStartTokenId,
            attentionMask: input.text.mask)
        return languageModel.model(
            input.text.tokens, cache: nil,
            inputEmbeddings: vision.inputEmbeddings, mask: nil,
            positionIds: positions, visualIndices: vision.visualIndices,
            deepstackEmbeds: vision.deepstackEmbeds)
    }
}
