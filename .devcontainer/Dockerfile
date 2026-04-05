# .devcontainer/Dockerfile
FROM haskell:9.6.7

# 裝 ghcup 的依賴
RUN apt-get update && apt-get install -y \
    curl xz-utils make gcc libgmp-dev zlib1g-dev \
    libncurses5-dev libffi-dev libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# 裝 GHCup，再用它裝 HLS
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    sh

ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:${PATH}"