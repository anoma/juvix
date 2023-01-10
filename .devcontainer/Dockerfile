# OS
ARG VARIANT="ubuntu-22.04"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
# avoid interactive mode when installing packages
ENV DEBIAN_FRONTEND=noninteractive
# avoid interactive mode when installing ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# GHC version
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.2.5
# Cabal version
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.8.1.0
# install stack
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1
# install haskell language server
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
# OS packages for ghcup
RUN apt-get update && \
    apt-get -y install --no-install-recommends \
    git \
    sudo \
    jq \
    bc \
    make \
    cmake \
    clang \
    llvm \
    automake \
    rsync \
    htop \
    curl \
    build-essential \
    lsb-release \
    pkg-config \
    libffi-dev \
    libgmp-dev \
    software-properties-common \
    libssl-dev \
    libicu-dev \
    libtinfo-dev \
    libsystemd-dev \
    zlib1g-dev \
    g++ \
    wget \
    libncursesw5 \
    libtool \
    autoconf && apt-get clean
# change user
USER vscode
# install ghcup
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
# set PATH
ENV PATH=${PATH}:/home/vscode/.local/bin
ENV PATH=${PATH}:/home/vscode/.ghcup/bin
