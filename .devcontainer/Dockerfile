# OS
ARG VARIANT="ubuntu-22.04"

FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}
ENV DEBIAN_FRONTEND=noninteractive
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.2.7
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0
ENV BOOTSTRAP_HASKELL_STACK_VERSION=2.9.3
ENV BOOTSTRAP_HASKELL_INSTALL_STACK=1
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1

# OS packages for ghcup and juvix runtime
RUN apt-get update && \
      apt-get -y install --no-install-recommends \
      curl \
      sudo \
      wget \
      unzip \
      git \
      jq \
      bc \
      make \
      cmake \
      clang \
      lldb \
      lld \
      llvm \
      automake \
      rsync \
      htop \
      build-essential \
      lsb-release \
      pkg-config \
      libffi-dev \
      libgmp-dev \
      libgmp10 \
      software-properties-common \
      libssl-dev \
      libicu-dev \
      libtinfo-dev \
      libsystemd-dev \
      zlib1g-dev \
      g++ \
      libncurses-dev \
      libncursesw5 \
      libtinfo5 \
      libtool \
      autoconf && apt-get clean

RUN sudo bash -c 'curl -sSL https://get.haskellstack.org/ | sh'

ENV PATH=${PATH}:/home/vscode/.ghcup/bin

RUN sudo bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

ENV PATH=${PATH}:/home/vscode/.local/bin
RUN sudo bash -c 'mkdir -p /home/vscode/.local/bin'
RUN sudo bash -c "chown -R vscode /home/vscode/.local/"

RUN sudo bash -c 'curl https://get.wasmer.io -sSfL | sudo WASMER_DIR=/home/vscode/.local sh'

# to be used by the juvix extension
RUN sudo bash -c 'mkdir -p /home/vscode/.local/juvix'
RUN sudo bash -c 'chmod -R a+rXw /home/vscode/.local/juvix'

ENV XDG_BIN_HOME=/home/vscode/.local/bin
ENV XDG_DATA_HOME=/home/vscode/.local/share
ENV JUVIX_INSTALLER_INSTALL_VAMPIR_YES=true
ENV JUVIX_INSTALLER_ASSUME_YES=true
RUN sudo bash -c 'curl --proto "=https" --tlsv1.2 -sSf https://get.juvix.org | sudo sh'

RUN sudo bash -c 'mkdir -p /home/vscode/.local/wasi-sysroot'
RUN sudo bash -c 'curl -s https://api.github.com/repos/WebAssembly/wasi-sdk/releases/tags/wasi-sdk-16 \
      | grep -a "browser_download_url.*wasi-sysroot-16.*tar.gz" \
      | cut -d : -f 2,3 \
      | tr -d \" \
      | wget --output-document wasi-sysroot.tar.gz -qi - \
      && tar xvf wasi-sysroot.tar.gz --directory /home/vscode/.local/wasi-sysroot/'
ENV WASI_SYSROOT_PATH=/home/vscode/.local/wasi-sysroot/

RUN sudo bash -c 'curl -s https://api.github.com/repos/jonaprieto/smoke/releases/latest \
      | grep -a "browser_download_url.*Linux" \
      | cut -d : -f 2,3 \
      | tr -d \" \
      | wget --output-document smoke -qi - \
      && mv smoke /home/vscode/.local/bin/smoke \
      && chmod a+x /home/vscode/.local/bin/smoke'
