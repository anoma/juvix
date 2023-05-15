# The GHC alpine image

We need this image to workaround an issue with the official GHC alpine binary,
see https://gitlab.haskell.org/ghc/ghc/-/issues/20266

We use this image to make static linux binaries.

## Building the image

The tag of the image should be prefixed by the location of the GitHub docker
repository that you're pushing to. In this case the repository is `
ghcr.io/paulcadman`.

```shell
docker build -t ghcr.io/paulcadman/ghc-alpine:9.2.7 -f Dockerfile-ghc-alpine-9.2.7 .
```

## Authenticating with the GitHub Docker repository

First create a classic personal access token with `repo` and `write:packages`
permissions.

Consult the GitHub documentation on how to do this:

https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-to-the-container-registry

Set the token to the variable `CR_PAT` and then authenticate:

```shell
echo $CR_PAT | docker login ghcr.io -u USERNAME --password-stdin
```

NB: You do not substitue your username for `USERNAME` in the command above.

## Testing the image

```shell
docker run -it --rm ghcr.io/paulcadman/ghc-alpine:9.2.7
```

## Pushing the image

```shell
docker push ghcr.io/paulcadman/ghc-alpine:9.2.7

```
