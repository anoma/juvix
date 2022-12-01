# The GHC alpine image

We need this image to workaround an issue with the official GHC alpine binary,
see https://gitlab.haskell.org/ghc/ghc/-/issues/20266

We use this image to make static linux binaries.

## Building the image

The tag of the image should be prefixed by the location of the GitHub docker
repository that you're pushing to. In this case the repository is `
ghcr.io/paulcadman`.

``` shell
docker build -t ghcr.io/paulcadman/ghc-alpine:9.2.5 -f Dockerfile-ghc-alpine-9.2.5 .
```

## Authenticating with the GitHub Docker repository

First create a classic personal access token with `repo` and `write:packages`
permissions.

Consult the GitHub documentation on how to do this:

https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#authenticating-to-the-container-registry

Then authenticate:

``` shell
echo $CR_PAT | docker login ghcr.io -u USERNAME --password-stdin
```

## Testing the image

``` shell
docker run -it --rm ghcr.io/paulcadman/ghc-alpine:9.2.5
```

## Pushing the image

``` shell
docker push ghcr.io/paulcadman/ghc-alpine:9.2.5

```
