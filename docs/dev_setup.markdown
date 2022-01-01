# Developer Setup

Project:M36 is developed in Haskell with GHC 8.8+ and stack or cabal. Project:M36 includes server and client executables, a test suite, and example programs. See [project-m36.cabal](https://github.com/agentm/project-m36/blob/master/project-m36.cabal) for the available options.


## cabal

Use [`ghcup`](https://www.haskell.org/ghcup/) to install GHC and `cabal` to build and run `tutd`.

```
ghcup install ghc
cabal new-run tutd
```

## stack

Use [`stack`](https://docs.haskellstack.org/en/stable/README/) to build and run `tutd`.

```
stack --stack-yaml=stack.ghc.8.10.yaml run tutd
```

## VSCode

Project:M36 can be used with the haskell-language-server (HLS) with the Haskell Language plugin to VSCode with either `stack` or `cabal`.

### cabal

Before launching VSCode, rename `cabal.hie.yaml` to `hie.yaml`.

### stack

Before launching VSCode, rename `stack.hie.yaml` to `hie.yaml`.



