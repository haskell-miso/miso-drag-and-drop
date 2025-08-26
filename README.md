:ramen: 📁 miso-drag-and-drop
====================

<img width="927" height="389" alt="image" src="https://github.com/user-attachments/assets/a20e4ea8-d4d9-4cb4-993b-993e2e48fc92" />

An example of using the Drag and Drop API in [miso](https://github.com/dmjio/miso). See live [here](https://drag-and-drop.haskell-miso.org)

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
