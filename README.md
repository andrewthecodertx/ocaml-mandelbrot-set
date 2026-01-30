# Mandelbrot Set Viewer

An interactive Mandelbrot set explorer written in OCaml using the Graphics library.

The [Mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set) is the set of complex numbers *c* for which the iteration *z = z² + c* (starting from *z = 0*) remains bounded. Points inside the set are colored black; points outside are colored based on how many iterations it takes to escape, producing the characteristic fractal boundary.

## Requirements

- OCaml compiler
- [opam](https://opam.ocaml.org/) package manager
- SDL2 runtime library

Install SDL2 and the OCaml dependencies:

```bash
# Ubuntu/Debian
sudo apt-get install libsdl2-dev

# macOS
brew install sdl2

# Windows (MSYS2)
pacman -S mingw-w64-x86_64-SDL2
```

```bash
opam install . --deps-only
```

## Build & Run

```bash
dune build
dune exec mandelbrot
```

## Controls

| Input | Action |
|---|---|
| Click | Zoom in 2x, centered on cursor |
| `+` / `=` | Zoom in 2x at cursor position |
| `-` | Zoom out 2x at cursor position |
| `h` / `a` | Pan left |
| `l` / `d` | Pan right |
| `j` / `s` | Pan down |
| `k` / `w` | Pan up |
| `r` | Reset to original view |
| `q` / `Esc` | Quit |
