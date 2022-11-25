
# wipNES

What is this?

It's an emulator written in plain C11, with SDL backend for Audio/Video. The actual state is kinda limited to run just CPU opcodes,
there is no actual video output, nor audio.

There is a minimal PPU simulation, which is needed to boot some ROMs, as they wait for
some register changes to continue execution.


## Run Locally

Clone the project

```bash
  git clone https://github.com/polaco1782/wipNES
```

Build or Run it with Bazel

```bash
  cd wipNES
  bazel run //:wipnes --compilation_mode=dbg
```


## Screenshots

![Debug Console](https://i.imgur.com/HhP1U4f.png)

