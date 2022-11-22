
 # wipNES

What is this?

It's an emulator written in plain C11, with SDL backend for Audio/Video. The actual state is kinda limited to run just CPU opcodes,
there is no actual video output, nor audio.

There is a minimal PPU simulation, which is needed to boot some ROMs, as they wait for
some register changes to continue execution.

