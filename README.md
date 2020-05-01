# JitSynth

JitSynth is a synthesis tool for generating JIT compilers.
Our tool is designed to target in-kernel DSLs, but can synthesize compilers for many source and target languages.
JitSynth takes as input two interpreters: one for the source language and one for the target. 
JitSynth also takes in a mapping between source and target states.

## Scripts

Below is a description of the scripts that can be used to reproduce the results from the paper.
Scripts that execute a synthesis task will generally store their progress in case of any unexpected crashes,
so scripts that are restarted after stopping can use this progress.
If you wish to clear this stored progress, please use `clean.sh`.

### quick.sh
 Runs a synthesis task for a simplified version of each source-target pair to save time.
Expected to take 1-5 hours, depending on hardware.
Please ensure that your VM has at least 4GB of RAM when running `quick.sh`.
The source-target pairs are LibSecComp to eBPF, Classic (old) BPF to eBPF, and eBPF to RISC-V.
Since LibSecComp to eBPF synthesis is already fast, `quick.sh` runs the entire synthesis task for this pair. For the remaining two, `quick.sh` synthesizes the compiler for a small subset of the source language.
All optimizations are turned on for each of these synthesis tasks.
Logs the time spent per-instruction and per-program-length in the following format:

```
name,(length/"all"),real-time,cpu-time,garbage-collection-time
```

The generated partial compilers are saved in the `quick-results` folder.

`quick.sh` also generates CSV files to reproduce figure 8 in the paper.
For the left graph (LibSecComp to eBPF), these generated files are `l2b-jitsynth.csv` and `l2b-linux.csv`.
For the right graph (Classic BPF to eBPF), these generated files are `o2b-jitsynth.csv` and `o2b-linux.csv`.


### slow.sh
Runs a synthesis task for each source-target pair.
Expected to take at least 48 hours, but runtime could be much longer depending on hardware.
All optimizations are turned on for each of these synthesis tasks.
The timing results of these synthesis tasks correspond to the right-most column (PLD sketch)
of figure 9.
**If you plan to run `slow.sh`, please increase the amount of memory given to the VM to at least 16GB.**

`slow.sh` also generates CSV files to reproduce figure 8 in the paper.
For the left graph (LibSecComp to eBPF), these generated files are `l2b-jitsynth.csv` and `l2b-linux.csv`.
For the right graph (Classic BPF to eBPF), these generated files are `o2b-jitsynth.csv` and `o2b-linux.csv`.


### run-with-config.sh
Run synthesis for a specific source-target pair and specific set of optimizations.
This script can be used to reproduce figure 9.

The first option to give `run-with-config.sh` is the optimization set. The possible optimization options are "naive", "rw", and "pld".
The second option to give `run-with-config.sh` is the source-target pair. The possible source-target pair options are "lsc2bpf", "old-bpf2bpf", and "bpf2riscv", which are short for "LibSecComp to eBPF", "Classic BPF to eBPF", and "eBPF to RISC-V" respectively.

For example, to run synthesis for LibSecComp to eBPF with the naive sketch optimizations, run the following command:

```
./run-with-config.sh naive lsc2bpf
```


### fig-8.sh
Reconstructs figure 8 from pre-compiled LibSecComp and Classic BPF benchmarks.
The generated CSV and LaTex files are stored in the `graphs` directory.
To view the graphs, please copy the files in `graphs` into a LaTex editor (e.g. Overleaf)
and compile `main.tex`.

### clean.sh
Removes partial progress of synthesis queries.


## Directories
Below is a description of all directories in the JitSynth project. This may be useful for reviewers interested in understanding the JitSynth source code.

### ams
Library for specifying abstract register machines.

### boolector
Binary file for Boolector, the SMT solver used by JitSynth.

### bpf
Interpreter and language specification for eBPF.

### bpf2riscv
State mapping between eBPF and RISC-V.

### common
Common libraries, including structs for state and instructions, data structures for memory and registers, state equality functions, and convenience functions.

### genc
Library for converting JitSynth Racket output into C code.

### gensketch
Library for generated program synthesis sketches from JitSynth input specification.

### graphs
Generated CSV and LaTeX files for figure 8 graphs.

### l2b-bench
Compiled programs run for figure 8, left graph.

### linux
Resources from Linux which JitSynth compares against.

### lsc
Interpreter and language specification for LibSecComp.

### lsc2bpf
State mapping between LibSecComp and eBPF.

### o2b-bench
Compiled programs run for figure 8, right graph.

### old-bpf
Interpreter and language specification for Classic BPF.

### old-bpf2bpf
State mapping between Classic BPF and eBPF.

### proofs
Proofs for theorem 1 and theorem 2, written in Lean.

### quick-b2r
Simplified eBPF to RISC-V mapping.

### quick-bpf
Subset of the eBPF language.

### quick-o2b
Simplified Classic BPF to eBPF mapping.

### quick-old-bpf
Subset of the Classic BPF language.

### quick-results
Generated compilers from `quick.sh`.

### riscv
Interpreter and language specification for RISC-V.

### slow-results
Generated compilers from `slow.sh`.

### synthesis
Library for calling Rosette `synthesize` queries.

### test
Library for testing implemented interpreters and implemented/synthesized compilers.

### toy-ebpf
Subset of eBPF used for the motivating example for JitSynth.

### toy-ebpf-to-toy-riscv
Mapping from eBPF subset to RISC-V subset used for the motivating example for JitSynth.

### toy-riscv
Subset of RISC-V used for the motivating example for JitSynth.
