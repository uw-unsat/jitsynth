#!/usr/bin/env python3

import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from itertools import cycle

plt.rcParams.update({
    'pgf.texsystem': 'pdflatex',
    'pgf.rcfonts': False,
})

sns.set(style="whitegrid", font_scale=1.75)

fig = plt.figure()
instrs = pd.read_csv("graphs/l2b-fig8.csv")
instrs['Compiler'] = instrs['compiler'].map(
        dict(zip(["seccomp","jitsynth"],
            ["libseccomp","JitSynth"])))
instrs['Benchmark'] = instrs['benchmark']
g = sns.catplot(x="Benchmark", y="instructions", hue="Compiler", data=instrs, kind="bar", aspect=1.6, legend_out=False)
for i,bar in enumerate(g.ax.patches):
    bar.set_hatch(' ' if i >= 5 else '///')
g.ax.legend(loc='best')
g.despine(left=True)
plt.title("libseccomp to eBPF benchmarks")
g.set_ylabels("Instructions executed")
plt.tight_layout()
plt.savefig("graphs/l2b-cc.pgf")
plt.close(fig)

fig = plt.figure()
instrs = pd.read_csv("graphs/o2b-fig8.csv")
instrs['Compiler'] = instrs['compiler'].map(
        dict(zip(["linux","jitsynth"],
            ["Linux","JitSynth"])))
instrs['Benchmark'] = instrs['benchmark']
g = sns.catplot(x="Benchmark", y="instructions", hue="Compiler", data=instrs, kind="bar", aspect=1.6, legend_out=False)
for i,bar in enumerate(g.ax.patches):
    bar.set_hatch(' ' if i >= 7 else '///')
g.ax.legend(loc='best')
g.despine(left=True)
plt.title("Classic BPF to eBPF benchmarks")
g.set_ylabels("Instructions executed")
plt.tight_layout()
plt.savefig("graphs/o2b-cc.pgf")
plt.close(fig)
