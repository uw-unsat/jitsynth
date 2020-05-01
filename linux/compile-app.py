#!/usr/bin/python

import sys
import glob, os

def output_op(op, fw):
    op = op[::-1]
    fw.write(''.join(op))
    fw.write('\n')

def dump_to_bpf(fileread, filewrite):
    ops = []
    with open(fileread, 'r') as fr:
        for line in fr:
            byteline = line.split(': ')[2].split()
            ops += [byteline[0:8]]
            if len(byteline) > 8:
                ops += [byteline[8:16]]
    
    with open(filewrite, 'w+') as fw:
        fw.write(str(len(ops)))
        fw.write('\n')
        for op in ops:
            output_op(op, fw)

# Take directory as input
compiler = sys.argv[1]
indir = sys.argv[2]
outdir = sys.argv[3]
os.chdir(indir)
# files = ["qemu0.ebpf.dump"] 
files = glob.glob("*.dump")
# TODO cd to original dir
os.chdir("../..")
for filein in files:
    # bpf_to_rv(filein, fileout)
    ebpf, _ = os.path.splitext(filein)
    name, _ = os.path.splitext(ebpf)
    print(name)
    sys.stdout.flush()
    bpffile = indir + '/' + name + '.in'
    rvfile = outdir + '/' + name + '.rv.bin'
    dump_to_bpf(indir + '/' + filein, bpffile)
    # TODO make compiler a parameter too
    os.system('./' + compiler + ' ' + bpffile + ' ' + rvfile + ' -d')
    print('done')
    sys.stdout.flush()
