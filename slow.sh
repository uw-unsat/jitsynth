rm -rf lsc2bpf/all-cchh;
rm -rf old-bpf2bpf/all-cchh;
rm -rf bpf2riscv/all-cchh;
rm -rf slow-results
mkdir -p slow-results

echo "Starting LibSecComp to eBPF";
racket test-all-opts.rkt lsc2bpf | tee l2b.out;
echo "Finished LibSecComp to eBPF";
echo "Output stored in l2b.out";
racket count-time-spent.rkt l2b.out;
cp lsc2bpf/all-cchh/compiler.h slow-results/l2b_compiler.h
echo "";

echo "Starting Classic BPF to eBPF";
racket test-all-opts.rkt old-bpf2bpf | tee so2b.out;
echo "Finished Classic BPF to eBPF";
echo "Output stored in so2b.out";
racket count-time-spent.rkt so2b.out;
cp old-bpf2bpf/all-cchh/compiler.h slow-results/o2b_compiler.h
echo "";

echo "Starting eBPF to RISC-V";
racket test-all-opts.rkt bpf2riscv | tee sb2r.out;
echo "Finished eBPF to RISC-V";
echo "Output stored in sb2r.out";
racket count-time-spent.rkt sb2r.out;
cp bpf2riscv/all-cchh/compiler.h slow-results/b2r_compiler.h
echo "";

./fig-8.sh;
