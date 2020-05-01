rm -rf lsc2bpf/all-cchh;
rm -rf quick-o2b/all-cchh;
rm -rf quick-b2r/all-cchh;
rm -rf quick-results
mkdir -p quick-results

echo "Starting LibSecComp to eBPF";
racket test-all-opts.rkt lsc2bpf | tee l2b.out;
echo "Finished LibSecComp to eBPF";
echo "Output stored in l2b.out";
racket count-time-spent.rkt l2b.out;
cp lsc2bpf/all-cchh/compiler.h quick-results/l2b_compiler.h
echo "";

echo "Starting subset of Classic BPF to eBPF";
racket test-all-opts.rkt quick-o2b | tee qo2b.out;
echo "Finished subset of Classic BPF to eBPF";
echo "Output stored in qo2b.out";
racket count-time-spent.rkt qo2b.out;
cp quick-o2b/all-cchh/compiler.h quick-results/o2b_compiler.h
echo "";

echo "Starting subset of eBPF to RISC-V";
racket test-all-opts.rkt quick-b2r | tee qb2r.out;
echo "Finished subset of eBPF to RISC-V";
echo "Output stored in qb2r.out";
racket count-time-spent.rkt qb2r.out;
cp quick-b2r/all-cchh/compiler.h quick-results/b2r_compiler.h
echo "";

./fig-8.sh;
