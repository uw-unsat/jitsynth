opt=$1
dir=$2

if [ "$dir" != "lsc2bpf" ] && [ "$dir" != "old-bpf2bpf" ] && [ "$dir" != "bpf2riscv" ]; then
  echo "Invalid directory option"
else
  if [ "$opt" = "naive" ]; then
    racket test-no-opts.rkt $dir | tee $dir-$opt.out;
    racket count-time-spent.rkt $dir-$opt.out;
  elif [ "$opt" = "rw" ]
  then
    racket test-no-load.rkt $dir | tee $dir-$opt.out;
    racket count-time-spent.rkt $dir-$opt.out;
  elif [ "$opt" = "pld" ]
  then
    racket test-all-opts.rkt $dir | tee $dir-$opt.out;
    racket count-time-spent.rkt $dir-$opt.out;
  else
    echo "Invalid optimization option"
  fi
fi
