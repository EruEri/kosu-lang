Arithmetic test
  $ kosuc -t x86_64 --cc --no-std test_arith.kosu && ./a.out
  s32: 3 < 5 : true
  s32: 3 <= 5 : true
  s32: 3 > 5 : false
  s32: 3 >= 5 : false
  s32: 3 == 5 : false
  s32: 3 != 5 : true
