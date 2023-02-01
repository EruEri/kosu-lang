Arithmetic test
  $ kosuc -t arm64e --no-std test_arith.kosu && ./a.out
  s32: 3 < 5 : true
  s32: 3 <= 5 : true
  s32: 3 > 5 : false
  s32: 3 >= 5 : false
  s32: 3 == 5 : false
  s32: 3 != 5 : true
