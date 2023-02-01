A random test
  $ kosuc -t arm64e --no-std -o main test_cmp.kosu && ./main
  s32: 3 < 5 : true
  s32: 3 <= 5 : true
  s32: 3 > 5 : false
  s32: 3 >= 5 : false
  s32: 3 == 5 : false
  s32: 3 != 5 : true
