Pointer deref Test
  $ kosuc -t arm64e --no-std test_deref.kosu && ./a.out
  expected false : get false
  expected true : get true
  expected 567: get 567
