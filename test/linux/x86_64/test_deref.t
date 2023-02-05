Pointer deref Test
  $ kosuc -t x86_64 --cc --no-std test_deref.kosu && ./a.out
  expected false : get false
  expected true : get true
  expected 567: get 567
