Pointer deref Test
  $ kosuc --arch x86_64 --os linux --cc --no-std test_deref.kosu && ./a.out
  expected false : get false
  expected true : get true
  expected 567: get 567
