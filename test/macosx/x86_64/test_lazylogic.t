# Test laziness of or and and
  $ kosuc --arch x86_64 --os macos --no-std test_lazylogic.kosu && ./a.out
  Always called
  expected "true", found "true"
  expected "true", found "true"
  Always called
  expected "false", found "false"
  expected "true", found "true"
