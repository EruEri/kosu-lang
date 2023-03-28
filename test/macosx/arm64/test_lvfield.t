Test Struct field modification
  $ kosuc --arch arm64 --os macos --no-std -o main test_lvfield.kosu && ./main
  (nom : direct, x = 30, y = 20)
  (nom : direct, x = 90, y = 70)
  (nom : indirect, x = 30, y = 20)
  (nom : indirect, x = 90, y = 70)
