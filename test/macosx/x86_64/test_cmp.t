A random test
  $ kosuc -t x86_64m --no-std -o main test_cmp.kosu && ./main
  s8: 0 < 2 : true
  s8: 0 <= 2 : true
  s8: 0 > 2 : false
  s8: 0 >= 2 : false
  s8: 0 == 2 : false
  s8: 0 != 2 : true
  
  s8: 65 < 87 : true
  s8: 65 <= 87 : true
  s8: 65 > 87 : false
  s8: 65 >= 87 : false
  s8: 65 == 87 : false
  s8: 65 != 87 : true
  
  s8: 4 < 4 : false
  s8: 4 <= 4 : true
  s8: 4 > 4 : false
  s8: 4 >= 4 : true
  s8: 4 == 4 : true
  s8: 4 != 4 : false
  
  s8: 0 < 0 : false
  s8: 0 <= 0 : true
  s8: 0 > 0 : false
  s8: 0 >= 0 : true
  s8: 0 == 0 : true
  s8: 0 != 0 : false
  
  s8: 1 < 1 : false
  s8: 1 <= 1 : true
  s8: 1 > 1 : false
  s8: 1 >= 1 : true
  s8: 1 == 1 : true
  s8: 1 != 1 : false
  
  s8: -7 < -7 : false
  s8: -7 <= -7 : true
  s8: -7 > -7 : false
  s8: -7 >= -7 : true
  s8: -7 == -7 : true
  s8: -7 != -7 : false
  
  s8: -5 < -7 : false
  s8: -5 <= -7 : false
  s8: -5 > -7 : true
  s8: -5 >= -7 : true
  s8: -5 == -7 : false
  s8: -5 != -7 : true
  
  u8: 0 < 2 : true
  u8: 0 <= 2 : true
  u8: 0 > 2 : false
  u8: 0 >= 2 : false
  u8: 0 == 2 : false
  u8: 0 != 2 : true
  
  u8: 65 < 87 : true
  u8: 65 <= 87 : true
  u8: 65 > 87 : false
  u8: 65 >= 87 : false
  u8: 65 == 87 : false
  u8: 65 != 87 : true
  
  u8: 4 < 4 : false
  u8: 4 <= 4 : true
  u8: 4 > 4 : false
  u8: 4 >= 4 : true
  u8: 4 == 4 : true
  u8: 4 != 4 : false
  
  u8: 78 < 147 : true
  u8: 78 <= 147 : true
  u8: 78 > 147 : false
  u8: 78 >= 147 : false
  u8: 78 == 147 : false
  u8: 78 != 147 : true
  
  u8: 0 < 0 : false
  u8: 0 <= 0 : true
  u8: 0 > 0 : false
  u8: 0 >= 0 : true
  u8: 0 == 0 : true
  u8: 0 != 0 : false
  
  u8: 1 < 1 : false
  u8: 1 <= 1 : true
  u8: 1 > 1 : false
  u8: 1 >= 1 : true
  u8: 1 == 1 : true
  u8: 1 != 1 : false
  
  s16: 0 < 2 : true
  s16: 0 <= 2 : true
  s16: 0 > 2 : false
  s16: 0 >= 2 : false
  s16: 0 == 2 : false
  s16: 0 != 2 : true
  
  s16: 65 < 87 : true
  s16: 65 <= 87 : true
  s16: 65 > 87 : false
  s16: 65 >= 87 : false
  s16: 65 == 87 : false
  s16: 65 != 87 : true
  
  s16: 4 < 4 : false
  s16: 4 <= 4 : true
  s16: 4 > 4 : false
  s16: 4 >= 4 : true
  s16: 4 == 4 : true
  s16: 4 != 4 : false
  
  s16: 78 < 147 : true
  s16: 78 <= 147 : true
  s16: 78 > 147 : false
  s16: 78 >= 147 : false
  s16: 78 == 147 : false
  s16: 78 != 147 : true
  
  s16: 0 < 0 : false
  s16: 0 <= 0 : true
  s16: 0 > 0 : false
  s16: 0 >= 0 : true
  s16: 0 == 0 : true
  s16: 0 != 0 : false
  
  s16: 1 < 1 : false
  s16: 1 <= 1 : true
  s16: 1 > 1 : false
  s16: 1 >= 1 : true
  s16: 1 == 1 : true
  s16: 1 != 1 : false
  
  u16: 0 < 2 : true
  u16: 0 <= 2 : true
  u16: 0 > 2 : false
  u16: 0 >= 2 : false
  u16: 0 == 2 : false
  u16: 0 != 2 : true
  
  u16: 65 < 87 : true
  u16: 65 <= 87 : true
  u16: 65 > 87 : false
  u16: 65 >= 87 : false
  u16: 65 == 87 : false
  u16: 65 != 87 : true
  
  u16: 4 < 4 : false
  u16: 4 <= 4 : true
  u16: 4 > 4 : false
  u16: 4 >= 4 : true
  u16: 4 == 4 : true
  u16: 4 != 4 : false
  
  u16: 78 < 147 : true
  u16: 78 <= 147 : true
  u16: 78 > 147 : false
  u16: 78 >= 147 : false
  u16: 78 == 147 : false
  u16: 78 != 147 : true
  
  u16: 0 < 0 : false
  u16: 0 <= 0 : true
  u16: 0 > 0 : false
  u16: 0 >= 0 : true
  u16: 0 == 0 : true
  u16: 0 != 0 : false
  
  u16: 1 < 1 : false
  u16: 1 <= 1 : true
  u16: 1 > 1 : false
  u16: 1 >= 1 : true
  u16: 1 == 1 : true
  u16: 1 != 1 : false
  
  s32: 0 < 2 : true
  s32: 0 <= 2 : true
  s32: 0 > 2 : false
  s32: 0 >= 2 : false
  s32: 0 == 2 : false
  s32: 0 != 2 : true
  
  s32: 65 < 87 : true
  s32: 65 <= 87 : true
  s32: 65 > 87 : false
  s32: 65 >= 87 : false
  s32: 65 == 87 : false
  s32: 65 != 87 : true
  
  s32: 4 < 4 : false
  s32: 4 <= 4 : true
  s32: 4 > 4 : false
  s32: 4 >= 4 : true
  s32: 4 == 4 : true
  s32: 4 != 4 : false
  
  s32: 78 < 147 : true
  s32: 78 <= 147 : true
  s32: 78 > 147 : false
  s32: 78 >= 147 : false
  s32: 78 == 147 : false
  s32: 78 != 147 : true
  
  s32: 0 < 0 : false
  s32: 0 <= 0 : true
  s32: 0 > 0 : false
  s32: 0 >= 0 : true
  s32: 0 == 0 : true
  s32: 0 != 0 : false
  
  s32: 1 < 1 : false
  s32: 1 <= 1 : true
  s32: 1 > 1 : false
  s32: 1 >= 1 : true
  s32: 1 == 1 : true
  s32: 1 != 1 : false
  
