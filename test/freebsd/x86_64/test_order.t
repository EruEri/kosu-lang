Test Ordered type and function
  $ kosuc --arch x86_64 --os freebsd --no-std -o main test_order.kosu && ./main
  Compare coordinate by x then y
  
  (x: 10, y: 20) <=> (x: 9, y: 19) : gt
  (x: 10, y: 20) == (x: 9, y: 19) : false
  (x: 10, y: 20) != (x: 9, y: 19) : true
  (x: 10, y: 20) > (x: 9, y: 19) : true
  (x: 10, y: 20) >= (x: 9, y: 19) : true
  (x: 10, y: 20) < (x: 9, y: 19) : false
  (x: 10, y: 20) <= (x: 9, y: 19) : false
  
  (x: 10, y: 20) <=> (x: 10, y: 20) : eq
  (x: 10, y: 20) == (x: 10, y: 20) : true
  (x: 10, y: 20) != (x: 10, y: 20) : false
  (x: 10, y: 20) > (x: 10, y: 20) : false
  (x: 10, y: 20) >= (x: 10, y: 20) : true
  (x: 10, y: 20) < (x: 10, y: 20) : false
  (x: 10, y: 20) <= (x: 10, y: 20) : true
  
  (x: 11, y: 18) <=> (x: 10, y: 20) : gt
  (x: 11, y: 18) == (x: 10, y: 20) : false
  (x: 11, y: 18) != (x: 10, y: 20) : true
  (x: 11, y: 18) > (x: 10, y: 20) : true
  (x: 11, y: 18) >= (x: 10, y: 20) : true
  (x: 11, y: 18) < (x: 10, y: 20) : false
  (x: 11, y: 18) <= (x: 10, y: 20) : false
  
  (x: 10, y: 20) <=> (x: 11, y: 18) : lt
  (x: 10, y: 20) == (x: 11, y: 18) : false
  (x: 10, y: 20) != (x: 11, y: 18) : true
  (x: 10, y: 20) > (x: 11, y: 18) : false
  (x: 10, y: 20) >= (x: 11, y: 18) : false
  (x: 10, y: 20) < (x: 11, y: 18) : true
  (x: 10, y: 20) <= (x: 11, y: 18) : true
  
  (x: 10, y: 20) <=> (x: 8, y: 21) : gt
  (x: 10, y: 20) == (x: 8, y: 21) : false
  (x: 10, y: 20) != (x: 8, y: 21) : true
  (x: 10, y: 20) > (x: 8, y: 21) : true
  (x: 10, y: 20) >= (x: 8, y: 21) : true
  (x: 10, y: 20) < (x: 8, y: 21) : false
  (x: 10, y: 20) <= (x: 8, y: 21) : false
  
