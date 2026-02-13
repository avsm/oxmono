Zero-copy binary access patterns

  $ ./zero_copy.exe
  === Zero-Copy UInt64 ===
  Buffer size: 80 bytes
    [0] = 0
    [1] = 1
    [2] = 2
    [3] = 3
    [4] = 4
    [5] = 5
    [6] = 6
    [7] = 7
    [8] = 8
    [9] = 9
  
  === Zero-Copy Sum (1M rows) ===
  Sum of 0..999999 = 499999500000
  Expected:          499999500000
  
  === Query Statistics ===
  Elapsed > 0: true
  Rows read: 100000
  Bytes read: 800000
  
  === String vs Bigstring ===
  String length: 10
  Buffer length: 10
  Equal lengths: true



