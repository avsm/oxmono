Output format showcase

  $ ./data_formats.exe
  === CSV ===
  1,"item_1",1.5
  2,"item_2",3
  3,"item_3",4.5
  4,"item_4",6
  
  === CSVWithNames ===
  "id","name","price"
  1,"item_1",1.5
  2,"item_2",3
  3,"item_3",4.5
  4,"item_4",6
  
  === TSV ===
  1	item_1	1.5
  2	item_2	3
  3	item_3	4.5
  4	item_4	6
  
  === TSVWithNames ===
  id	name	price
  1	item_1	1.5
  2	item_2	3
  3	item_3	4.5
  4	item_4	6
  
  === JSONEachRow ===
  {"id":1,"name":"item_1","price":1.5}
  {"id":2,"name":"item_2","price":3}
  {"id":3,"name":"item_3","price":4.5}
  {"id":4,"name":"item_4","price":6}
  
  === JSONCompactEachRow ===
  [1, "item_1", 1.5]
  [2, "item_2", 3]
  [3, "item_3", 4.5]
  [4, "item_4", 6]
  
  === Vertical ===
  Row 1:
  ──────
  id:    1
  name:  item_1
  price: 1.5
  
  Row 2:
  ──────
  id:    2
  name:  item_2
  price: 3
  
  Row 3:
  ──────
  id:    3
  name:  item_3
  price: 4.5
  
  Row 4:
  ──────
  id:    4
  name:  item_4
  price: 6
  
  === PrettyCompact ===
     ┌─id─┬─name───┬─price─┐
  1. │  1 │ item_1 │   1.5 │
  2. │  2 │ item_2 │     3 │
  3. │  3 │ item_3 │   4.5 │
  4. │  4 │ item_4 │     6 │
     └────┴────────┴───────┘










