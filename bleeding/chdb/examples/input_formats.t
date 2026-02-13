Input format ingestion and roundtrip tests

  $ ./input_formats.exe
  === CSV Input ===
  1	Alice	95.5
  2	Bob	87.3
  3	Carol	92.1
  
  === TSV Input ===
  10	Dave	88
  11	Eve	91.5
  12	Frank	76.2
  
  === JSONEachRow Input ===
  20	Grace	99
  21	Hank	85.5
  22	Iris	93.7
  
  === JSONCompactEachRow Input ===
  30	Jack	82
  31	Kate	94.5
  
  === Roundtrip (JSONEachRow) ===
  Exported:
  {"id":1,"name":"A","score":10}
  {"id":2,"name":"B","score":20}
  1	A	10
  2	B	20
  
  === Roundtrip (CSV) ===
  Exported:
  5,"X",50
  6,"Y",60
  5	X	50
  6	Y	60





