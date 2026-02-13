Error handling patterns

  $ ./error_handling.exe
  === Valid Query ===
  OK: 42
  
  === Syntax Error ===
  ERROR: Query failed: Code: 62. DB::Exception: Syntax error: failed at position 9 (42): 42. Expected one of: token sequence, Dot, token, OR, AND, IS NOT DISTINCT FROM, IS NULL, IS NOT NULL, BETWEEN, NOT BETWEEN, LIKE, ILIKE, NOT LIKE, NOT ILIKE, REGEXP, IN, NOT IN, GLOBAL IN, GLOBAL NOT IN, MOD, DIV, alias, AS, identifier, Comma, FROM, PREWHERE, WHERE, GROUP BY, WITH, HAVING, WINDOW, QUALIFY, ORDER BY, LIMIT, OFFSET, FETCH, SETTINGS, end of query. (SYNTAX_ERROR)
  
  === Unknown Table ===
  ERROR: Query failed: Code: 60. DB::Exception: Unknown table expression identifier 'nonexistent_table' in scope SELECT * FROM nonexistent_table. (UNKNOWN_TABLE)
  
  === Division By Zero ===
  OK: inf
  
  === Type Error ===
  OK: 0
  
  === Use After Close ===
  ERROR: Use after close





