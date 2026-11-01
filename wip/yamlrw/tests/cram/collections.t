Test collections_block.yml - Block style collections

  $ yamlcat collections_block.yml
  simple_sequence:
    - apple
    - banana
    - cherry
    - date
  simple_mapping:
    name: John Doe
    age: 30
    city: New York
    country: USA
  nested_sequences:
    - 
      - alpha
      - beta
      - gamma
    - 
      - one
      - two
      - three
    - 
      - red
      - green
      - blue
  nested_mappings:
    person:
      name: Alice
      contact:
        email: alice@example.com
        phone: 555-1234
      address:
        street: 123 Main St
        city: Boston
  mapping_with_sequences:
    colors:
      - red
      - green
      - blue
    sizes:
      - small
      - medium
      - large
    numbers:
      - 1
      - 2
      - 3
  sequence_with_mappings:
    - name: Alice
      age: 25
      role: developer
    - name: Bob
      age: 30
      role: designer
    - name: Charlie
      age: 35
      role: manager
  deep_nesting:
    level1:
      level2:
        level3:
          level4:
            - deeply
            - nested
            - values
          another_key: value
        items:
          - item1
          - item2
      metadata:
        created: 2024-01-01
        modified: 2024-12-04
  complex_structure:
    database:
      connections:
        - host: db1.example.com
          port: 5432
          credentials:
            username: admin
            password: secret
        - host: db2.example.com
          port: 5432
          credentials:
            username: readonly
            password: public
    services:
      - name: api
        endpoints:
          - /users
          - /posts
          - /comments
        config:
          timeout: 30
          retries: 3
      - name: worker
        tasks:
          - email
          - reports
        config:
          concurrency: 10
  empty_collections:
    empty_sequence: []
    empty_mapping: {}
    sequence_with_empty:
      - value1
      - []
      - value2
    mapping_with_empty:
      key1: value1
      key2: {}
      key3: value3

Test collections_block.yml with JSON output

  $ yamlcat --json collections_block.yml
  {"simple_sequence": ["apple", "banana", "cherry", "date"], "simple_mapping": {"name": "John Doe", "age": 30, "city": "New York", "country": "USA"}, "nested_sequences": [["alpha", "beta", "gamma"], ["one", "two", "three"], ["red", "green", "blue"]], "nested_mappings": {"person": {"name": "Alice", "contact": {"email": "alice@example.com", "phone": "555-1234"}, "address": {"street": "123 Main St", "city": "Boston"}}}, "mapping_with_sequences": {"colors": ["red", "green", "blue"], "sizes": ["small", "medium", "large"], "numbers": [1, 2, 3]}, "sequence_with_mappings": [{"name": "Alice", "age": 25, "role": "developer"}, {"name": "Bob", "age": 30, "role": "designer"}, {"name": "Charlie", "age": 35, "role": "manager"}], "deep_nesting": {"level1": {"level2": {"level3": {"level4": ["deeply", "nested", "values"], "another_key": "value"}, "items": ["item1", "item2"]}, "metadata": {"created": "2024-01-01", "modified": "2024-12-04"}}}, "complex_structure": {"database": {"connections": [{"host": "db1.example.com", "port": 5432, "credentials": {"username": "admin", "password": "secret"}}, {"host": "db2.example.com", "port": 5432, "credentials": {"username": "readonly", "password": "public"}}]}, "services": [{"name": "api", "endpoints": ["/users", "/posts", "/comments"], "config": {"timeout": 30, "retries": 3}}, {"name": "worker", "tasks": ["email", "reports"], "config": {"concurrency": 10}}]}, "empty_collections": {"empty_sequence": [], "empty_mapping": {}, "sequence_with_empty": ["value1", [], "value2"], "mapping_with_empty": {"key1": "value1", "key2": {}, "key3": "value3"}}}

Test collections_block.yml with flow output

  $ yamlcat --flow collections_block.yml
  {simple_sequence: [apple, banana, cherry, date], simple_mapping: {name: John Doe, age: 30, city: New York, country: USA}, nested_sequences: [[alpha, beta, gamma], [one, two, three], [red, green, blue]], nested_mappings: {person: {name: Alice, contact: {email: alice@example.com, phone: 555-1234}, address: {street: 123 Main St, city: Boston}}}, mapping_with_sequences: {colors: [red, green, blue], sizes: [small, medium, large], numbers: [1, 2, 3]}, sequence_with_mappings: [{name: Alice, age: 25, role: developer}, {name: Bob, age: 30, role: designer}, {name: Charlie, age: 35, role: manager}], deep_nesting: {level1: {level2: {level3: {level4: [deeply, nested, values], another_key: value}, items: [item1, item2]}, metadata: {created: 2024-01-01, modified: 2024-12-04}}}, complex_structure: {database: {connections: [{host: db1.example.com, port: 5432, credentials: {username: admin, password: secret}}, {host: db2.example.com, port: 5432, credentials: {username: readonly, password: public}}]}, services: [{name: api, endpoints: [/users, /posts, /comments], config: {timeout: 30, retries: 3}}, {name: worker, tasks: [email, reports], config: {concurrency: 10}}]}, empty_collections: {empty_sequence: [], empty_mapping: {}, sequence_with_empty: [value1, [], value2], mapping_with_empty: {key1: value1, key2: {}, key3: value3}}}

Test collections_compact.yml - Compact notation

  $ yamlcat collections_compact.yml
  compact_sequence:
    - name: Alice
      age: 25
      city: Boston
    - name: Bob
      age: 30
      city: Seattle
    - name: Charlie
      age: 35
      city: Portland
  compact_nested:
    - id: 1
      details:
        type: admin
        permissions:
          - read
          - write
          - delete
    - id: 2
      details:
        type: user
        permissions:
          - read
  compact_complex:
    - key1: value1
      key2: value2
      nested:
        sub1: val1
        sub2: val2
    - key1: value3
      key2: value4
      nested:
        sub1: val3
        sub2: val4
  users:
    - username: alice
      email: alice@example.com
      active: true
    - username: bob
      email: bob@example.com
      active: false
  compact_with_flow:
    - name: service1
      ports:
        - 8080
        - 8443
      env:
        DEBUG: true
        MODE: production
    - name: service2
      ports:
        - 3000
      env:
        DEBUG: false
        MODE: development
  deep_compact:
    - category: electronics
      items:
        - name: laptop
          specs:
            cpu: Intel i7
            ram: 16GB
            storage: 512GB SSD
        - name: phone
          specs:
            os: Android
            ram: 8GB
            storage: 256GB
    - category: furniture
      items:
        - name: desk
          dimensions:
            width: 150cm
            depth: 75cm
            height: 75cm
        - name: chair
          dimensions:
            width: 60cm
            depth: 60cm
            height: 120cm
  mixed_compact:
    databases:
      - type: postgresql
        connection:
          host: localhost
          port: 5432
        credentials:
          user: admin
          password: secret
      - type: mongodb
        connection:
          host: localhost
          port: 27017
        credentials:
          user: root
          password: root
  single_line_compact:
    - name: Alice
      age: 25
      role: developer
    - name: Bob
      age: 30
      role: designer
    - name: Charlie
      age: 35
      role: manager
  sequences_in_compact:
    - title: Project A
      members:
        - Alice
        - Bob
        - Charlie
      tags:
        - urgent
        - backend
    - title: Project B
      members:
        - David
        - Eve
      tags:
        - frontend
        - design
  compact_with_empty:
    - id: 1
      data: []
      meta: {}
    - id: 2
      data:
        - item1
      meta:
        key: value
  compact_complex_nesting:
    - level: 1
      children:
        - level: 2a
          children:
            - level: 3a
              value: leaf1
            - level: 3b
              value: leaf2
        - level: 2b
          children:
            - level: 3c
              value: leaf3
  api_endpoints:
    - path: /users
      method: GET
      auth: required
      params:
        - name: page
          type: integer
          default: 1
        - name: limit
          type: integer
          default: 10
    - path: '/users/:id'
      method: GET
      auth: required
      params: []
    - path: /users
      method: POST
      auth: required
      body:
        username: string
        email: string
        password: string
  compact_types:
    - string_val: hello
      number_val: 42
      float_val: 3.14
      bool_val: true
      null_val: null
    - string_val: world
      number_val: 100
      float_val: 2.71
      bool_val: false
      null_val: null
  minimal:
    - a: 1
    - b: 2
    - c: 3

Test collections_compact.yml with JSON output

  $ yamlcat --json collections_compact.yml
  {"compact_sequence": [{"name": "Alice", "age": 25, "city": "Boston"}, {"name": "Bob", "age": 30, "city": "Seattle"}, {"name": "Charlie", "age": 35, "city": "Portland"}], "compact_nested": [{"id": 1, "details": {"type": "admin", "permissions": ["read", "write", "delete"]}}, {"id": 2, "details": {"type": "user", "permissions": ["read"]}}], "compact_complex": [{"key1": "value1", "key2": "value2", "nested": {"sub1": "val1", "sub2": "val2"}}, {"key1": "value3", "key2": "value4", "nested": {"sub1": "val3", "sub2": "val4"}}], "users": [{"username": "alice", "email": "alice@example.com", "active": true}, {"username": "bob", "email": "bob@example.com", "active": false}], "compact_with_flow": [{"name": "service1", "ports": [8080, 8443], "env": {"DEBUG": true, "MODE": "production"}}, {"name": "service2", "ports": [3000], "env": {"DEBUG": false, "MODE": "development"}}], "deep_compact": [{"category": "electronics", "items": [{"name": "laptop", "specs": {"cpu": "Intel i7", "ram": "16GB", "storage": "512GB SSD"}}, {"name": "phone", "specs": {"os": "Android", "ram": "8GB", "storage": "256GB"}}]}, {"category": "furniture", "items": [{"name": "desk", "dimensions": {"width": "150cm", "depth": "75cm", "height": "75cm"}}, {"name": "chair", "dimensions": {"width": "60cm", "depth": "60cm", "height": "120cm"}}]}], "mixed_compact": {"databases": [{"type": "postgresql", "connection": {"host": "localhost", "port": 5432}, "credentials": {"user": "admin", "password": "secret"}}, {"type": "mongodb", "connection": {"host": "localhost", "port": 27017}, "credentials": {"user": "root", "password": "root"}}]}, "single_line_compact": [{"name": "Alice", "age": 25, "role": "developer"}, {"name": "Bob", "age": 30, "role": "designer"}, {"name": "Charlie", "age": 35, "role": "manager"}], "sequences_in_compact": [{"title": "Project A", "members": ["Alice", "Bob", "Charlie"], "tags": ["urgent", "backend"]}, {"title": "Project B", "members": ["David", "Eve"], "tags": ["frontend", "design"]}], "compact_with_empty": [{"id": 1, "data": [], "meta": {}}, {"id": 2, "data": ["item1"], "meta": {"key": "value"}}], "compact_complex_nesting": [{"level": 1, "children": [{"level": "2a", "children": [{"level": "3a", "value": "leaf1"}, {"level": "3b", "value": "leaf2"}]}, {"level": "2b", "children": [{"level": "3c", "value": "leaf3"}]}]}], "api_endpoints": [{"path": "/users", "method": "GET", "auth": "required", "params": [{"name": "page", "type": "integer", "default": 1}, {"name": "limit", "type": "integer", "default": 10}]}, {"path": "/users/:id", "method": "GET", "auth": "required", "params": []}, {"path": "/users", "method": "POST", "auth": "required", "body": {"username": "string", "email": "string", "password": "string"}}], "compact_types": [{"string_val": "hello", "number_val": 42, "float_val": 3.14, "bool_val": true, "null_val": null}, {"string_val": "world", "number_val": 100, "float_val": 2.71, "bool_val": false, "null_val": null}], "minimal": [{"a": 1}, {"b": 2}, {"c": 3}]}

Test collections_compact.yml with flow output

  $ yamlcat --flow collections_compact.yml
  {compact_sequence: [{name: Alice, age: 25, city: Boston}, {name: Bob, age: 30, city: Seattle}, {name: Charlie, age: 35, city: Portland}], compact_nested: [{id: 1, details: {type: admin, permissions: [read, write, delete]}}, {id: 2, details: {type: user, permissions: [read]}}], compact_complex: [{key1: value1, key2: value2, nested: {sub1: val1, sub2: val2}}, {key1: value3, key2: value4, nested: {sub1: val3, sub2: val4}}], users: [{username: alice, email: alice@example.com, active: true}, {username: bob, email: bob@example.com, active: false}], compact_with_flow: [{name: service1, ports: [8080, 8443], env: {DEBUG: true, MODE: production}}, {name: service2, ports: [3000], env: {DEBUG: false, MODE: development}}], deep_compact: [{category: electronics, items: [{name: laptop, specs: {cpu: Intel i7, ram: 16GB, storage: 512GB SSD}}, {name: phone, specs: {os: Android, ram: 8GB, storage: 256GB}}]}, {category: furniture, items: [{name: desk, dimensions: {width: 150cm, depth: 75cm, height: 75cm}}, {name: chair, dimensions: {width: 60cm, depth: 60cm, height: 120cm}}]}], mixed_compact: {databases: [{type: postgresql, connection: {host: localhost, port: 5432}, credentials: {user: admin, password: secret}}, {type: mongodb, connection: {host: localhost, port: 27017}, credentials: {user: root, password: root}}]}, single_line_compact: [{name: Alice, age: 25, role: developer}, {name: Bob, age: 30, role: designer}, {name: Charlie, age: 35, role: manager}], sequences_in_compact: [{title: Project A, members: [Alice, Bob, Charlie], tags: [urgent, backend]}, {title: Project B, members: [David, Eve], tags: [frontend, design]}], compact_with_empty: [{id: 1, data: [], meta: {}}, {id: 2, data: [item1], meta: {key: value}}], compact_complex_nesting: [{level: 1, children: [{level: 2a, children: [{level: 3a, value: leaf1}, {level: 3b, value: leaf2}]}, {level: 2b, children: [{level: 3c, value: leaf3}]}]}], api_endpoints: [{path: /users, method: GET, auth: required, params: [{name: page, type: integer, default: 1}, {name: limit, type: integer, default: 10}]}, {path: '/users/:id', method: GET, auth: required, params: []}, {path: /users, method: POST, auth: required, body: {username: string, email: string, password: string}}], compact_types: [{string_val: hello, number_val: 42, float_val: 3.14, bool_val: true, null_val: null}, {string_val: world, number_val: 100, float_val: 2.71, bool_val: false, null_val: null}], minimal: [{a: 1}, {b: 2}, {c: 3}]}

Test collections_flow.yml - Flow style collections

  $ yamlcat collections_flow.yml
  simple_flow_sequence:
    - apple
    - banana
    - cherry
    - date
  simple_flow_mapping:
    name: John
    age: 30
    city: New York
  nested_flow_sequences:
    - 
      - a
      - b
      - c
    - 
      - 1
      - 2
      - 3
    - 
      - red
      - green
      - blue
  nested_flow_mappings:
    person:
      name: Alice
      age: 25
    contact:
      email: alice@example.com
      phone: 555-1234
  flow_seq_with_maps:
    - name: Alice
      role: dev
    - name: Bob
      role: ops
    - name: Charlie
      role: qa
  flow_map_with_seqs:
    colors:
      - red
      - green
      - blue
    sizes:
      - S
      - M
      - L
    numbers:
      - 1
      - 2
      - 3
  deep_flow_nesting:
    level1:
      level2:
        level3:
          level4:
            - a
            - b
            - c
  empty_flow:
    empty_seq: []
    empty_map: {}
    both:
      - []
      - {}
  flow_in_block:
    sequence:
      - 1
      - 2
      - 3
      - 4
      - 5
    mapping:
      a: 1
      b: 2
      c: 3
    nested:
      items:
        - x
        - y
        - z
      config:
        timeout: 30
        retries: 3
  block_in_flow:
    users:
      - name: Alice
        tags:
          - dev
          - senior
      - name: Bob
        tags:
          - ops
          - junior
  mixed_structure:
    services:
      - name: api
        ports:
          - 8080
          - 8443
        env:
          DEBUG: true
          LOG_LEVEL: info
      - name: db
        ports:
          - 5432
        env:
          POSTGRES_DB: mydb
          POSTGRES_USER: admin
    config:
      version: 1
      enabled: true
  flow_types:
    strings:
      - hello
      - world
      - foo
      - bar
    numbers:
      - 1
      - 2
      - 3
      - 42
      - 100
    mixed:
      - string
      - 123
      - true
      - false
      - null
    quoted:
      - with spaces
      - 'special:chars'
      - commas, here
  flow_map_types:
    string: value
    number: 42
    boolean: true
    null_value: null
    float: 3.14
  nested_mixed:
    - id: 1
      data:
        - a
        - b
        - c
      meta:
        type: first
    - id: 2
      data:
        - d
        - e
        - f
      meta:
        type: second
    - id: 3
      data:
        - g
        - h
        - i
      meta:
        type: third
  multiline_flow:
    long_sequence:
      - item1
      - item2
      - item3
      - item4
    long_mapping:
      key1: value1
      key2: value2
      key3: value3
  edge_cases:
    single_item_seq:
      - alone
    single_item_map:
      only: one
    nested_empty:
      - []
      - 
        - {}
      - 
        - {}
        - []
    all_empty:
      - {}
      - []
      - a: []
      - b: {}

Test collections_flow.yml with JSON output

  $ yamlcat --json collections_flow.yml
  {"simple_flow_sequence": ["apple", "banana", "cherry", "date"], "simple_flow_mapping": {"name": "John", "age": 30, "city": "New York"}, "nested_flow_sequences": [["a", "b", "c"], [1, 2, 3], ["red", "green", "blue"]], "nested_flow_mappings": {"person": {"name": "Alice", "age": 25}, "contact": {"email": "alice@example.com", "phone": "555-1234"}}, "flow_seq_with_maps": [{"name": "Alice", "role": "dev"}, {"name": "Bob", "role": "ops"}, {"name": "Charlie", "role": "qa"}], "flow_map_with_seqs": {"colors": ["red", "green", "blue"], "sizes": ["S", "M", "L"], "numbers": [1, 2, 3]}, "deep_flow_nesting": {"level1": {"level2": {"level3": {"level4": ["a", "b", "c"]}}}}, "empty_flow": {"empty_seq": [], "empty_map": {}, "both": [[], {}]}, "flow_in_block": {"sequence": [1, 2, 3, 4, 5], "mapping": {"a": 1, "b": 2, "c": 3}, "nested": {"items": ["x", "y", "z"], "config": {"timeout": 30, "retries": 3}}}, "block_in_flow": {"users": [{"name": "Alice", "tags": ["dev", "senior"]}, {"name": "Bob", "tags": ["ops", "junior"]}]}, "mixed_structure": {"services": [{"name": "api", "ports": [8080, 8443], "env": {"DEBUG": true, "LOG_LEVEL": "info"}}, {"name": "db", "ports": [5432], "env": {"POSTGRES_DB": "mydb", "POSTGRES_USER": "admin"}}], "config": {"version": 1, "enabled": true}}, "flow_types": {"strings": ["hello", "world", "foo", "bar"], "numbers": [1, 2, 3, 42, 100], "mixed": ["string", 123, true, false, null], "quoted": ["with spaces", "special:chars", "commas, here"]}, "flow_map_types": {"string": "value", "number": 42, "boolean": true, "null_value": null, "float": 3.14}, "nested_mixed": [{"id": 1, "data": ["a", "b", "c"], "meta": {"type": "first"}}, {"id": 2, "data": ["d", "e", "f"], "meta": {"type": "second"}}, {"id": 3, "data": ["g", "h", "i"], "meta": {"type": "third"}}], "multiline_flow": {"long_sequence": ["item1", "item2", "item3", "item4"], "long_mapping": {"key1": "value1", "key2": "value2", "key3": "value3"}}, "edge_cases": {"single_item_seq": ["alone"], "single_item_map": {"only": "one"}, "nested_empty": [[], [{}], [{}, []]], "all_empty": [{}, [], {"a": []}, {"b": {}}]}}

Test collections_flow.yml with flow output

  $ yamlcat --flow collections_flow.yml
  {simple_flow_sequence: [apple, banana, cherry, date], simple_flow_mapping: {name: John, age: 30, city: New York}, nested_flow_sequences: [[a, b, c], [1, 2, 3], [red, green, blue]], nested_flow_mappings: {person: {name: Alice, age: 25}, contact: {email: alice@example.com, phone: 555-1234}}, flow_seq_with_maps: [{name: Alice, role: dev}, {name: Bob, role: ops}, {name: Charlie, role: qa}], flow_map_with_seqs: {colors: [red, green, blue], sizes: [S, M, L], numbers: [1, 2, 3]}, deep_flow_nesting: {level1: {level2: {level3: {level4: [a, b, c]}}}}, empty_flow: {empty_seq: [], empty_map: {}, both: [[], {}]}, flow_in_block: {sequence: [1, 2, 3, 4, 5], mapping: {a: 1, b: 2, c: 3}, nested: {items: [x, y, z], config: {timeout: 30, retries: 3}}}, block_in_flow: {users: [{name: Alice, tags: [dev, senior]}, {name: Bob, tags: [ops, junior]}]}, mixed_structure: {services: [{name: api, ports: [8080, 8443], env: {DEBUG: true, LOG_LEVEL: info}}, {name: db, ports: [5432], env: {POSTGRES_DB: mydb, POSTGRES_USER: admin}}], config: {version: 1, enabled: true}}, flow_types: {strings: [hello, world, foo, bar], numbers: [1, 2, 3, 42, 100], mixed: [string, 123, true, false, null], quoted: [with spaces, 'special:chars', commas, here]}, flow_map_types: {string: value, number: 42, boolean: true, null_value: null, float: 3.14}, nested_mixed: [{id: 1, data: [a, b, c], meta: {type: first}}, {id: 2, data: [d, e, f], meta: {type: second}}, {id: 3, data: [g, h, i], meta: {type: third}}], multiline_flow: {long_sequence: [item1, item2, item3, item4], long_mapping: {key1: value1, key2: value2, key3: value3}}, edge_cases: {single_item_seq: [alone], single_item_map: {only: one}, nested_empty: [[], [{}], [{}, []]], all_empty: [{}, [], {a: []}, {b: {}}]}}

Inline test: Simple sequence

  $ echo '- a
  > - b
  > - c' | yamlcat
  - a
  - b
  - c

  $ echo '- a
  > - b
  > - c' | yamlcat --json
  ["a", "b", "c"]

  $ echo '- a
  > - b
  > - c' | yamlcat --flow
  [a, b, c]

Inline test: Simple mapping

  $ echo 'key1: value1
  > key2: value2
  > key3: value3' | yamlcat
  key1: value1
  key2: value2
  key3: value3

  $ echo 'key1: value1
  > key2: value2
  > key3: value3' | yamlcat --json
  {"key1": "value1", "key2": "value2", "key3": "value3"}

  $ echo 'key1: value1
  > key2: value2
  > key3: value3' | yamlcat --flow
  {key1: value1, key2: value2, key3: value3}

Inline test: Nested sequences

  $ echo 'outer:
  >   - - inner1
  >     - inner2
  >   - - inner3
  >     - inner4' | yamlcat
  outer:
    - 
      - inner1
      - inner2
    - 
      - inner3
      - inner4

  $ echo 'outer:
  >   - - inner1
  >     - inner2
  >   - - inner3
  >     - inner4' | yamlcat --json
  {"outer": [["inner1", "inner2"], ["inner3", "inner4"]]}

  $ echo 'outer:
  >   - - inner1
  >     - inner2
  >   - - inner3
  >     - inner4' | yamlcat --flow
  {outer: [[inner1, inner2], [inner3, inner4]]}

Inline test: Nested mappings

  $ echo 'level1:
  >   level2:
  >     level3:
  >       key: value' | yamlcat
  level1:
    level2:
      level3:
        key: value

  $ echo 'level1:
  >   level2:
  >     level3:
  >       key: value' | yamlcat --json
  {"level1": {"level2": {"level3": {"key": "value"}}}}

  $ echo 'level1:
  >   level2:
  >     level3:
  >       key: value' | yamlcat --flow
  {level1: {level2: {level3: {key: value}}}}

Inline test: Flow sequence

  $ echo '[a, b, c]' | yamlcat
  - a
  - b
  - c

  $ echo '[a, b, c]' | yamlcat --json
  ["a", "b", "c"]

  $ echo '[a, b, c]' | yamlcat --flow
  [a, b, c]

Inline test: Flow mapping

  $ echo '{a: 1, b: 2, c: 3}' | yamlcat
  a: 1
  b: 2
  c: 3

  $ echo '{a: 1, b: 2, c: 3}' | yamlcat --json
  {"a": 1, "b": 2, "c": 3}

  $ echo '{a: 1, b: 2, c: 3}' | yamlcat --flow
  {a: 1, b: 2, c: 3}

Inline test: Nested flow collections

  $ echo '[[1, 2], [3, 4], [5, 6]]' | yamlcat
  - 
    - 1
    - 2
  - 
    - 3
    - 4
  - 
    - 5
    - 6

  $ echo '[[1, 2], [3, 4], [5, 6]]' | yamlcat --json
  [[1, 2], [3, 4], [5, 6]]

  $ echo '[[1, 2], [3, 4], [5, 6]]' | yamlcat --flow
  [[1, 2], [3, 4], [5, 6]]

Inline test: Flow mapping with nested mapping

  $ echo '{outer: {inner: value}}' | yamlcat
  outer:
    inner: value

  $ echo '{outer: {inner: value}}' | yamlcat --json
  {"outer": {"inner": "value"}}

  $ echo '{outer: {inner: value}}' | yamlcat --flow
  {outer: {inner: value}}

Inline test: Mixed block and flow (flow in block)

  $ echo 'block_key:
  >   flow_seq: [1, 2, 3]
  >   flow_map: {a: 1, b: 2}' | yamlcat
  block_key:
    flow_seq:
      - 1
      - 2
      - 3
    flow_map:
      a: 1
      b: 2

  $ echo 'block_key:
  >   flow_seq: [1, 2, 3]
  >   flow_map: {a: 1, b: 2}' | yamlcat --json
  {"block_key": {"flow_seq": [1, 2, 3], "flow_map": {"a": 1, "b": 2}}}

  $ echo 'block_key:
  >   flow_seq: [1, 2, 3]
  >   flow_map: {a: 1, b: 2}' | yamlcat --flow
  {block_key: {flow_seq: [1, 2, 3], flow_map: {a: 1, b: 2}}}

Inline test: Mixed block and flow (block in flow)

  $ echo '{users: [{name: Alice, age: 30}, {name: Bob, age: 25}]}' | yamlcat
  users:
    - name: Alice
      age: 30
    - name: Bob
      age: 25

  $ echo '{users: [{name: Alice, age: 30}, {name: Bob, age: 25}]}' | yamlcat --json
  {"users": [{"name": "Alice", "age": 30}, {"name": "Bob", "age": 25}]}

  $ echo '{users: [{name: Alice, age: 30}, {name: Bob, age: 25}]}' | yamlcat --flow
  {users: [{name: Alice, age: 30}, {name: Bob, age: 25}]}

Inline test: Compact notation - sequence of mappings

  $ echo '- name: Alice
  >   role: dev
  > - name: Bob
  >   role: ops' | yamlcat
  - name: Alice
    role: dev
  - name: Bob
    role: ops

  $ echo '- name: Alice
  >   role: dev
  > - name: Bob
  >   role: ops' | yamlcat --json
  [{"name": "Alice", "role": "dev"}, {"name": "Bob", "role": "ops"}]

  $ echo '- name: Alice
  >   role: dev
  > - name: Bob
  >   role: ops' | yamlcat --flow
  [{name: Alice, role: dev}, {name: Bob, role: ops}]

Inline test: Compact with nested collections

  $ echo '- id: 1
  >   tags: [a, b, c]
  >   config:
  >     enabled: true
  > - id: 2
  >   tags: [x, y, z]
  >   config:
  >     enabled: false' | yamlcat
  - id: 1
    tags:
      - a
      - b
      - c
    config:
      enabled: true
  - id: 2
    tags:
      - x
      - y
      - z
    config:
      enabled: false

  $ echo '- id: 1
  >   tags: [a, b, c]
  >   config:
  >     enabled: true
  > - id: 2
  >   tags: [x, y, z]
  >   config:
  >     enabled: false' | yamlcat --json
  [{"id": 1, "tags": ["a", "b", "c"], "config": {"enabled": true}}, {"id": 2, "tags": ["x", "y", "z"], "config": {"enabled": false}}]

  $ echo '- id: 1
  >   tags: [a, b, c]
  >   config:
  >     enabled: true
  > - id: 2
  >   tags: [x, y, z]
  >   config:
  >     enabled: false' | yamlcat --flow
  [{id: 1, tags: [a, b, c], config: {enabled: true}}, {id: 2, tags: [x, y, z], config: {enabled: false}}]

Inline test: Empty collections

  $ echo 'empty_seq: []
  > empty_map: {}' | yamlcat
  empty_seq: []
  empty_map: {}

  $ echo 'empty_seq: []
  > empty_map: {}' | yamlcat --json
  {"empty_seq": [], "empty_map": {}}

  $ echo 'empty_seq: []
  > empty_map: {}' | yamlcat --flow
  {empty_seq: [], empty_map: {}}

Inline test: Sequence with mapping values

  $ echo 'items:
  >   - apple
  >   - banana
  > config:
  >   mode: fast' | yamlcat
  items:
    - apple
    - banana
  config:
    mode: fast

  $ echo 'items:
  >   - apple
  >   - banana
  > config:
  >   mode: fast' | yamlcat --json
  {"items": ["apple", "banana"], "config": {"mode": "fast"}}

  $ echo 'items:
  >   - apple
  >   - banana
  > config:
  >   mode: fast' | yamlcat --flow
  {items: [apple, banana], config: {mode: fast}}

Inline test: Complex nested structure

  $ echo 'services:
  >   - name: web
  >     ports:
  >       - 80
  >       - 443
  >     env:
  >       DEBUG: false
  >       MODE: prod' | yamlcat
  services:
    - name: web
      ports:
        - 80
        - 443
      env:
        DEBUG: false
        MODE: prod

  $ echo 'services:
  >   - name: web
  >     ports:
  >       - 80
  >       - 443
  >     env:
  >       DEBUG: false
  >       MODE: prod' | yamlcat --json
  {"services": [{"name": "web", "ports": [80, 443], "env": {"DEBUG": false, "MODE": "prod"}}]}

  $ echo 'services:
  >   - name: web
  >     ports:
  >       - 80
  >       - 443
  >     env:
  >       DEBUG: false
  >       MODE: prod' | yamlcat --flow
  {services: [{name: web, ports: [80, 443], env: {DEBUG: false, MODE: prod}}]}

Inline test: Flow sequence with various types

  $ echo '[string, 42, true, false, null, 3.14]' | yamlcat --json
  ["string", 42, true, false, null, 3.14]

Inline test: Flow mapping with various types

  $ echo '{str: hello, num: 42, bool: true, nil: null, float: 3.14}' | yamlcat --json
  {"str": "hello", "num": 42, "bool": true, "nil": null, "float": 3.14}
