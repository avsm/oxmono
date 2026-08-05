Anchor and Alias Support

These tests verify that anchor (&) and alias (*) support is working correctly.

Test: Simple scalar anchor and alias

  $ yamlcat anchor_simple.yml 2>&1
  anchor: value
  alias: value

Test: Numeric anchor and alias

  $ yamlcat anchor_number.yml 2>&1
  original: 42
  copy: 42

Test: Sequence anchor and alias

  $ yamlcat anchor_list.yml 2>&1
  list:
    - one
    - two
  copy:
    - one
    - two

Test: Mapping anchor and alias

  $ yamlcat anchor_mapping.yml 2>&1
  person:
    name: Alice
    age: 30
  copy:
    name: Alice
    age: 30

Test: Multiple aliases to same anchor

  $ yamlcat anchor_multiple_refs.yml 2>&1
  value: 100
  first: 100
  second: 100
  third: 100

Test: Anchor in flow context

  $ yamlcat anchor_flow_seq.yml 2>&1
  - apple
  - apple

Test: Anchor with mapping in flow

  $ yamlcat anchor_flow_map.yml 2>&1
  original:
    a: 1
  copy:
    a: 1

Test: Anchors file from test suite

  $ yamlcat anchors_basic.yml 2>&1
  scalar_anchor: Hello, World!
  scalar_alias: Hello, World!
  ---
  original: 42
  copy: 42
  another_copy: 42
  ---
  original_list:
    - apple
    - banana
    - cherry
  copied_list:
    - apple
    - banana
    - cherry
  ---
  original_map:
    name: Alice
    age: 30
    city: London
  copied_map:
    name: Alice
    age: 30
    city: London
  ---
  defaults:
    timeout: 30
    retries: 3
  colors:
    - red
    - green
    - blue
  config:
    settings:
      timeout: 30
      retries: 3
    palette:
      - red
      - green
      - blue
  ---
  template:
    metadata:
      version: 1
      author: John Doe
    settings:
      enabled: true
      debug: false
  instance1:
    metadata:
      version: 1
      author: John Doe
    settings:
      enabled: true
      debug: false
  instance2:
    metadata:
      version: 1
      author: John Doe
    settings:
      enabled: true
      debug: false
  ---
  items:
    - id: 1
      name: First
    - id: 2
      name: Second
    - id: 1
      name: First
  ---
  shared_value: 100
  calculations:
    base: 100
    doubled: 200
    reference: 100
    another_ref: 100
  ---
  feature_flag: true
  features:
    login: true
    signup: true
    export: true
  ---
  empty: null
  values:
    first: null
    second: null
  ---
  message: "This is a multi-line\nmessage with some\nspecial content!\n"
  output1: "This is a multi-line\nmessage with some\nspecial content!\n"
  output2: "This is a multi-line\nmessage with some\nspecial content!\n"
  ---
  database:
    primary:
      host: localhost
      port: 5432
      ssl: true
    replica:
      host: localhost
      port: 5432
      ssl: true
    backup:
      host: localhost
      port: 5432
      ssl: true

  $ yamlcat anchors_merge.yml 2>&1
  defaults:
    timeout: 30
    retries: 3
    verbose: false
  production:
    <<:
      timeout: 30
      retries: 3
      verbose: false
    environment: production
  ---
  base:
    color: red
    size: medium
    weight: 100
  custom:
    <<:
      color: red
      size: medium
      weight: 100
    color: blue
    shape: circle
  ---
  connection:
    host: localhost
    port: 8080
  authentication:
    username: admin
    password: secret
  server:
    <<:
      - host: localhost
        port: 8080
      - username: admin
        password: secret
    ssl: true
  ---
  defaults:
    timeout: 30
    retries: 3
  advanced:
    cache: true
    pool_size: 10
  config:
    <<:
      - timeout: 30
        retries: 3
      - cache: true
        pool_size: 10
    timeout: 60
    custom: value
  ---
  base_style:
    font: Arial
    size: 12
  heading_defaults:
    <<:
      font: Arial
      size: 12
    weight: bold
  main_heading:
    <<:
      <<:
        font: Arial
        size: 12
      weight: bold
    size: 18
    color: navy
  ---
  common:
    enabled: true
    log_level: info
  services:
    - name: web
      <<:
        enabled: true
        log_level: info
      port: 80
    - name: api
      <<:
        enabled: true
        log_level: info
      port: 3000
    - name: worker
      <<:
        enabled: true
        log_level: info
      threads: 4
  ---
  empty: {}
  config:
    <<: {}
    key: value
  ---
  metadata:
    created: 2023-01-01
    author: Admin
    tags:
      - v1
      - stable
  document:
    <<:
      created: 2023-01-01
      author: Admin
      tags:
        - v1
        - stable
    title: Important Document
    content: Some content here
  ---
  level1:
    a: 1
    b: 2
  level2:
    <<:
      a: 1
      b: 2
    c: 3
  level3:
    <<:
      <<:
        a: 1
        b: 2
      c: 3
    d: 4
  ---
  first:
    name: First
    value: 100
    priority: low
  second:
    name: Second
    value: 200
    category: important
  combined:
    <<:
      - name: First
        value: 100
        priority: low
      - name: Second
        value: 200
        category: important
    name: Combined
  ---
  numbers:
    count: 42
    ratio: 3.14
    active: true
  derived:
    <<:
      count: 42
      ratio: 3.14
      active: true
    label: Test
  ---
  db_defaults:
    pool_size: 5
    timeout: 30
    ssl: false
  cache_defaults:
    ttl: 3600
    max_size: 1000
  development:
    database:
      <<:
        pool_size: 5
        timeout: 30
        ssl: false
      host: localhost
      name: dev_db
    cache:
      <<:
        ttl: 3600
        max_size: 1000
      backend: memory
  production:
    database:
      <<:
        pool_size: 5
        timeout: 30
        ssl: false
      host: prod.example.com
      name: prod_db
      ssl: true
      pool_size: 20
    cache:
      <<:
        ttl: 3600
        max_size: 1000
      backend: redis
      ttl: 7200

Note: The anchor test files also use multiple documents, so they fail
with multi-document errors before even hitting anchor issues.
