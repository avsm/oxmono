Multi-document stream support

These tests verify that multi-document YAML streams are working correctly.

Test: Two documents separated by ---

  $ echo '---
  > first: document
  > ---
  > second: document' | yamlcat 2>&1
  first: document
  ---
  second: document

Test: Three documents with different types

  $ echo '---
  > mapping: value
  > ---
  > - sequence
  > - items
  > ---
  > scalar value' | yamlcat 2>&1
  mapping: value
  ---
  - sequence
  - items
  ---
  scalar value

Test: Documents with explicit end markers

  $ echo '---
  > doc1: value
  > ...
  > ---
  > doc2: value
  > ...' | yamlcat 2>&1
  doc1: value
  ---
  doc2: value

Test: Empty documents

  $ echo '---
  > ---
  > content: here
  > ---' | yamlcat 2>&1
  null
  ---
  content: here
  ---
  null

Test: Multi-document file

  $ yamlcat documents_multi.yml 2>&1
  document: first
  type: mapping
  data:
    key1: value1
    key2: value2
  ---
  document: second
  type: mapping
  data:
    key3: value3
    key4: value4

  $ yamlcat documents_multi_three.yml 2>&1
  name: John Doe
  age: 30
  city: New York
  ---
  - apple
  - banana
  - orange
  - grape
  ---
  This is a plain scalar document

  $ yamlcat documents_multi_with_end.yml 2>&1
  first:
    document: data1
    value: 100
  ---
  second:
    document: data2
    value: 200
  ---
  third:
    document: data3
    value: 300

  $ yamlcat documents_multi_empty.yml 2>&1
  null
  ---
  key: value
  ---
  null
  ---
  - item1
  - item2

Test: Anchors file (uses multiple documents)

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
