Test yamlcat with simple YAML

  $ yamlcat simple_hello.yml
  hello: world

  $ yamlcat person.yml
  name: Alice
  age: 30

Test nested mappings

  $ yamlcat nested_mappings.yml
  server:
    host: localhost
    port: 8080
  database:
    name: mydb

Test sequences

  $ yamlcat fruits_list.yml
  - apple
  - banana
  - cherry

Test mapping with sequence value

  $ yamlcat fruits_mapping.yml
  fruits:
    - apple
    - banana

Test flow style output

  $ yamlcat --flow hobbies.yml
  {name: Alice, hobbies: [reading, coding]}

Test JSON output

  $ yamlcat --json person.yml
  {"name": "Alice", "age": 30}

Test seq.yml file (multiline plain scalar)

  $ yamlcat seq.yml
  - hello - whats - up
  - foo
  - bar

Test seq.yml roundtrip preserves data

  $ yamlcat --json seq.yml
  ["hello - whats - up", "foo", "bar"]

Test cohttp.yml

  $ yamlcat cohttp.yml
  language: c
  sudo: false
  services:
    - docker
  install: 'wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-docker.sh'
  script: bash -ex ./.travis-docker.sh
  env:
    global:
      - "EXTRA_REMOTES=\"https://github.com/mirage/mirage-dev.git\""
      - "PINS=\"cohttp-top:. cohttp-async:. cohttp-lwt-unix:. cohttp-lwt-jsoo:. cohttp-lwt:. cohttp-mirage:. cohttp:.\""
    matrix:
      - "PACKAGE=\"cohttp\" DISTRO=\"alpine-3.5\" OCAML_VERSION=\"4.06.0\""
      - "PACKAGE=\"cohttp-async\" DISTRO=\"alpine\" OCAML_VERSION=\"4.06.0\""
      - "PACKAGE=\"cohttp-lwt\" DISTRO=\"debian-unstable\" OCAML_VERSION=\"4.03.0\""
      - "PACKAGE=\"cohttp-mirage\" DISTRO=\"debian-unstable\" OCAML_VERSION=\"4.03.0\""
  notifications:
    webhooks:
      urls:
        - 'https://webhooks.gitter.im/e/6ee5059c7420709f4ad1'
      on_success: change
      on_failure: always
      on_start: false

Test cohttp.yml roundtrip with JSON

  $ yamlcat --json cohttp.yml
  {"language": "c", "sudo": false, "services": ["docker"], "install": "wget https://raw.githubusercontent.com/ocaml/ocaml-travisci-skeleton/master/.travis-docker.sh", "script": "bash -ex ./.travis-docker.sh", "env": {"global": ["EXTRA_REMOTES=\"https://github.com/mirage/mirage-dev.git\"", "PINS=\"cohttp-top:. cohttp-async:. cohttp-lwt-unix:. cohttp-lwt-jsoo:. cohttp-lwt:. cohttp-mirage:. cohttp:.\""], "matrix": ["PACKAGE=\"cohttp\" DISTRO=\"alpine-3.5\" OCAML_VERSION=\"4.06.0\"", "PACKAGE=\"cohttp-async\" DISTRO=\"alpine\" OCAML_VERSION=\"4.06.0\"", "PACKAGE=\"cohttp-lwt\" DISTRO=\"debian-unstable\" OCAML_VERSION=\"4.03.0\"", "PACKAGE=\"cohttp-mirage\" DISTRO=\"debian-unstable\" OCAML_VERSION=\"4.03.0\""]}, "notifications": {"webhooks": {"urls": ["https://webhooks.gitter.im/e/6ee5059c7420709f4ad1"], "on_success": "change", "on_failure": "always", "on_start": false}}}

Test special values

  $ yamlcat --json special_values.yml
  {"null_val": null, "bool_true": true, "bool_false": false, "number": 42, "float": 3.14}

Test quoted strings

  $ yamlcat quoted_strings.yml
  single: hello world
  double: hello world

Test literal block scalar

  $ yamlcat --json literal_block.yml
  {"text": "line one\nline two\n"}

Test folded block scalar

  $ yamlcat --json folded_block.yml
  {"text": "line one line two\n"}

Test linuxkit.yml (sequences of mappings)

  $ yamlcat linuxkit.yml | head -30
  kernel:
    image: 'linuxkit/kernel:4.9.40'
    cmdline: console=tty0 console=ttyS0
  init:
    - 'linuxkit/init:906e174b3f2e07f97d6fd693a2e8518e98dafa58'
    - 'linuxkit/runc:90e45f13e1d0a0983f36ef854621e3eac91cf541'
    - 'linuxkit/containerd:7c986fb7df33bea73b5c8097b46989e46f49d875'
    - 'linuxkit/ca-certificates:e44b0a66df5a102c0e220f0066b0d904710dcb10'
  onboot:
    - name: sysctl
      image: 'linuxkit/sysctl:184c914d23a017062d7b53d7fc1dfaf47764bef6'
    - name: dhcpcd
      image: 'linuxkit/dhcpcd:f3f5413abb78fae9020e35bd4788fa93df4530b7'
      command:
        - /sbin/dhcpcd
        - '--nobackground'
        - '-f'
        - /dhcpcd.conf
        - '-1'
  onshutdown:
    - name: shutdown
      image: 'busybox:latest'
      command:
        - /bin/echo
        - so long and thanks for all the fish
  services:
    - name: getty
      image: 'linuxkit/getty:2c841cdc34396e3fa8f25b62d112808f63f16df6'
      env:
        - INSECURE=true
