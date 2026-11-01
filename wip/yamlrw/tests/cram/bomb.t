Billion laughs attack protection tests

Test with a tight node limit (small bomb would expand to ~6561 nodes):

  $ yamlcat --max-nodes 100 --json bomb_small.yml
  Error: alias expansion exceeded node limit (100 nodes)
  [1]

Test with a limit that allows the small bomb:

  $ yamlcat --max-nodes 10000 --json bomb_small.yml | head -c 100
  {"a": [1, 2, 3, 4, 5, 6, 7, 8, 9], "b": [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 2, 3, 4, 5, 6, 7, 8, 9], [

Test depth limit with a nested alias chain:

  $ yamlcat --max-depth 2 --json depth_bomb.yml | head -c 50
  Error: alias expansion exceeded depth limit (2 levels)

  $ yamlcat --max-depth 10 --json depth_bomb.yml | head -c 50
  {"a": ["x", "y", "z"], "b": [["x", "y", "z"], ["x"

Test that --no-resolve-aliases keeps aliases as-is (in debug mode):

  $ yamlcat --no-resolve-aliases --debug simple_alias.yml
  Document 1:
  document(
    implicit_start=true,
    implicit_end=true,
    root=mapping(
           style=block,
           members={
           scalar("anchor", style=plain):
             scalar("hello", anchor=anc, style=plain),
           scalar("alias", style=plain): *anc
    })
  )

With resolve (default), aliases are expanded:

  $ yamlcat --json simple_alias.yml
  {"anchor": "hello", "alias": "hello"}

Test the full bomb is rejected with default limits:

  $ yamlcat --json bomb.yml 2>&1 | head -1
  Error: alias expansion exceeded node limit (10000000 nodes)

With a very small limit:

  $ yamlcat --max-nodes 50 --json bomb.yml
  Error: alias expansion exceeded node limit (50 nodes)
  [1]

Test that valid YAML with aliases works:

  $ yamlcat --json valid_alias.yml
  {"defaults": {"timeout": 30, "retries": 3}, "production": {"<<": {"timeout": 30, "retries": 3}, "port": 8080}}

Test help includes the new options:

  $ yamlcat --help=plain | grep 'max-nodes'
         --max-nodes=N (absent=10000000)
           yamlcat --max-nodes 1000 --max-depth 10 untrusted.yaml

  $ yamlcat --help=plain | grep 'max-depth'
         --max-depth=N (absent=100)
           yamlcat --max-nodes 1000 --max-depth 10 untrusted.yaml

  $ yamlcat --help=plain | grep 'no-resolve-aliases'
         --no-resolve-aliases
