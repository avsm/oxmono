Model output is plain in a pipe and can be colored explicitly. These commands
inspect metadata. They do not load a model or fetch weights.

  $ humpty-cpu models list --color=never | awk 'NR == 1 { print $1, $2, $3 }'
  STATUS MODEL DESCRIPTION

  $ humpty-cpu models list --color=always | awk 'index($0, sprintf("%c", 27)) { found = 1 } END { exit !found }'

  $ humpty-cpu models list | awk 'index($0, sprintf("%c", 27)) { found = 1 } END { exit found }'

  $ NO_COLOR=1 humpty-cpu models list | awk 'index($0, sprintf("%c", 27)) { found = 1 } END { exit found }'

  $ humpty-cpu models show ds4/auto --color=never | sed -n '1p'
  ds4/auto
