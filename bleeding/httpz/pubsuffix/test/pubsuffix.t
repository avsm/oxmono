Public Suffix List Tests
========================

These tests are based on the official test vectors from:
https://raw.githubusercontent.com/publicsuffix/list/master/tests/test_psl.txt

The checkPublicSuffix function tests the registrable domain output.
null input -> null output means an error is expected.
domain -> null means the domain is a public suffix (no registrable domain).

Basic Statistics
----------------

  $ httpz-pubsuffix stats
  Total rules: 10249
  ICANN rules: 6949
  Private rules: 3300

Null Input (Empty Domain)
-------------------------

  $ httpz-pubsuffix registrable "" >pubsuffix.stdout 2>pubsuffix.stderr
  [2]
  $ cat pubsuffix.stderr
  ERROR: Empty domain
  $ test ! -s pubsuffix.stdout

Mixed Case Tests
----------------

  $ httpz-pubsuffix registrable "COM"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "example.COM"
  example.com

  $ httpz-pubsuffix registrable "WwW.example.COM"
  example.com

Leading Dot Tests
-----------------

  $ httpz-pubsuffix registrable ".com"
  ERROR: Domain has a leading dot
  [2]

  $ httpz-pubsuffix registrable ".example"
  ERROR: Domain has a leading dot
  [2]

  $ httpz-pubsuffix registrable ".example.com"
  ERROR: Domain has a leading dot
  [2]

  $ httpz-pubsuffix registrable ".example.example"
  ERROR: Domain has a leading dot
  [2]

Unlisted TLD (Implicit * Rule)
------------------------------

Per the algorithm, if no rules match, the implicit * rule applies.
For an unlisted TLD like "example", the TLD itself is the suffix.

  $ httpz-pubsuffix registrable "example"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "example.example"
  example.example

  $ httpz-pubsuffix registrable "b.example.example"
  example.example

  $ httpz-pubsuffix registrable "a.b.example.example"
  example.example

TLD Listed With No Subdomains (.biz)
------------------------------------

  $ httpz-pubsuffix registrable "biz"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "domain.biz"
  domain.biz

  $ httpz-pubsuffix registrable "b.domain.biz"
  domain.biz

  $ httpz-pubsuffix registrable "a.b.domain.biz"
  domain.biz

TLD Listed With Subdomains (.com)
---------------------------------

  $ httpz-pubsuffix registrable "com"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "example.com"
  example.com

  $ httpz-pubsuffix registrable "b.example.com"
  example.com

  $ httpz-pubsuffix registrable "a.b.example.com"
  example.com

Second-Level Domain (.uk.com)
-----------------------------

  $ httpz-pubsuffix registrable "uk.com"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "example.uk.com"
  example.uk.com

  $ httpz-pubsuffix registrable "b.example.uk.com"
  example.uk.com

  $ httpz-pubsuffix registrable "a.b.example.uk.com"
  example.uk.com

TLD with Single Character (.ac)
-------------------------------

  $ httpz-pubsuffix registrable "test.ac"
  test.ac

Wildcard TLD (.mm has *.mm rule, so c.mm is a suffix)
-----------------------------------------------------

  $ httpz-pubsuffix registrable "mm"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "c.mm"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "b.c.mm"
  b.c.mm

  $ httpz-pubsuffix registrable "a.b.c.mm"
  b.c.mm

Japan Tests (.jp)
-----------------

More complex TLD with multiple levels:

  $ httpz-pubsuffix registrable "jp"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.jp"
  test.jp

  $ httpz-pubsuffix registrable "www.test.jp"
  test.jp

Second-level suffix under .jp:

  $ httpz-pubsuffix registrable "ac.jp"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.ac.jp"
  test.ac.jp

  $ httpz-pubsuffix registrable "www.test.ac.jp"
  test.ac.jp

Kyoto has a rule, so kyoto.jp is a suffix:

  $ httpz-pubsuffix registrable "kyoto.jp"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.kyoto.jp"
  test.kyoto.jp

ide.kyoto.jp has *.ide.kyoto.jp rule (wildcard):

  $ httpz-pubsuffix registrable "ide.kyoto.jp"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "b.ide.kyoto.jp"
  b.ide.kyoto.jp

  $ httpz-pubsuffix registrable "a.b.ide.kyoto.jp"
  b.ide.kyoto.jp

Kobe has *.kobe.jp wildcard but !city.kobe.jp exception:

  $ httpz-pubsuffix registrable "c.kobe.jp"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "b.c.kobe.jp"
  b.c.kobe.jp

  $ httpz-pubsuffix registrable "a.b.c.kobe.jp"
  b.c.kobe.jp

Exception rule: city.kobe.jp is registrable despite *.kobe.jp:

  $ httpz-pubsuffix registrable "city.kobe.jp"
  city.kobe.jp

  $ httpz-pubsuffix registrable "www.city.kobe.jp"
  city.kobe.jp

Cook Islands Tests (.ck with !www.ck exception)
-----------------------------------------------

.ck has *.ck wildcard rule and !www.ck exception:

  $ httpz-pubsuffix registrable "ck"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.ck"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "b.test.ck"
  b.test.ck

  $ httpz-pubsuffix registrable "a.b.test.ck"
  b.test.ck

Exception: www.ck is registrable:

  $ httpz-pubsuffix registrable "www.ck"
  www.ck

  $ httpz-pubsuffix registrable "www.www.ck"
  www.ck

United States Tests (.us)
-------------------------

  $ httpz-pubsuffix registrable "us"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.us"
  test.us

  $ httpz-pubsuffix registrable "www.test.us"
  test.us

State subdivision (.ak.us):

  $ httpz-pubsuffix registrable "ak.us"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.ak.us"
  test.ak.us

  $ httpz-pubsuffix registrable "www.test.ak.us"
  test.ak.us

Deep subdivision (.k12.ak.us):

  $ httpz-pubsuffix registrable "k12.ak.us"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "test.k12.ak.us"
  test.k12.ak.us

  $ httpz-pubsuffix registrable "www.test.k12.ak.us"
  test.k12.ak.us

Internationalized Domain Names (IDN) - Chinese
----------------------------------------------

These tests use Chinese characters.
食狮 = "food lion" in Chinese
公司 = "company" in Chinese

  $ httpz-pubsuffix registrable "食狮.com.cn"
  xn--85x722f.com.cn

  $ httpz-pubsuffix registrable "食狮.公司.cn"
  xn--85x722f.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "www.食狮.公司.cn"
  xn--85x722f.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "shishi.公司.cn"
  shishi.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "公司.cn"
  ERROR: Domain is itself a public suffix
  [2]

IDN TLD (中国 = China):

  $ httpz-pubsuffix registrable "食狮.中国"
  xn--85x722f.xn--fiqs8s

  $ httpz-pubsuffix registrable "www.食狮.中国"
  xn--85x722f.xn--fiqs8s

  $ httpz-pubsuffix registrable "shishi.中国"
  shishi.xn--fiqs8s

  $ httpz-pubsuffix registrable "中国"
  ERROR: Domain is itself a public suffix
  [2]

Punycode Input (Same as Above in ASCII)
---------------------------------------

  $ httpz-pubsuffix registrable "xn--85x722f.com.cn"
  xn--85x722f.com.cn

  $ httpz-pubsuffix registrable "xn--85x722f.xn--55qx5d.cn"
  xn--85x722f.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "www.xn--85x722f.xn--55qx5d.cn"
  xn--85x722f.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "shishi.xn--55qx5d.cn"
  shishi.xn--55qx5d.cn

  $ httpz-pubsuffix registrable "xn--55qx5d.cn"
  ERROR: Domain is itself a public suffix
  [2]

  $ httpz-pubsuffix registrable "xn--85x722f.xn--fiqs8s"
  xn--85x722f.xn--fiqs8s

  $ httpz-pubsuffix registrable "www.xn--85x722f.xn--fiqs8s"
  xn--85x722f.xn--fiqs8s

  $ httpz-pubsuffix registrable "shishi.xn--fiqs8s"
  shishi.xn--fiqs8s

  $ httpz-pubsuffix registrable "xn--fiqs8s"
  ERROR: Domain is itself a public suffix
  [2]

Public Suffix Tests
-------------------

Test the public_suffix function directly:

  $ httpz-pubsuffix suffix "www.example.com"
  com

  $ httpz-pubsuffix suffix "www.example.co.uk"
  co.uk

  $ httpz-pubsuffix suffix "example.com"
  com

  $ httpz-pubsuffix suffix "com"
  com

  $ httpz-pubsuffix suffix "b.ide.kyoto.jp"
  ide.kyoto.jp

  $ httpz-pubsuffix suffix "city.kobe.jp"
  kobe.jp

  $ httpz-pubsuffix suffix "www.ck"
  ck

is_public_suffix Tests
----------------------

  $ httpz-pubsuffix is_suffix "com"
  true

  $ httpz-pubsuffix is_suffix "example.com"
  false

  $ httpz-pubsuffix is_suffix "co.uk"
  true

  $ httpz-pubsuffix is_suffix "example.co.uk"
  false

  $ httpz-pubsuffix is_suffix "test.ck"
  true

  $ httpz-pubsuffix is_suffix "www.ck"
  false

  $ httpz-pubsuffix is_suffix "city.kobe.jp"
  false

  $ httpz-pubsuffix is_suffix "ide.kyoto.jp"
  true

is_registrable_domain Tests
---------------------------

  $ httpz-pubsuffix is_registrable "example.com"
  true

  $ httpz-pubsuffix is_registrable "www.example.com"
  false

  $ httpz-pubsuffix is_registrable "com"
  false

  $ httpz-pubsuffix is_registrable "city.kobe.jp"
  true

  $ httpz-pubsuffix is_registrable "www.city.kobe.jp"
  false

Section Information Tests
-------------------------

Test that ICANN vs Private section is correctly reported:

  $ httpz-pubsuffix suffix_section "example.com"
  com (ICANN)

  $ httpz-pubsuffix suffix_section "example.co.uk"
  co.uk (ICANN)

Blogspot.com is in the PRIVATE section:

  $ httpz-pubsuffix suffix_section "example.blogspot.com"
  blogspot.com (PRIVATE)

  $ httpz-pubsuffix registrable_section "www.example.blogspot.com"
  example.blogspot.com (PRIVATE)

GitHub.io is in the PRIVATE section:

  $ httpz-pubsuffix suffix_section "example.github.io"
  github.io (PRIVATE)

  $ httpz-pubsuffix registrable_section "myproject.github.io"
  myproject.github.io (PRIVATE)

Trailing Dot Tests (FQDN)
-------------------------

Per the wiki, trailing dots should be preserved:

  $ httpz-pubsuffix suffix "example.com."
  com.

  $ httpz-pubsuffix suffix "example.com"
  com

  $ httpz-pubsuffix registrable "www.example.com."
  example.com.

  $ httpz-pubsuffix registrable "www.example.com"
  example.com

Edge Cases from Wiki Examples
-----------------------------

From the Format.md examples:

Rule 1 (com): Cookies MAY be set for foo.com

  $ httpz-pubsuffix registrable "foo.com"
  foo.com

Rule 2 (*.foo.com): This isn't in the real PSL, but we test similar patterns
with *.ck:

  $ httpz-pubsuffix is_suffix "bar.ck"
  true

Rule 3 (*.jp): bar.jp is a suffix

  $ httpz-pubsuffix is_suffix "bar.jp"
  false

Rule 4: Note that *.hokkaido.jp is not in the actual PSL - only specific
city subdomains are listed. So bar.hokkaido.jp follows hokkaido.jp rule.

  $ httpz-pubsuffix is_suffix "bar.hokkaido.jp"
  false

  $ httpz-pubsuffix registrable "foo.bar.hokkaido.jp"
  bar.hokkaido.jp

  $ httpz-pubsuffix is_suffix "abashiri.hokkaido.jp"
  true

  $ httpz-pubsuffix registrable "foo.abashiri.hokkaido.jp"
  foo.abashiri.hokkaido.jp

Rule 6 (!pref.hokkaido.jp): pref.hokkaido.jp is registrable (exception)

  $ httpz-pubsuffix registrable "pref.hokkaido.jp"
  pref.hokkaido.jp

  $ httpz-pubsuffix is_suffix "pref.hokkaido.jp"
  false
