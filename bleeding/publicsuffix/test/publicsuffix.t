Public Suffix List Tests
========================

These tests are based on the official test vectors from:
https://raw.githubusercontent.com/publicsuffix/list/master/tests/test_psl.txt

The checkPublicSuffix function tests the registrable domain output.
null input -> null output means an error is expected.
domain -> null means the domain is a public suffix (no registrable domain).

Basic Statistics
----------------

  $ publicsuffix stats
  Total rules: 10064
  ICANN rules: 6930
  Private rules: 3134

Null Input (Empty Domain)
-------------------------

  $ publicsuffix registrable ""
  ERROR: Empty domain

Mixed Case Tests
----------------

  $ publicsuffix registrable "COM"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "example.COM"
  example.com

  $ publicsuffix registrable "WwW.example.COM"
  example.com

Leading Dot Tests
-----------------

  $ publicsuffix registrable ".com"
  ERROR: Domain has a leading dot

  $ publicsuffix registrable ".example"
  ERROR: Domain has a leading dot

  $ publicsuffix registrable ".example.com"
  ERROR: Domain has a leading dot

  $ publicsuffix registrable ".example.example"
  ERROR: Domain has a leading dot

Unlisted TLD (Implicit * Rule)
------------------------------

Per the algorithm, if no rules match, the implicit * rule applies.
For an unlisted TLD like "example", the TLD itself is the suffix.

  $ publicsuffix registrable "example"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "example.example"
  example.example

  $ publicsuffix registrable "b.example.example"
  example.example

  $ publicsuffix registrable "a.b.example.example"
  example.example

TLD Listed With No Subdomains (.biz)
------------------------------------

  $ publicsuffix registrable "biz"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "domain.biz"
  domain.biz

  $ publicsuffix registrable "b.domain.biz"
  domain.biz

  $ publicsuffix registrable "a.b.domain.biz"
  domain.biz

TLD Listed With Subdomains (.com)
---------------------------------

  $ publicsuffix registrable "com"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "example.com"
  example.com

  $ publicsuffix registrable "b.example.com"
  example.com

  $ publicsuffix registrable "a.b.example.com"
  example.com

Second-Level Domain (.uk.com)
-----------------------------

  $ publicsuffix registrable "uk.com"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "example.uk.com"
  example.uk.com

  $ publicsuffix registrable "b.example.uk.com"
  example.uk.com

  $ publicsuffix registrable "a.b.example.uk.com"
  example.uk.com

TLD with Single Character (.ac)
-------------------------------

  $ publicsuffix registrable "test.ac"
  test.ac

Wildcard TLD (.mm has *.mm rule, so c.mm is a suffix)
-----------------------------------------------------

  $ publicsuffix registrable "mm"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "c.mm"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "b.c.mm"
  b.c.mm

  $ publicsuffix registrable "a.b.c.mm"
  b.c.mm

Japan Tests (.jp)
-----------------

More complex TLD with multiple levels:

  $ publicsuffix registrable "jp"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.jp"
  test.jp

  $ publicsuffix registrable "www.test.jp"
  test.jp

Second-level suffix under .jp:

  $ publicsuffix registrable "ac.jp"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.ac.jp"
  test.ac.jp

  $ publicsuffix registrable "www.test.ac.jp"
  test.ac.jp

Kyoto has a rule, so kyoto.jp is a suffix:

  $ publicsuffix registrable "kyoto.jp"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.kyoto.jp"
  test.kyoto.jp

ide.kyoto.jp has *.ide.kyoto.jp rule (wildcard):

  $ publicsuffix registrable "ide.kyoto.jp"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "b.ide.kyoto.jp"
  b.ide.kyoto.jp

  $ publicsuffix registrable "a.b.ide.kyoto.jp"
  b.ide.kyoto.jp

Kobe has *.kobe.jp wildcard but !city.kobe.jp exception:

  $ publicsuffix registrable "c.kobe.jp"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "b.c.kobe.jp"
  b.c.kobe.jp

  $ publicsuffix registrable "a.b.c.kobe.jp"
  b.c.kobe.jp

Exception rule: city.kobe.jp is registrable despite *.kobe.jp:

  $ publicsuffix registrable "city.kobe.jp"
  city.kobe.jp

  $ publicsuffix registrable "www.city.kobe.jp"
  city.kobe.jp

Cook Islands Tests (.ck with !www.ck exception)
-----------------------------------------------

.ck has *.ck wildcard rule and !www.ck exception:

  $ publicsuffix registrable "ck"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.ck"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "b.test.ck"
  b.test.ck

  $ publicsuffix registrable "a.b.test.ck"
  b.test.ck

Exception: www.ck is registrable:

  $ publicsuffix registrable "www.ck"
  www.ck

  $ publicsuffix registrable "www.www.ck"
  www.ck

United States Tests (.us)
-------------------------

  $ publicsuffix registrable "us"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.us"
  test.us

  $ publicsuffix registrable "www.test.us"
  test.us

State subdivision (.ak.us):

  $ publicsuffix registrable "ak.us"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.ak.us"
  test.ak.us

  $ publicsuffix registrable "www.test.ak.us"
  test.ak.us

Deep subdivision (.k12.ak.us):

  $ publicsuffix registrable "k12.ak.us"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "test.k12.ak.us"
  test.k12.ak.us

  $ publicsuffix registrable "www.test.k12.ak.us"
  test.k12.ak.us

Internationalized Domain Names (IDN) - Chinese
----------------------------------------------

These tests use Chinese characters.
食狮 = "food lion" in Chinese
公司 = "company" in Chinese

  $ publicsuffix registrable "食狮.com.cn"
  xn--85x722f.com.cn

  $ publicsuffix registrable "食狮.公司.cn"
  xn--85x722f.xn--55qx5d.cn

  $ publicsuffix registrable "www.食狮.公司.cn"
  xn--85x722f.xn--55qx5d.cn

  $ publicsuffix registrable "shishi.公司.cn"
  shishi.xn--55qx5d.cn

  $ publicsuffix registrable "公司.cn"
  ERROR: Domain is itself a public suffix

IDN TLD (中国 = China):

  $ publicsuffix registrable "食狮.中国"
  xn--85x722f.xn--fiqs8s

  $ publicsuffix registrable "www.食狮.中国"
  xn--85x722f.xn--fiqs8s

  $ publicsuffix registrable "shishi.中国"
  shishi.xn--fiqs8s

  $ publicsuffix registrable "中国"
  ERROR: Domain is itself a public suffix

Punycode Input (Same as Above in ASCII)
---------------------------------------

  $ publicsuffix registrable "xn--85x722f.com.cn"
  xn--85x722f.com.cn

  $ publicsuffix registrable "xn--85x722f.xn--55qx5d.cn"
  xn--85x722f.xn--55qx5d.cn

  $ publicsuffix registrable "www.xn--85x722f.xn--55qx5d.cn"
  xn--85x722f.xn--55qx5d.cn

  $ publicsuffix registrable "shishi.xn--55qx5d.cn"
  shishi.xn--55qx5d.cn

  $ publicsuffix registrable "xn--55qx5d.cn"
  ERROR: Domain is itself a public suffix

  $ publicsuffix registrable "xn--85x722f.xn--fiqs8s"
  xn--85x722f.xn--fiqs8s

  $ publicsuffix registrable "www.xn--85x722f.xn--fiqs8s"
  xn--85x722f.xn--fiqs8s

  $ publicsuffix registrable "shishi.xn--fiqs8s"
  shishi.xn--fiqs8s

  $ publicsuffix registrable "xn--fiqs8s"
  ERROR: Domain is itself a public suffix

Public Suffix Tests
-------------------

Test the public_suffix function directly:

  $ publicsuffix suffix "www.example.com"
  com

  $ publicsuffix suffix "www.example.co.uk"
  co.uk

  $ publicsuffix suffix "example.com"
  com

  $ publicsuffix suffix "com"
  com

  $ publicsuffix suffix "b.ide.kyoto.jp"
  ide.kyoto.jp

  $ publicsuffix suffix "city.kobe.jp"
  kobe.jp

  $ publicsuffix suffix "www.ck"
  ck

is_public_suffix Tests
----------------------

  $ publicsuffix is_suffix "com"
  true

  $ publicsuffix is_suffix "example.com"
  false

  $ publicsuffix is_suffix "co.uk"
  true

  $ publicsuffix is_suffix "example.co.uk"
  false

  $ publicsuffix is_suffix "test.ck"
  true

  $ publicsuffix is_suffix "www.ck"
  false

  $ publicsuffix is_suffix "city.kobe.jp"
  false

  $ publicsuffix is_suffix "ide.kyoto.jp"
  true

is_registrable_domain Tests
---------------------------

  $ publicsuffix is_registrable "example.com"
  true

  $ publicsuffix is_registrable "www.example.com"
  false

  $ publicsuffix is_registrable "com"
  false

  $ publicsuffix is_registrable "city.kobe.jp"
  true

  $ publicsuffix is_registrable "www.city.kobe.jp"
  false

Section Information Tests
-------------------------

Test that ICANN vs Private section is correctly reported:

  $ publicsuffix suffix_section "example.com"
  com (ICANN)

  $ publicsuffix suffix_section "example.co.uk"
  co.uk (ICANN)

Blogspot.com is in the PRIVATE section:

  $ publicsuffix suffix_section "example.blogspot.com"
  blogspot.com (PRIVATE)

  $ publicsuffix registrable_section "www.example.blogspot.com"
  example.blogspot.com (PRIVATE)

GitHub.io is in the PRIVATE section:

  $ publicsuffix suffix_section "example.github.io"
  github.io (PRIVATE)

  $ publicsuffix registrable_section "myproject.github.io"
  myproject.github.io (PRIVATE)

Trailing Dot Tests (FQDN)
-------------------------

Per the wiki, trailing dots should be preserved:

  $ publicsuffix suffix "example.com."
  com.

  $ publicsuffix suffix "example.com"
  com

  $ publicsuffix registrable "www.example.com."
  example.com.

  $ publicsuffix registrable "www.example.com"
  example.com

Edge Cases from Wiki Examples
-----------------------------

From the Format.md examples:

Rule 1 (com): Cookies MAY be set for foo.com

  $ publicsuffix registrable "foo.com"
  foo.com

Rule 2 (*.foo.com): This isn't in the real PSL, but we test similar patterns
with *.ck:

  $ publicsuffix is_suffix "bar.ck"
  true

Rule 3 (*.jp): bar.jp is a suffix

  $ publicsuffix is_suffix "bar.jp"
  false

Rule 4: Note that *.hokkaido.jp is not in the actual PSL - only specific
city subdomains are listed. So bar.hokkaido.jp follows hokkaido.jp rule.

  $ publicsuffix is_suffix "bar.hokkaido.jp"
  false

  $ publicsuffix registrable "foo.bar.hokkaido.jp"
  bar.hokkaido.jp

  $ publicsuffix is_suffix "abashiri.hokkaido.jp"
  true

  $ publicsuffix registrable "foo.abashiri.hokkaido.jp"
  foo.abashiri.hokkaido.jp

Rule 6 (!pref.hokkaido.jp): pref.hokkaido.jp is registrable (exception)

  $ publicsuffix registrable "pref.hokkaido.jp"
  pref.hokkaido.jp

  $ publicsuffix is_suffix "pref.hokkaido.jp"
  false
