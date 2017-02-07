<TeXmacs|1.99.9>

<style|<tuple|generic|test-clipboard-cut-paste>>

<\body>
  <\session|scheme|default>
    <\input|Scheme] >
      (load "test-clipboard-cut-paste.scm")
    </input>

    <\unfolded-io|Scheme] >
      has-testtags?
    <|unfolded-io>
      #\<less\>procedure has-testtags? (t)\<gtr\>
    </unfolded-io>

    <\input|Scheme] >
      (use-modules (test-clipboard-cut-paste))
    </input>

    <\unfolded-io|Scheme] >
      (in-test-clipboard-cut-paste-style?)
    <|unfolded-io>
      #f
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (style-has? "test-clipboard-cut-paste-dtd")
    <|unfolded-io>
      #f
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <testtag|+HMWPT2B3fKGXnK|<tuple|3|<#>|false>|Text>

  <testtag|+HMWPT2B3fKGXnK|<tuple|3|<#>|false>|Text>

  <\testtag|+HMWPT2B3fKGXnK|<tuple|3|<#>|false>>
    Text
  </testtag>

  <testtag|+HMWPT2B3fKGXnK|<tuple|3|<#>|false>|Text>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
  </collection>
</initial>