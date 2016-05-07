<TeXmacs|1.99.4>

<style|generic>

<\body>
  <\hide-preamble>
    <assign|mymacrowithfootnote|<macro|body|See footnote<\footnote>
      <arg|body>
    </footnote>.>>
  </hide-preamble>

  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (current-buffer-url)
    <|unfolded-io>
      \<less\>url /home/karlheg/.TeXmacs/texts/scratch/no_name_25.tm\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (current-buffer)
    <|unfolded-io>
      \<less\>url /home/karlheg/.TeXmacs/texts/scratch/no_name_25.tm\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (path-to-buffer (cursor-path))
    <|unfolded-io>
      \<less\>url /home/karlheg/.TeXmacs/texts/scratch/no_name_25.tm\<gtr\>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (cursor-path)
    <|unfolded-io>
      (0 0 2 4 1 0 0)TeXmacs/progs/generic/
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (buffer-path)
    <|unfolded-io>
      (0)
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (create-unique-id)
    <|unfolded-io>
      "+Y8LXt9cdrlARR6"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (tm-define (renumber-footnotes)

      \ \ 
    <|unfolded-io>
      <errput|#\<unknown port\>:1:32: end of file>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <set-binding|documentID|+Y8LXt9cdrlARR6>

  <get-binding|documentID>

  <mymacrowithfootnote|This is another one.>

  <get-binding|bindwfn1>

  <set-binding|mybindtree2|Set this above the other.<\footnote>
    Ahah.
  </footnote>>

  <set-binding|mybindtree1|<math|<frac|1|2>> of a <\footnote>
    blah
  </footnote>>

  <get-binding|quotemacfn1>

  <get-binding|macroinwithfn1>

  <get-binding|mybindtree1>

  <get-binding|mybindtree2>

  <mymacrowithfootnote|And another.>\ 

  <set-binding|bindwfn1|<mymacrowithfootnote|This is bindwfn1>>

  <set-binding|quotemacfn1|<inactive|<quote|<mymacrowithfootnote|This is in
  quotemacfn1>>>>

  <set-binding|macroinwithfn1|<inactive|<macro|<mymacrowithfootnote|Inside
  macroinwithfn1.>>>>
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
    <associate|preamble|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|bindwfn1|<tuple|See footnote<assign|footnote-nr|7><hidden|<tuple>><\float|footnote|>
      <with|font-size|<quote|0.771>|<with|par-mode|<quote|justify>|par-left|<quote|0cm>|par-right|<quote|0cm>|font-shape|<quote|right>|dummy|<quote|1.0fn>|dummy|<quote|7.5fn>|<\surround|<locus|<id|%2957738-3AB0C60>|<link|hyperlink|<id|%2957738-3AB0C60>|<url|#footnr-7>>|7>.
      |<hidden|<tuple|footnote-7>><htab|0fn|first>>
        This is bindwfn1
      </surround>>>
    </float><space|0spc><label|footnr-7><rsup|<with|font-shape|<quote|right>|<reference|footnote-7>>>.|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|documentID|<tuple|+Y8LXt9cdrlARR6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-1|<tuple|1|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-2|<tuple|2|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-3|<tuple|3|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-4|<tuple|4|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-5|<tuple|5|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-6|<tuple|6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-7|<tuple|7|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-8|<tuple|8|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnote-9|<tuple|9|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-1|<tuple|1|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-2|<tuple|3|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-3|<tuple|3|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-4|<tuple|1|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-5|<tuple|6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-6|<tuple|6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-7|<tuple|7|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-8|<tuple|6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|footnr-9|<tuple|6|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|macroinwithfn1|<tuple|<mark|<arg|body>|<inline-tag|macro|<with|color|<quote|black>|See
    footnote<assign|footnote-nr|9><hidden|<tuple>><\float|footnote|>
      <with|font-size|<quote|0.771>|<with|par-mode|<quote|justify>|par-left|<quote|0cm>|par-right|<quote|0cm>|font-shape|<quote|right>|dummy|<quote|1.0fn>|dummy|<quote|7.5fn>|<\surround|<locus|<id|%2957738-3B5B700>|<link|hyperlink|<id|%2957738-3B5B700>|<url|#footnr-9>>|9>.
      |<hidden|<tuple|footnote-9>><htab|0fn|first>>
        Inside macroinwithfn1.
      </surround>>>
    </float><space|0spc><label|footnr-9><rsup|<with|font-shape|<quote|right>|<reference|footnote-9>>>.>>>|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|mybindtree1|<tuple|<with|mode|<quote|math>|<frac|1|2>> of a
    <assign|footnote-nr|6><hidden|<tuple>><\float|footnote|>
      <with|font-size|<quote|0.771>|<with|par-mode|<quote|justify>|par-left|<quote|0cm>|par-right|<quote|0cm>|font-shape|<quote|right>|dummy|<quote|1.0fn>|dummy|<quote|7.5fn>|<\surround|<locus|<id|%2957738-3A1B898>|<link|hyperlink|<id|%2957738-3A1B898>|<url|#footnr-6>>|6>.
      |<hidden|<tuple|footnote-6>><htab|0fn|first>>
        blah
      </surround>>>
    </float><space|0spc><label|footnr-6><rsup|<with|font-shape|<quote|right>|<reference|footnote-6>>>|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|mybindtree2|<tuple|Set this above the
    other.<assign|footnote-nr|5><hidden|<tuple>><\float|footnote|>
      <with|font-size|<quote|0.771>|<with|par-mode|<quote|justify>|par-left|<quote|0cm>|par-right|<quote|0cm>|font-shape|<quote|right>|dummy|<quote|1.0fn>|dummy|<quote|7.5fn>|<\surround|<locus|<id|%2957738-3A953D0>|<link|hyperlink|<id|%2957738-3A953D0>|<url|#footnr-5>>|5>.
      |<hidden|<tuple|footnote-5>><htab|0fn|first>>
        Ahah.
      </surround>>>
    </float><space|0spc><label|footnr-5><rsup|<with|font-shape|<quote|right>|<reference|footnote-5>>>|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
    <associate|quotemacfn1|<tuple|<mark|<arg|body>|<inline-tag|quote|<with|color|<quote|black>|See
    footnote<assign|footnote-nr|8><hidden|<tuple>><\float|footnote|>
      <with|font-size|<quote|0.771>|<with|par-mode|<quote|justify>|par-left|<quote|0cm>|par-right|<quote|0cm>|font-shape|<quote|right>|dummy|<quote|1.0fn>|dummy|<quote|7.5fn>|<\surround|<locus|<id|%2957738-3B50A20>|<link|hyperlink|<id|%2957738-3B50A20>|<url|#footnr-8>>|8>.
      |<hidden|<tuple|footnote-8>><htab|0fn|first>>
        This is in quotemacfn1
      </surround>>>
    </float><space|0spc><label|footnr-8><rsup|<with|font-shape|<quote|right>|<reference|footnote-8>>>.>>>|?|../../../../.TeXmacs/texts/scratch/no_name_25.tm>>
  </collection>
</references>