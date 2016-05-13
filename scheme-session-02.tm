<TeXmacs|1.99.4>

<style|tmarticle>

<\body>
  <\session|scheme|default>
    <\input|Scheme] >
      (load-from-path "zotero.scm")
    </input>

    <\input|Scheme] >
      (zotero-addCitation)
    </input>

    <\unfolded-io|Scheme] >
      (tm-widget (zotero-icon-with-label-and-buttons cmd)

      \ \ \ \ (icon (%search-load-path "icon-stop.png")) (text "This is the
      text of it.")

      \ \ (bottom-buttons \<gtr\>\<gtr\> ("Ok" (cmd "Ok")) ("Cancel" (cmd
      "Cancel"))))
    <|unfolded-io>
      ((zotero) (zotero) (zotero) (zotero) (zotero) (zotero))
    </unfolded-io>

    <\input|Scheme] >
      (dialogue-window zotero-icon-with-label-and-buttons (lambda (arg)
      (display* arg "\\n")) "Window Title")
    </input>

    <\unfolded-io|Scheme] >
      (apropos "path")
    <|unfolded-io>
      <errput|Unbound variable: apropos>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (tm-widget ((zotero-display-alert str_Text int_Icon int_Buttons) cmd)

      \ \ (let ((i (list-ref (map %search-load-path '("icon-stop.png"
      "icon-notice.png" "icon-caution.png")) int_Icon)))

      \ \ (padded

      \ \ \ \ (hlist (icon i)

      \ \ \ \ (centered (text str_Text))))

      \ \ (bottom-buttons \<gtr\>\<gtr\>\<gtr\> (cond

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons 0) (("Ok" (cmd 1))))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons 1) (("Ok" (cmd 1))
      ("Cancel" (cmd 0))))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons 2) (("Yes" (cmd 1)) ("No"
      (cmd 0))))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons 3) (("Yes" (cmd 2)) ("No"
      (cmd 1)) ("Cancel" (cmd 0))))))))
    <|unfolded-io>
      ((zotero) (zotero) (zotero) (zotero) (zotero) (zotero) (zotero))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (dialogue-window (zotero-display-alert "Alert text here." 0 0) (lambda
      (arg) (display arg)) "Zotero Alert")
    <|unfolded-io>
      <errput|Missing expression in (lambda ()).>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (tm-widget ((zotero-display-alert str_Text int_Icon int_Buttons) cmd)\ 

      \ \ (let ((i (list-ref\ 

      \ \ \ \ \ \ \ \ \ \ \ \ (map %search-load-path\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("icon-stop.png" "icon-notice.png"
      "icon-caution.png"))

      \ \ \ \ \ \ \ \ \ \ \ \ int_Icon)))

      \ \ \ \ (resize "375px" "250px"\ 

      \ \ \ \ \ \ (hlist (icon i)

      \ \ \ \ \ \ \ \ \ \ \ \ \ (centered (text str_Text)))\ 

      \ \ \ \ \ \ (bottom-buttons \<gtr\>\<gtr\>\<gtr\> (cond\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons
      0) (("Ok" (cmd 1))))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons
      1) (("Ok" (cmd 1))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ("Cancel"
      (cmd 0))))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons
      2) (("Yes" (cmd 1))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ("No"
      (cmd 0))))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((= int_Buttons
      3) (("Yes" (cmd 2))

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ("No"
      (cmd 1))\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ("Cancel"
      (cmd 0)))))))))
    <|unfolded-io>
      ((zotero) (zotero) (zotero) (zotero) (zotero) (zotero) (zotero)
      (zotero) (zotero) (zotero) (zotero) (zotero))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (dialogue-window (zotero-display-alert "Alert text here." 0 0) identity
      "Zotero Alert")
    <|unfolded-io>
      <errput|Missing expression in (lambda ()).>
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
  </collection>
</initial>