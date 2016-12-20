<TeXmacs|1.99.9>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|semantic-lawsuit|0.1>

      <\src-purpose>
        The semantic-lawsuit package is for adding semantic markup to legal
        documents so that things such as, <with|font-shape|italic|e.g.,> the
        set of (potentially prioritized) factual claims can be easily gleaned
        from the document.
      </src-purpose>

      <\src-copyright|2015, 2016>
        Karl M.<space|1spc>Hegbloom
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <\active*>
    <\src-comment>
      Any ideas?

      Automatic spelling correction as-you-type;

      Automatic abbrev expansion, with template variable dialogs.

      Boilerplate contract construction?

      Integration with DMS.

      For now these are just one argument `wrappers'. Later they may be in
      need of a way to make an optional toc label, or brief-form label for a
      table of factual-claims, or table of ...? whatever turns out to be what
      is needed for this.

      For needs-citation and probably others, there ought to be a satellite
      buffer or menu, like Emacs' Speedbar or Imenu, to help find the "loose
      ends".

      What about a proof-assistant style\ 
    </src-comment>
  </active*>

  <assign|factual-claim|<macro|body|<arg|body>>>

  <assign|needs-citation|<macro|<flag|Needs Citation!|orange>>>

  <assign|needs-supporting-evidence|<macro|<flag|Needs Supporting
  Evidence!|orange>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
  </collection>
</initial>