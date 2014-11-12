pdf-mode.el - Edit raw PDF files in Emacs
=========================================

`pdf-mode` is a major mode for editing PDF files in Emacs.  It's not perfect, but it should be a good starting point.
It might be useful for the poor souls who are working on generating PDF; definitely useful to me.

Features:

- parser for PDF entities

- syntax highlighting based on the AST produced by the parser

- find references to an object (`M-x pdf-highlight-refs`)

- find definition of an object (`M-x pdf-find-definition`)

- rewrites the `xref` section and stream `/Length`-s when saving a file (`M-x pdf-fix-xrefs`)

- easily insert a new object/stream (`M-x pdf-new-object`)

- discard objects that are not referenced (`M-x pdf-cleanup`)

### Default key bindings

- `C-c o` insert a new object (at point; make sure the cursor is somewhere where an object makes sense)
- `M-?` highlight references to object/reference at point
- `M-.` locate definition of object reference at point
- `M-,` go back to previous location (after `M-.`)

`pdf-fix-xrefs` will run automatically before saving a file, so if that succeeds the new file should be valid (i.e. the
`xref` and `startxref` sections should be properly updated).  If there's a parse error, however, the file won't be saved
at all.  This is probably a bad idea; comments welcome.

### Limitations

Only PDF-1.4 non-linearized format is supported.  PDF-1.5 introduced “object streams” (see PDF spec, section 3.4.6),
along with a new `XRef` stream to support the new objects.  So far I haven't need that, so it's not supported.  For
dealing with linearized PDFs or object streams, you can use [qpdf](http://qpdf.sourceforge.net/):

    qpdf --object-streams=disable -qdf input.pdf output.pdf

Now `output.pdf` should be a file that our little mode can work with.

Syntax highlighting is based on parsing the whole buffer, so if you throw a megabyte-order file at it it might feel
pretty slow.  I couldn't figure out how to make it work with Emacs' built-in font-locking support, because streams
contain arbitrary data that can break the fragile regexp-based syntax highlighting (my bad for not striving hard enough
to understand how do multiline font locking with Emacs).

The parser might not work properly with DOS-style newlines.
