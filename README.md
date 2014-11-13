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

- decompress stream at point (`M-x pdf-inflate-stream`)

- easily insert a new object/stream (`M-x pdf-new-object`)

- discard objects that are not referenced (`M-x pdf-cleanup`)

### Default key bindings

(customize `pdf-mode-map`)

- `C-c o` — insert a new object (at point; make sure the cursor is somewhere where an object makes sense).  Pass a
  prefix argument (`C-u`) to make the new object a stream.

- `M-?` — highlight references to object/reference at point.

- `M-.` — locate definition of object reference at point.

- `M-,` — go back to previous location (after `M-.`).

`pdf-fix-xrefs` will run automatically before saving a file, so if that succeeds the new file should be valid (i.e. the
`xref` and `startxref` sections should be properly updated).  If there's a parse error, however, the file won't be saved
at all.  This is probably a bad idea; comments welcome.

`pdf-fix-xrefs` expects `startxref` to really be at the end of the file, and the `trailer` dictionary to precede it, as
per the spec.  The `xref` table can be anywhere in the page, but `pdf-fix-xrefs` fill move it at the end just before the
`trailer`.  If no `xref` section exists, `pdf-fix-xrefs` won't mind and will just generate one.

Highlighting references with `M-?` (`M-x pdf-highlight-refs`) will enter a minor mode where the following additional key
bindings are available:

- `C-<up>` — move to the previous occurrence
- `C-<down>` — move to the next occurrence
- `C-g` or `<escape>` — remove highlighting and exit this mode

### Limitations

Only PDF-1.4 non-linearized format is supported.

PDF-1.5 introduced “object streams”.  See, before 1.5, PDF only had “stream objects”.  Object streams are stream objects
that can contain objects, but oddly enough, not stream objects.  If this gibberish doesn't make any sense, and it
doesn't, see the [PDF spec](http://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/pdf_reference_1-7.pdf),
section 3.4.6—3.4.7, but only if you're okay with completely losing faith in humanity.  Any case, starting with 1.5
there's a whole new definition of the cross-reference table optionally possible (object streams + `XRef` stream).  For
the time being, we don't support it.

For dealing with linearized PDFs or object streams, you can use [qpdf](http://qpdf.sourceforge.net/):

    qpdf --object-streams=disable -qdf input.pdf output.pdf

Now `output.pdf` should be a file that our little mode can work with.

Syntax highlighting is based on parsing the whole buffer, so if you throw a megabyte-order file at it it might feel
pretty slow.  I couldn't figure out how to make it work with Emacs' built-in font-locking support, because streams
contain arbitrary data that can break the fragile regexp-based syntax highlighting (my bad for not trying hard enough to
understand how do multi-line font locking with Emacs).

The parser might not work properly with DOS-style newlines.

Besides these known issues, there are of course an infinity of bugs.  Please file issues or pull requests for the finite
number of bugs you may find.
