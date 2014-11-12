;;; -*- lexical-binding: t -*-
;;; pdf-mode.el --- Major mode for editing PDF files

;; Copyright (C) 2014  Mihai Bazon

;; Author: Mihai Bazon <mihai.bazon@gmail.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing raw PDF files.

;;; Code:

(require 'cl)

;;; --- parser and syntax highlighting ---------------------------------

(defvar *pdf--rx-delimiter* "\\(?:[][)(><}{/%[:space:]]\\|$\\)")
(defvar *pdf--fix-stream-length* nil)
(defvar *pdf--highlight* nil)
(defvar *pdf--no-parse-errors* nil)
(defvar *pdf--error-locations* nil)

(defun pdf--read ()
  (pdf--skip-whitespace)
  (prog1
      (cond
       ((looking-at "\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+obj")
        (pdf--read-object))

       ((looking-at "<<")
        (pdf--read-dictionary))

       ((looking-at "/")
        (pdf--read-name))

       ((looking-at "\\[")
        (pdf--read-array))

       ((looking-at (concat "\\(\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)[[:space:]]+R\\)"
                            *pdf--rx-delimiter*))
        (pdf--read-ref))

       ((looking-at "[-+]?[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?")
        (pdf--read-number))

       ((looking-at "[-+]?\\(?:\\.[[:digit:]]+\\)")
        (pdf--read-number))

       ((looking-at (concat "\\(true\\|false\\)" *pdf--rx-delimiter*))
        (pdf--read-bool))

       ((looking-at (concat "\\(null\\)" *pdf--rx-delimiter*))
        (pdf--read-null))

       ((looking-at "(")
        (pdf--read-literal-string))

       ((looking-at "<")
        (pdf--read-hex-string))

       ((looking-at (concat "\\(xref\\)" *pdf--rx-delimiter*))
        (pdf--read-xref))

       ((looking-at (concat "\\(trailer\\)" *pdf--rx-delimiter*))
        (pdf--read-trailer))

       ((looking-at (concat "\\(startxref\\)" *pdf--rx-delimiter*))
        (pdf--read-startxref))

       ((not (eobp))
        (pdf--croak "Can't parse that" t)))

    (pdf--skip-whitespace)))

(defun pdf--croak (msg &optional skip)
  (push (point) *pdf--error-locations*)
  (unless *pdf--no-parse-errors*
    (error (format "%s (%d:%d)" msg
                   ;; always enjoyed the similitude between the
                   ;; following two function names:
                   (line-number-at-pos)
                   (current-column))))
  (when skip
    (when *pdf--highlight*
      (remove-text-properties (point) (1+ (point)) '(face nil)))
    (forward-char 1)))

(defun pdf--skip-whitespace ()
  (while (looking-at "\\(?:[[:space:]]+\\|\\(%.*\\)\\)")
    (when (and *pdf--highlight* (match-beginning 1))
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           '(face font-lock-comment-face)))
    (goto-char (match-end 0))))

(defun pdf--read-object ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (id (string-to-number (match-string 1)))
        (rev (string-to-number (match-string 2)))
        (data (pdf--read)))
    (unless (looking-at "endobj")
      (pdf--croak "Missing endobj"))
    (goto-char (match-end 0))
    `((type . object)
      (offset . ,offset)
      (end . ,(point))
      (id . ,id)
      (rev . ,rev)
      (data . ,data))))

(defun pdf--read-stream (dict)
  (let ((offset (match-beginning 1))
        (lenprop (cdr (pdf--dict-lookup dict "Length")))
        start curlen)
    (unless lenprop
      (pdf--croak "No Length in stream dictionary"))
    (goto-char (match-end 1))
    (setf start (1+ (point)))
    (unless (search-forward-regexp "^endstream" nil t)
      (pdf--croak "Missing endstream"))
    (setf curlen (max 0 (- (match-beginning 0) start 1)))
    (goto-char (match-end 0))
    (when *pdf--fix-stream-length*
      (save-excursion
        (goto-char (pdf.offset lenprop))
        (delete-region (point) (pdf.end lenprop))
        (insert (number-to-string curlen))))
    `((type . stream)
      (offset . ,offset)
      (end . ,(point))
      (start . ,start)
      (length . ,curlen)
      (dict . ,dict))))

(defun pdf--read-dictionary ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (data (cl-loop do (pdf--skip-whitespace)
                       until (or (eobp) (looking-at ">>"))
                       collect `(,(pdf--read) . ,(pdf--read)))))
    (unless (eobp)
      (goto-char (match-end 0)))
    (let ((dict `((type . dictionary)
                  (offset . ,offset)
                  (end . ,(point))
                  (data . ,data))))
      (pdf--skip-whitespace)
      (if (looking-at (concat "\\(stream\\)" *pdf--rx-delimiter*))
          (pdf--read-stream dict)
        dict))))

(defun pdf--read-array ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (data (cl-loop do (pdf--skip-whitespace)
                       until (or (eobp) (looking-at "\\]"))
                       collect (pdf--read))))
    (unless (eobp)
      (goto-char (match-end 0)))
    `((type . array)
      (offset . ,offset)
      (end . ,(point))
      (data . ,data))))

(defun pdf--read-name ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (data (when (looking-at (concat "\\(.*?\\)" *pdf--rx-delimiter*))
                (goto-char (match-end 1))
                (match-string 1))))
    `((type . name)
      (offset . ,offset)
      (end . ,(point))
      (data . ,data))))

(defun pdf--read-number ()
  (goto-char (match-end 0))
  `((type . number)
    (offset . ,(match-beginning 0))
    (end . ,(point))
    (data . ,(string-to-number (match-string 0)))))

(defun pdf--read-ref ()
  (goto-char (match-end 1))
  `((type . ref)
    (offset . ,(match-beginning 1))
    (end . ,(point))
    (id . ,(string-to-number (match-string 2)))
    (rev . ,(string-to-number (match-string 3)))))

(defun pdf--read-bool ()
  (goto-char (match-end 1))
  `((type . bool)
    (offset . ,(match-beginning 1))
    (end . ,(point))
    (data . ,(match-string 1))))

(defun pdf--read-null ()
  (goto-char (match-end 1))
  `((type . null)
    (offset . ,(match-beginning 1))
    (end . ,(point))))

(defun pdf--read-literal-string ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (parens 0)
        (data '()))
    (cl-flet ((collect (ch) (setf data (cons ch data))))
      (catch 'out
        (while (not (eobp))
          (cond ((looking-at "(")
                 (forward-char 1)
                 (incf parens)
                 (collect 40))
                ((looking-at ")")
                 (forward-char 1)
                 (when (zerop parens)
                   (throw 'out t))
                 (decf parens)
                 (collect 41))
                ((looking-at "\\\\[\\\n\r\t\b\f()]")
                 (goto-char (match-end 0))
                 (collect (char-before)))
                ((looking-at "\\\\\\([[:digit:]]\\{3\\}\\)")
                 (goto-char (match-end 0))
                 (collect (string-to-number (match-string 1) 8)))
                ((looking-at "\\\\")
                 (forward-char 1))
                (t
                 (collect (char-after))
                 (forward-char 1)))))
      `((type . lstring)
        (offset . ,offset)
        (end . ,(point))
        (data . ,(apply #'string (reverse data)))))))

(defun pdf--read-hex-string ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (data '()))
    (cl-flet ((collect (ch) (setf data (cons ch data))))
      (pdf--skip-whitespace)
      (while (looking-at "[[:xdigit:]][[:xdigit:]]?")
        (goto-char (match-end 0))
        (let ((code (string-to-number (match-string 0) 16)))
          (when (< code 16)
            (setf code (* code 16)))
          (collect code))
        (pdf--skip-whitespace))
      (unless (looking-at ">")
        (pdf--croak "Unterminated hex string"))
      (forward-char 1)
      `((type . xstring)
        (offset . ,offset)
        (end . ,(point))
        (data . ,(apply #'string (reverse data)))))))

(defun pdf--read-xref-section (start count)
  (cl-loop while (looking-at "\\([[:digit:]]\\{10\\}\\) \\([[:digit:]]\\{5\\}\\) \\([fn]\\)[[:space:]]*$")
           collect `((type . xref-section)
                     (offset . ,(match-beginning 0))
                     (end . ,(match-end 0))
                     (address . ,(string-to-number (match-string 1)))
                     (id . ,(incf start))
                     (rev . ,(string-to-number (match-string 2)))
                     (fn . ,(match-string 3)))
           do (goto-char (1+ (match-end 0)))))

(defun pdf--read-xref ()
  (let ((offset (match-beginning 1)))
    (goto-char (match-end 1))
    (pdf--skip-whitespace)
    (let ((data (cl-loop while (looking-at "\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)[[:space:]]*$")
                         append (let ((start (string-to-number (match-string 1)))
                                      (count (string-to-number (match-string 2))))
                                  (goto-char (1+ (match-end 0)))
                                  (pdf--read-xref-section start count)))))
      `((type . xref)
        (offset . ,offset)
        (end . ,(point))
        (data . ,data)))))

(defun pdf--read-trailer ()
  (goto-char (match-end 1))
  `((type . trailer)
    (offset . ,(match-beginning 1))
    (data . ,(pdf--read))
    (end . ,(point))))

(defun pdf--read-startxref ()
  (goto-char (match-end 1))
  `((type . startxref)
    (offset . ,(match-beginning 1))
    (data . ,(pdf--read))
    (end . ,(point))))

(defun pdf--parse ()
  (setf *pdf--error-locations* '())
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cl-loop for thing = (pdf--read)
               when thing collect thing
               until (eobp)))))

;;; AST utilities

(defun pdf--dict-lookup (dict propname)
  (cl-assoc propname (cdr (assq 'data dict))
            :test #'string-equal
            :key (lambda (prop)
                   (cdr (assq 'data prop)))))

(defun pdf.type (node) (cdr (assq 'type node)))
(defun pdf.data (node) (cdr (assq 'data node)))
(defun pdf.offset (node) (cdr (assq 'offset node)))
(defun pdf.address (node) (cdr (assq 'address node)))
(defun pdf.start (node) (cdr (assq 'start node)))
(defun pdf.end (node) (cdr (assq 'end node)))
(defun pdf.length (node) (cdr (assq 'length node)))
(defun pdf.dict-key (dict prop) (car (pdf--dict-lookup dict prop)))
(defun pdf.dict-val (dict prop) (cdr (pdf--dict-lookup dict prop)))
(defun pdf.object-type (obj)
  (let ((dict (pdf.data obj)))
    (when (and dict (eq 'dictionary (pdf.type dict)))
      (let ((type (pdf.dict-val dict "Type")))
        (and type (pdf.data type))))))
(defun pdf.id (node) (cdr (assq 'id node)))
(defun pdf.rev (node) (cdr (assq 'rev node)))
(defun pdf.dict (node) (cdr (assq 'dict node)))

(defun pdf.visit (node func)
  (funcall func node 'before)
  (case (pdf.type node)
    (object (pdf.visit (pdf.data node) func))
    (stream (pdf.visit (pdf.dict node) func))
    (dictionary (mapc (lambda (x)
                        (pdf.visit (car x) func)
                        (pdf.visit (cdr x) func))
                      (pdf.data node)))
    ((array xref) (mapc (lambda (x)
                          (pdf.visit x func))
                        (pdf.data node)))
    (trailer (pdf.visit (pdf.data node) func)))
  (funcall func node 'after))

;;; fontification; should be better built-in the parser

(defvar *pdf--needs-fontification* nil)

(defun pdf--ensure-fontification (&rest ignore)
  (setf *pdf--needs-fontification* t))

(defun pdf--fontify-node (node)
  (pdf.visit
   node
   (lambda (node stage)
     (when (eq stage 'before)
       (let* ((start (pdf.offset node))
              (end (pdf.end node))
              (type (pdf.type node))
              (prop (case type
                      (number 'font-lock-constant-face)
                      (name 'font-lock-builtin-face)
                      (lstring 'font-lock-string-face)
                      (xstring 'font-lock-string-face)
                      (bool 'font-lock-constant-face)
                      (null 'font-lock-constant-face)
                      (ref 'font-lock-type-face)
                      (xref-section 'font-lock-type-face)
                      ((object trailer xref startxref) 'font-lock-keyword-face)
                      (stream
                       (setf start (pdf.start node)
                             end (+ start (pdf.length node)))
                       'font-lock-doc-face)
                      (otherwise 'default))))
         (when (and start end prop
                    (>= start (point-min))
                    (<= end (point-max)))
           (add-text-properties start end `(face ,prop))))))))

(defun pdf--fontify-buffer ()
  (when *pdf--needs-fontification*
    (setf *pdf--needs-fontification* nil)
    (let ((*pdf--highlight* t)
          (*pdf--no-parse-errors* t))
      (mapc #'pdf--fontify-node (pdf--parse)))))

(defun pdf--fontify-region (begin end &optional verbose)
  (pdf--fontify-buffer))

;;; --- highlight references -------------------------------------------

(defun pdf--highlight-regions (regions &optional face)
  (cl-loop for (start end myface) in regions
           do (let ((o (make-overlay start end)))
                (overlay-put o 'face (or myface face 'highlight))
                (overlay-put o 'evaporate t)
                (overlay-put o 'pdf-highlight t))))

(defun pdf--highlight-get-overlays ()
  (sort (cl-loop for o in (overlays-in (point-min) (point-max))
                 when (overlay-get o 'pdf-highlight)
                 collect o)
        (lambda (a b)
          (< (overlay-start a) (overlay-start b)))))

(defun pdf-goto-next-symbol ()
  "Move to the next highlighted symbol.  See `pdf-highlight-refs'"
  (interactive)
  (catch 'done
    (dolist (i (pdf--highlight-get-overlays))
      (let ((x (overlay-start i)))
        (when (> x (point))
          (goto-char x)
          (throw 'done nil))))))

(defun pdf-goto-prev-symbol ()
  "Move to the previous highlighted symbol.  See `pdf-highlight-refs'"
  (interactive)
  (catch 'done
    (dolist (i (reverse (pdf--highlight-get-overlays)))
      (when (< (overlay-end i) (point))
        (goto-char (overlay-start i))
        (throw 'done nil)))))

(defun pdf-forget-highlighting ()
  "Remove symbol highlight overlays.  See `pdf-highlight-refs'"
  (interactive)
  (remove-overlays (point-min) (point-max) 'pdf-highlight t)
  (pdf--highlight-mode 0))

(defvar pdf--highlight-mode-keymap (make-sparse-keymap))
(define-key pdf--highlight-mode-keymap (kbd "C-<down>") 'pdf-goto-next-symbol)
(define-key pdf--highlight-mode-keymap (kbd "C-<up>") 'pdf-goto-prev-symbol)
(define-key pdf--highlight-mode-keymap (kbd "<escape>") 'pdf-forget-highlighting)
(define-key pdf--highlight-mode-keymap (kbd "C-g") 'pdf-forget-highlighting)

(define-minor-mode pdf--highlight-mode
  "Internal mode used by `pdf-mode` while highlighting references"
  nil
  nil
  pdf--highlight-mode-keymap)

(defun pdf-dig-at-point (cont)
  (let ((*pdf--no-parse-errors* t)
        (pt (point))
        (path '())
        (defs (make-hash-table))
        (refs (make-hash-table))
        (pages (make-hash-table)))
    (mapc
     (lambda (node)
       (pdf.visit
        node
        (lambda (node stage)
          (when (eq stage 'before)
            (when (and (<= (pdf.offset node) pt)
                       (<= pt (pdf.end node)))
              (push node path))
            (case (pdf.type node)
              (object
               (puthash (pdf.id node) node defs)
               (when (string-equal "Page" (pdf.object-type node))
                 (puthash (pdf.id node) node pages)))
              (ref (puthash (pdf.id node)
                            (cons node (gethash (pdf.id node) refs '()))
                            refs)))))))
     (pdf--parse))
    (funcall cont path defs refs pages)))

(defun pdf-highlight-refs ()
  "Highlight references to the node at point.  The node is
expected to be an object reference (of the form 10 0 R), or an
object.  This enters a minor mode where the keys C-<up> /
C-<down> allow you to move through the highlighted refs.  Exit
this mode with <escape> or C-g."
  (interactive)
  (pdf-forget-highlighting)
  (pdf-dig-at-point
   (lambda (path defs refs pages)
     (when path
       (let ((node (car path)))
         (case (pdf.type node)
           ((ref object)
            (let ((refs (gethash (pdf.id node) refs))
                  (def (gethash (pdf.id node) defs)))
              (when (or refs def)
                (when def
                  (pdf--highlight-regions (list (list (pdf.offset def)
                                                      (pdf.end def)
                                                      'secondary-selection))))
                (pdf--highlight-regions (mapcar (lambda (ref)
                                                  (list (pdf.offset ref)
                                                        (pdf.end ref)))
                                                refs))
                (pdf--highlight-mode)
                (message "%d references found" (length refs)))))))))))

(defvar *pdf-locations* '())

(defun pdf--goto-location (offset)
  (when (or (not *pdf-locations*)
            (/= (point) (car *pdf-locations*)))
    (push (point) *pdf-locations*))
  (goto-char offset))

(defun pdf-pop-location ()
  "Return to the previous location, usually after a jump with
`pdf-find-definition'."
  (interactive)
  (let ((loc (pop *pdf-locations*)))
    (when loc
      (goto-char loc))))

(defun pdf-find-definition ()
  "Locate the definition of the object at point.  The object at
point should be a reference (i.e. 10 0 R) or a xref
section (i.e. 0000000234 00000 f)."
  (interactive)
  (pdf-dig-at-point
   (lambda (path defs refs pages)
     (unless (when path
               (let ((node (car path)))
                 (case (pdf.type node)
                   ((ref)
                    (let ((def (gethash (pdf.id node) defs)))
                      (when def
                        (pdf--goto-location (pdf.offset def))
                        t)))
                   ((xref-section)
                    (pdf--goto-location (1+ (pdf.address node)))
                    t))))
       (message "No definition found")))))

;;; --- rewrite xref utility -------------------------------------------

(defun pdf--dig-buffer (cont)
  (cl-loop with nodes = (pdf--parse)
           for i in nodes
           for type = (pdf.type i)
           when (eq 'object type) collect i into objects
           when (eq 'xref type) collect i into xref
           when (eq 'startxref type) collect i into startxref
           when (eq 'trailer type) collect i into trailer
           finally (return (funcall cont nodes objects xref startxref trailer))))

(defun pdf--do-reverse (things func)
  (mapc func
        (sort (copy-list things)
              (lambda (a b)
                (> (pdf.offset a) (pdf.offset b))))))

(defun pdf--write-xref (objects)
  (let ((objects (sort (copy-list objects)
                       (lambda (a b)
                         (< (pdf.id a) (pdf.id b))))))
    ;; XXX: optimize this perhaps.  we can insert a single header
    ;; entry for consecutive IDs.
    (insert "xref\n0 1\n0000000000 65535 f \n")
    (dolist (obj objects)
      (insert (format "%d 1\n%010d %05d n \n"
                      (pdf.id obj)
                      (- (pdf.offset obj) 1)
                      (pdf.rev obj))))
    (insert "\n")))

(defun pdf--fix-xrefs ()
  (let ((*pdf--fix-stream-length* t)
        (del (lambda (x)
               (delete-region (pdf.offset x) (pdf.end x)))))
    (pdf--dig-buffer
     (lambda (nodes objects xref startxref trailer)
       (let ((trailer-code (buffer-substring-no-properties
                            (pdf.offset (car trailer))
                            (pdf.end (car trailer)))))
         (pdf--do-reverse (append xref startxref trailer) del)
         (goto-char (point-max))
         (let ((xref (point)))
           (pdf--write-xref objects)
           (insert trailer-code
                   (format "startxref\n%d\n%%%%EOF" (- xref 1)))))))))

(defun pdf-fix-xrefs ()
  "Rewrite the xref, trailer and startxref sections based on the
current buffer contents.  Note that any comments in that area
will be lost.  This also fixes the /Length property for all PDF
streams in the buffer.

This function is called automatically before saving the buffer.
Note that in case of a parse error, this function will fail and
the buffer will not be saved.  I'm not entirely sure this is a
good idea, but I got into it."
  (interactive)
  (atomic-change-group
    (save-excursion
      (save-restriction
        (widen)
        (pdf--fix-xrefs))))
  nil)

(defun pdf-cleanup ()
  (interactive)
  "Removes any objects that are not referenced."
  (pdf-dig-at-point
   (lambda (path defs refs pages)
     (let ((count 0)
           (objects '()))
       (maphash (lambda (key val)
                  (push val objects))
                defs)
       (pdf--do-reverse objects
                        (lambda (obj)
                          (unless (gethash (pdf.id obj) refs)
                            (incf count)
                            (delete-region (pdf.offset obj)
                                           (pdf.end obj)))))
       (message "%d object(s) removed" count)))))

;;; --------------------------------------------------------------------

(defvar *pdf--new-object-template* "%d 0 obj <<
  |
>> endobj")

(defvar *pdf--new-stream-template* "%d 0 obj <<
  /Length 0
>> stream
|
endstream endobj")

(defun pdf-new-object (&optional stream)
  "Insert a new object definition.  Pass a prefix argument to
make it a stream object.  The new object ID will be one more than
the maximum ID among objects in the buffer."
  (interactive "P")
  (pdf--dig-buffer
   (lambda (nodes objects &rest ignore)
     (let ((max-id (reduce #'max objects :key #'pdf.id :initial-value 0)))
       (insert (format (if stream
                           *pdf--new-stream-template*
                         *pdf--new-object-template*) (+ max-id 1)))
       (search-backward "|")
       (delete-forward-char 1)))))

;; (defvar *pdf-font-lock-defaults*
;;   `((

;;      ("\\(%+\\)\\(.*\\)"
;;       (1 font-lock-comment-delimiter-face)
;;       (2 font-lock-comment-face))

;;      ("/[[:word:]]+" . font-lock-function-name-face)

;;      ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(obj\\)\\>"
;;       (1 font-lock-variable-name-face)
;;       (2 font-lock-keyword-face))

;;      ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(R\\)\\>"
;;       (1 font-lock-type-face)
;;       (2 font-lock-builtin-face))

;;      (,(regexp-opt '("obj" "endobj"
;;                      "stream" "endstream"
;;                      "xref" "startxref" "trailer")
;;                    'words)
;;       . font-lock-keyword-face)

;;      (,(regexp-opt '("true" "false" "null")) . font-lock-builtin-face)

;;      ("<\\([a-fA-F0-9[:space:]]+\\)>" (1 font-lock-string-face))

;;      ("(\\(.*?\\))" (1 font-lock-string-face))

;;      ("[-+]?[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?" . font-lock-constant-face)

;;      )))

(define-derived-mode pdf-mode
  fundamental-mode "PDF"
  "Major mode for editing PDF."

  (make-variable-buffer-local 'comment-start)
  (make-variable-buffer-local '*pdf--error-locations*)
  (make-variable-buffer-local '*pdf--needs-fontification*)
  (make-variable-buffer-local 'font-lock-fontify-buffer-function)
  (make-variable-buffer-local 'font-lock-fontify-region-function)
  (make-variable-buffer-local 'next-error-function)

  ;;;;; syntax

  (modify-syntax-entry ?< "(>" pdf-mode-syntax-table)
  (modify-syntax-entry ?> ")<" pdf-mode-syntax-table)
  (dolist (i '(?. ?- ?_ ?* ?+))
    (modify-syntax-entry i "w" pdf-mode-syntax-table))

  ;; WTF?  this breaks our parser!
  ;; (modify-syntax-entry ?% "< b" pdf-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" pdf-mode-syntax-table)

  (add-hook 'write-contents-functions 'pdf-fix-xrefs nil t)
  (setf comment-start "%"
        comment-end "")

  ;;;;; font-lock

  (pdf--ensure-fontification)
  (setf font-lock-fontify-buffer-function 'pdf--fontify-buffer)
  (setf font-lock-fontify-region-function 'pdf--fontify-region)
  (jit-lock-register 'pdf--fontify-region)

  (add-hook 'before-change-functions 'pdf--ensure-fontification nil t)

  ;; (setf font-lock-defaults *pdf-font-lock-defaults*)
  ;; (setf font-lock-defaults '(nil t))

  )

;;; --- key bindings ---------------------------------------------------

(define-key pdf-mode-map (kbd "C-c o") 'pdf-new-object)
(define-key pdf-mode-map (kbd "M-?") 'pdf-highlight-refs)
(define-key pdf-mode-map (kbd "M-.") 'pdf-find-definition)
(define-key pdf-mode-map (kbd "M-,") 'pdf-pop-location)

(provide 'pdf-mode)
;;; pdf-mode.el ends here
