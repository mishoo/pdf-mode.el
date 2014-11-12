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

;;

;;; Code:

(require 'cl)

(defvar *pdf--rx-delimiter* "\\(?:[][)(><}{/%[:space:]]\\|$\\)")
(defvar *pdf-fix-stream-length* nil)

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
        (pdf--croak "Can't parse that")))

    (pdf--skip-whitespace)))

(defun pdf--croak (msg)
  (error (format "%s (%d:%d)" msg
                 ;; always enjoyed the similitude between the
                 ;; following two function names:
                 (line-number-at-pos)
                 (current-column))))

(defun pdf--skip-whitespace ()
  (while (looking-at "[[:space:]]+\\|\\(?:%.*\\)")
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
    (delete-region (point)
                   (save-excursion (end-of-line) (point)))
    (forward-char)
    (setf start (point))
    (unless (search-forward-regexp "^endstream" nil t)
      (pdf--croak "Missing endstream"))
    (setf curlen (max 0 (- (match-beginning 0) start 1)))
    (goto-char (match-end 0))
    (when (and *pdf-fix-stream-length*
               (/= curlen (cdr (assq 'data lenprop))))
      (save-excursion
        (goto-char (cdr (assq 'offset lenprop)))
        (delete-region (point) (progn
                                 (skip-chars-forward "+-[:digit:]")
                                 (point)))
        (insert (number-to-string curlen))))
    (setf (cdr (assq 'data lenprop)) curlen)
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
    (data . ((id . ,(string-to-number (match-string 2)))
             (rev . ,(string-to-number (match-string 3)))))))

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
  (goto-char (match-end 0))
  (pdf--skip-whitespace)
  (cl-loop while (looking-at "\\([[:digit:]]\\{10\\}\\) \\([[:digit:]]\\{5\\}\\) \\([fn]\\)")
           collect `((type . xref-section)
                     (offset . ,(match-beginning 0))
                     (end . ,(match-end 0))
                     (address . ,(string-to-number (match-string 1)))
                     (id . ,(incf start))
                     (rev . ,(string-to-number (match-string 2)))
                     (fn . ,(match-string 3)))
           do (progn
                (goto-char (match-end 0))
                (pdf--skip-whitespace))))

(defun pdf--read-xref ()
  (let ((offset (match-beginning 1)))
    (goto-char (match-end 1))
    (pdf--skip-whitespace)
    (let ((data (cl-loop while (looking-at "\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)")
                         append (let ((start (string-to-number (match-string 1)))
                                      (count (string-to-number (match-string 2))))
                                  (pdf--read-xref-section start count))
                         do (pdf--skip-whitespace))))
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
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cl-loop for thing = (pdf--read)
               while thing collect thing))))

;;; AST accessors

(defun pdf--dict-lookup (dict propname)
  (cl-assoc propname (cdr (assq 'data dict))
            :test #'string-equal
            :key (lambda (prop)
                   (cdr (assq 'data prop)))))

(defun pdf.type (node) (cdr (assq 'type node)))
(defun pdf.data (node) (cdr (assq 'data node)))
(defun pdf.offset (node) (cdr (assq 'offset node)))
(defun pdf.start (node) (cdr (assq 'start node)))
(defun pdf.end (node) (cdr (assq 'end node)))
(defun pdf.length (node) (cdr (assq 'length node)))
(defun pdf.dict-key (dict prop) (car (pdf--dict-lookup dict prop)))
(defun pdf.dict-val (dict prop) (cdr (pdf--dict-lookup dict prop)))
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

(defun pdf--fontify-buffer ()
  (font-lock-mode -1)
  (mapc
   (lambda (node)
     (pdf.visit
      node
      (lambda (node stage)
        (when (eq stage 'before)
          (let* ((start (pdf.offset node))
                 (end (pdf.end node))
                 (type (pdf.type node))
                 (prop (case type
                         (number 'font-lock-constant-face)
                         (name 'font-lock-function-name-face)
                         (lstring 'font-lock-string-face)
                         (xstring 'font-lock-string-face)
                         (bool 'font-lock-builtin-face)
                         (null 'font-lock-builtin-face)
                         (ref 'font-lock-preprocessor-face)
                         (xref-section 'font-lock-preprocessor-face)
                         ((object trailer xref startxref) 'font-lock-keyword-face)
                         (stream
                          (setf start (pdf.start node)
                                end (+ start (pdf.length node)))
                          'font-lock-doc-face)
                         (otherwise 'default))))
            (message "%s" type)
            (when (and start end prop
                       (>= start (point-min))
                       (<= end (point-max)))
              (add-text-properties start end `(face ,prop))))))))
   (pdf--parse)))

;;; --------------------------------------------------------------------

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

(defun pdf--fix-refs ()
  (let ((*pdf-fix-stream-length* t)
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
                   (format "startxref\n%d\n%%%%EOF" (- xref 1)))))
       (pdf--fontify-buffer)))))

(defun pdf-fix-refs ()
  (interactive)
  (atomic-change-group
    (save-excursion
      (save-restriction
        (widen)
        (pdf--fix-refs))))
  nil)

;;; mode def

(defvar *pdf-font-lock-defaults*
  `((

     ("\\(%+\\)\\(.*\\)"
      (1 font-lock-comment-delimiter-face)
      (2 font-lock-comment-face))

     ("/[[:word:]]+" . font-lock-function-name-face)

     ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(obj\\)\\>"
      (1 font-lock-variable-name-face)
      (2 font-lock-keyword-face))

     ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(R\\)\\>"
      (1 font-lock-type-face)
      (2 font-lock-builtin-face))

     (,(regexp-opt '("obj" "endobj"
                     "stream" "endstream"
                     "xref" "startxref" "trailer")
                   'words)
      . font-lock-keyword-face)

     (,(regexp-opt '("true" "false" "null")) . font-lock-builtin-face)

     ("<\\([a-fA-F0-9[:space:]]+\\)>" (1 font-lock-string-face))

     ("(\\(.*?\\))" (1 font-lock-string-face))

     ("[-+]?[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?" . font-lock-constant-face)

     )))

(define-derived-mode pdf-mode
  fundamental-mode "PDF"
  "Major mode for editing PDF."

  (make-variable-buffer-local 'comment-start)

  (modify-syntax-entry ?< "(>" pdf-mode-syntax-table)
  (modify-syntax-entry ?> ")<" pdf-mode-syntax-table)
  (dolist (i '(?. ?- ?_ ?* ?+))
    (modify-syntax-entry i "w" pdf-mode-syntax-table))

  ;; WTF?  this breaks our parser!
  ;; (modify-syntax-entry ?% "< b" pdf-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" pdf-mode-syntax-table)

  (add-hook 'write-contents-functions 'pdf-fix-refs)
  (setf comment-start "%"
        comment-end "")

  (setf font-lock-defaults *pdf-font-lock-defaults*)
  ;; (setf font-lock-defaults '(nil t))
  )

(provide 'pdf-mode)
;;; pdf-mode.el ends here
