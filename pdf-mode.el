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
  (prog1
      `((type . object)
        (offset . ,(match-beginning 0))
        (id . ,(string-to-number (match-string 1)))
        (rev . ,(string-to-number (match-string 2)))
        (data . ,(pdf--read)))
    (if (looking-at "endobj")
        (goto-char (match-end 0))
      (pdf--croak "Missing endobj"))))

(defun pdf--dict-lookup (dict propname)
  (cl-assoc propname (cdr (assq 'data dict))
            :test #'string-equal
            :key (lambda (prop)
                   (cdr (assq 'data prop)))))

(defun pdf--read-stream (dict)
  (let ((lenprop (cdr (pdf--dict-lookup dict "Length")))
        (offset (match-beginning 0))
        start curlen)
    (unless lenprop
      (pdf--croak "No Length in stream dictionary"))
    (goto-char (match-end 0))
    (delete-region (point)
                   (save-excursion (end-of-line) (point)))
    (forward-char)
    (setf start (point))
    (search-forward-regexp "^endstream")
    (setf curlen (max 0 (- (match-beginning 0) start 1)))
    ;; (add-text-properties start (match-beginning 0)
    ;;                      '(font-lock-multiline t))
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
      (start . ,start)
      (length . ,curlen)
      (dict . ,dict))))

(defun pdf--read-dictionary ()
  (goto-char (match-end 0))
  (let ((dict `((type . dictionary)
                (offset . ,(match-beginning 0))
                (data . ,(cl-loop until (or (eobp) (looking-at ">>"))
                                  collect `(,(pdf--read) . ,(pdf--read)))))))
    (unless (eobp)
      (goto-char (match-end 0)))
    (pdf--skip-whitespace)
    (if (looking-at "stream")
        (pdf--read-stream dict)
      dict)))

(defun pdf--read-name ()
  (goto-char (match-end 0))
  `((type . name)
    (offset . ,(match-beginning 0))
    (data . ,(when (looking-at (concat "\\(.*?\\)" *pdf--rx-delimiter*))
               (goto-char (match-end 1))
               (match-string 1)))))

(defun pdf--read-array ()
  (goto-char (match-end 0))
  (prog1
      `((type . array)
        (offset . ,(match-beginning 0))
        (data . ,(cl-loop until (or (eobp) (looking-at "\\]"))
                          collect (pdf--read))))
    (unless (eobp)
      (goto-char (match-end 0)))))

(defun pdf--read-number ()
  (prog1
      `((type . number)
        (offset . ,(match-beginning 0))
        (data . ,(string-to-number (match-string 0))))
    (goto-char (match-end 0))))

(defun pdf--read-ref ()
  (prog1
      `((type . ref)
        (offset . ,(match-beginning 1))
        (data . ((id . ,(string-to-number (match-string 2)))
                 (rev . ,(string-to-number (match-string 3))))))
    (goto-char (match-end 1))))

(defun pdf--read-bool ()
  (prog1
      `((type . bool)
        (offset . ,(match-beginning 1))
        (data . ,(match-string 1)))
    (goto-char (match-end 1))))

(defun pdf--read-null ()
  (prog1
      `((type . null)
        (offset . ,(match-beginning 1)))
    (goto-char (match-end 1))))

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
        (data . ,(apply #'string (reverse data)))))))

(defun pdf--read-hex-string ()
  (goto-char (match-end 0))
  (let ((offset (match-beginning 0))
        (data '()))
    (cl-flet ((collect (ch) (setf data (cons ch data))))
      (pdf--skip-whitespace)
      (while (looking-at "[[:xdigit:]][[:xdigit:]]?")
        (message (match-string 0))
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
        (data . ,(apply #'string (reverse data)))))))

(defun pdf--read-xref-section (start count)
  (goto-char (match-end 0))
  (pdf--skip-whitespace)
  (cl-loop while (looking-at "\\([[:digit:]]\\{10\\}\\) \\([[:digit:]]\\{5\\}\\) \\([fn]\\)")
           collect `((id . ,(incf start))
                     (offset . ,(string-to-number (match-string 1)))
                     (rev . ,(string-to-number (match-string 2)))
                     (fn . ,(match-string 3)))
           do (progn
                (goto-char (match-end 0))
                (pdf--skip-whitespace))))

(defun pdf--read-xref ()
  (let ((offset (match-beginning 0)) data)
    (goto-char (match-end 0))
    (pdf--skip-whitespace)
    (setf data
          (cl-loop while (looking-at "\\([[:digit:]]+\\)[[:space:]]+\\([[:digit:]]+\\)")
                   collect (let ((start (string-to-number (match-string 1)))
                                 (count (string-to-number (match-string 2))))
                             (pdf--read-xref-section start count))
                   do (pdf--skip-whitespace)))
    `((type . xref)
      (offset . ,offset)
      (end . ,(point))
      (data . ,data))))

(defun pdf--read-trailer ()
  (goto-char (match-end 0))
  `((type . trailer)
    (offset . ,(match-beginning 0))
    (data . ,(pdf--read))
    (end . ,(point))))

(defun pdf--read-startxref ()
  (goto-char (match-end 0))
  `((type . startxref)
    (offset . ,(match-beginning 0))
    (data . ,(pdf--read))
    (end . ,(point))))

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

       ((looking-at "xref")
        (pdf--read-xref))

       ((looking-at "trailer")
        (pdf--read-trailer))

       ((looking-at "startxref")
        (pdf--read-startxref))

       ((not (eobp))
        (pdf--croak "Can't parse that")))

    (pdf--skip-whitespace)))

(defun pdf--parse ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (cl-loop for thing = (pdf--read)
               while thing collect thing))))

;;; AST accessors

(defun pdf.type (node) (cdr (assq 'type node)))
(defun pdf.data (node) (cdr (assq 'data node)))
(defun pdf.offset (node) (cdr (assq 'offset node)))
(defun pdf.end (node) (cdr (assq 'end node)))
(defun pdf.dict-key (dict prop) (car (pdf--dict-lookup dict prop)))
(defun pdf.dict-val (dict prop) (cdr (pdf--dict-lookup dict prop)))
(defun pdf.id (node) (cdr (assq 'id node)))
(defun pdf.rev (node) (cdr (assq 'rev node)))

;;; --------------------------------------------------------------------

(defun pdf--dig-buffer (cont)
  (cl-loop for i in (pdf--parse)
           for type = (pdf.type i)
           when (eq 'object type) collect i into objects
           when (eq 'xref type) collect i into xref
           when (eq 'startxref type) collect i into startxref
           when (eq 'trailer type) collect i into trailer
           finally (return (funcall cont objects xref startxref trailer))))

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
     (lambda (objects xref startxref trailer)
       (let ((trailer-code (buffer-substring-no-properties
                            (pdf.offset (car trailer)) (pdf.end (car trailer)))))
         (pdf--do-reverse (append xref startxref trailer) del)
         (goto-char (point-max))
         (let ((xref (point)))
           (pdf--write-xref objects)
           (insert trailer-code
                   (format "startxref\n%d\n%%%%EOF" (- xref 1)))))))))

(defun pdf-fix-refs ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (pdf--fix-refs))))

;;; mode def

(defvar *pdf-font-lock-defaults*
  `((
     ;; multi-line is not reliable
     ;; ("\\(stream\\)\\(\\(?:.\\|\n\\)*?\\)\\(endstream\\)"
     ;;  (1 font-lock-keyword-face)
     ;;  (2 font-lock-doc-face)
     ;;  (3 font-lock-keyword-face))

     ("\\(%+\\)\\(.*\\)"
      (1 font-lock-comment-delimiter-face)
      (2 font-lock-comment-face))

     ("/[[:word:]]+" . font-lock-function-name-face)

     ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(obj\\)\\>"
      (1 font-lock-variable-name-face)
      (2 font-lock-keyword-face))

     ("\\<\\([[:digit:]]+\s+[[:digit:]]\\)+\s+\\(R\\)\\>"
      (1 font-lock-variable-name-face)
      (2 font-lock-variable-name-face))

     (,(regexp-opt '("obj" "endobj"
                     "stream" "endstream"
                     "xref" "startxref" "trailer")
                   'words)
      . font-lock-keyword-face)

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

  ;; (setf font-lock-multiline t)
  (setf font-lock-defaults *pdf-font-lock-defaults*))

(provide 'pdf-mode)
;;; pdf-mode.el ends here
