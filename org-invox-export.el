;;; org-invox-export.el --- PDF export for org-invox -*- lexical-binding: t -*-

;; Copyright (C) 2026 Manu Mohan

;; Author: Manu Mohan
;; URL: https://github.com/manu-r-n/org-invox
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides PDF export functionality for org-invox.
;; Generates professional invoices via LaTeX/pdflatex.

;;; Code:

(require 'org-invox)

(defun org-invox-export--read-field (field-name)
  "Read FIELD-NAME value from the current invoice buffer's org tables."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s: \\(.+\\)$" (upcase field-name)) nil t)
      (string-trim (match-string 1)))))

(defun org-invox-export--read-table-field (heading field)
  "Read FIELD value from the org table under HEADING."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*\\* %s" heading) nil t)
      (when (re-search-forward "^|" nil t)
        (beginning-of-line)
        (let ((table-begin (point)))
          (forward-paragraph)
          (let ((table-text (buffer-substring-no-properties table-begin (point))))
            (when (string-match (format "| %s *| \\([^|]+\\)|" (regexp-quote field)) table-text)
              (string-trim (match-string 1 table-text)))))))))

(defun org-invox-export--parse-invoice ()
  "Parse the current invoice buffer into a property list."
  (let ((props '()))
    ;; File-level properties
    (dolist (key '("INVOICE_NUMBER" "INVOICE_DATE" "DUE_DATE" "STATUS"))
      (let ((val (org-invox-export--read-field key)))
        (when val (setq props (plist-put props (intern (concat ":" (downcase key))) val)))))
    ;; From details (from org properties)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* From" nil t)
        (dolist (pair '(("FROM_NAME" . :from-name)
                        ("FROM_COMPANY" . :from-company)
                        ("FROM_EMAIL" . :from-email)
                        ("FROM_PHONE" . :from-phone)))
          (let ((val (org-entry-get (point) (car pair))))
            (when val (setq props (plist-put props (cdr pair) val)))))
        ;; Read address from example block
        (when (re-search-forward "#\\+begin_example" nil t)
          (forward-line 1)
          (let ((start (point)))
            (when (re-search-forward "#\\+end_example" nil t)
              (forward-line 0)
              (setq props (plist-put props :from-address
                                    (string-trim (buffer-substring-no-properties start (point))))))))))
    ;; To details (from org properties)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* To" nil t)
        (dolist (pair '(("TO_NAME" . :to-name)
                        ("TO_COMPANY" . :to-company)
                        ("TO_EMAIL" . :to-email)
                        ("TO_CONTACT" . :to-contact)))
          (let ((val (org-entry-get (point) (car pair))))
            (when val (setq props (plist-put props (cdr pair) val)))))
        ;; Read address from example block
        (when (re-search-forward "#\\+begin_example" nil t)
          (forward-line 1)
          (let ((start (point)))
            (when (re-search-forward "#\\+end_example" nil t)
              (forward-line 0)
              (setq props (plist-put props :to-address
                                    (string-trim (buffer-substring-no-properties start (point))))))))))
    ;; Line items - parse from the table
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Line Items" nil t)
        (when (re-search-forward "^|[^-]" nil t)
          (beginning-of-line)
          ;; Skip header row
          (forward-line 1)
          ;; Skip separator
          (when (looking-at "^|-") (forward-line 1))
          ;; Read data rows until we hit subtotal
          (let (items)
            (while (and (looking-at "^| *\\([0-9]+\\) *|")
                        (not (eobp)))
              (let* ((line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))
                     (cells (split-string line "|" t)))
                (when (>= (length cells) 5)
                  (push (list :num (string-trim (nth 0 cells))
                              :description (string-trim (nth 1 cells))
                              :hours (string-trim (nth 2 cells))
                              :rate (string-trim (nth 3 cells))
                              :amount (string-trim (nth 4 cells)))
                        items)))
              (forward-line 1))
            (setq props (plist-put props :line-items (nreverse items)))
            ;; Read subtotal, tax, total from remaining rows
            (while (and (not (eobp)) (looking-at "^|"))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (cond
                 ((string-match "Subtotal.*| *\\*?\\([0-9.]+\\)\\*? *|" line)
                  (setq props (plist-put props :subtotal (match-string 1 line))))
                 ((string-match "\\(HST\\|GST\\|VAT\\|Tax\\|[A-Z]+\\) *(\\([0-9.]+\\)%).*| *\\*?\\([0-9.]+\\)\\*? *|" line)
                  (setq props (plist-put props :tax-label (match-string 1 line)))
                  (setq props (plist-put props :tax-rate (match-string 2 line)))
                  (setq props (plist-put props :tax-amount (match-string 3 line))))
                 ((string-match "TOTAL.*| *\\*?\\([0-9.]+\\)\\*? *|" line)
                  (setq props (plist-put props :total (match-string 1 line)))
                  (when (string-match "TOTAL *(\\([A-Z]+\\))" line)
                    (setq props (plist-put props :currency (match-string 1 line)))))))
              (forward-line 1))))))
    ;; Payment terms
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Payment Terms" nil t)
        (forward-line 1)
        (setq props (plist-put props :payment-terms
                               (string-trim (buffer-substring-no-properties
                                             (point) (line-end-position)))))))
    ;; Invoice period
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Invoice Period" nil t)
        (forward-line 1)
        (setq props (plist-put props :invoice-period
                               (string-trim (buffer-substring-no-properties
                                             (point) (line-end-position)))))))
    props))

(defun org-invox-export--latex-escape (str)
  "Escape special LaTeX characters in STR."
  (if (null str) ""
    (let ((result str))
      (dolist (pair '(("\\\\" . "\\textbackslash{}")
                      ("&" . "\\&")
                      ("%" . "\\%")
                      ("\\$" . "\\$")
                      ("#" . "\\#")
                      ("_" . "\\_")
                      ("{" . "\\{")
                      ("}" . "\\}")
                      ("~" . "\\textasciitilde{}")
                      ("\\^" . "\\textasciicircum{}")))
        (setq result (replace-regexp-in-string (car pair) (cdr pair) result t t)))
      result)))

(defun org-invox-export--default-template ()
  "Return the default LaTeX template as a string."
  "\\documentclass[11pt,letterpaper]{article}
\\usepackage[letterpaper,margin=1in]{geometry}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{tabularx}
\\usepackage{xcolor}
\\usepackage{fancyhdr}
\\usepackage{lastpage}
\\usepackage{parskip}
\\usepackage{microtype}
\\usepackage{hyperref}

% Colors
\\definecolor{primary}{HTML}{2C3E50}
\\definecolor{accent}{HTML}{3498DB}
\\definecolor{lightgray}{HTML}{ECF0F1}
\\definecolor{medgray}{HTML}{95A5A6}

% Header/Footer
\\pagestyle{fancy}
\\fancyhf{}
\\renewcommand{\\headrulewidth}{0pt}
\\fancyfoot[C]{\\small\\color{medgray}Page \\thepage\\ of \\pageref{LastPage}}

\\hypersetup{
  colorlinks=true,
  linkcolor=accent,
  urlcolor=accent
}

\\begin{document}

% Invoice Header
{\\color{primary}\\huge\\bfseries INVOICE}

\\vspace{0.5em}
\\noindent\\rule{\\textwidth}{2pt}

\\vspace{1em}

% Invoice metadata
\\begin{tabularx}{\\textwidth}{@{}lXr@{}}
\\textbf{Invoice Number:} & & \\textbf{<<INVOICE_NUMBER>>} \\\\
\\textbf{Invoice Date:} & & <<INVOICE_DATE>> \\\\
\\textbf{Due Date:} & & <<DUE_DATE>> \\\\
\\textbf{Invoice Period:} & & <<INVOICE_PERIOD>> \\\\
\\end{tabularx}

\\vspace{1.5em}

% From / To
\\noindent
\\begin{minipage}[t]{0.48\\textwidth}
\\textbf{\\color{primary}FROM}\\\\[0.5em]
<<FROM_BLOCK>>
\\end{minipage}
\\hfill
\\begin{minipage}[t]{0.48\\textwidth}
\\textbf{\\color{primary}BILL TO}\\\\[0.5em]
<<TO_BLOCK>>
\\end{minipage}

\\vspace{2em}

% Line Items
\\begin{tabularx}{\\textwidth}{@{}c X r r r@{}}
\\toprule
\\textbf{\\#} & \\textbf{Description} & \\textbf{Hours} & \\textbf{Rate (<<CURRENCY_SYM>>)} & \\textbf{Amount (<<CURRENCY_SYM>>)} \\\\
\\midrule
<<LINE_ITEMS>>
\\bottomrule
\\end{tabularx}

\\vspace{1em}

% Totals
\\begin{tabularx}{\\textwidth}{@{}Xr@{}}
& \\begin{tabular}{@{}lr@{}}
\\textbf{Subtotal:} & <<CURRENCY_SYM>><<SUBTOTAL>> \\\\
\\textbf{<<TAX_LABEL>> (<<TAX_RATE>>\\%):} & <<CURRENCY_SYM>><<TAX_AMOUNT>> \\\\
\\midrule
\\textbf{\\Large Total (<<CURRENCY>>):} & \\textbf{\\Large <<CURRENCY_SYM>><<TOTAL>>} \\\\
\\end{tabular} \\\\
\\end{tabularx}

\\vspace{2em}

% Payment Terms
\\noindent\\rule{\\textwidth}{0.5pt}

\\vspace{0.5em}

\\textbf{\\color{primary}Payment Terms:} <<PAYMENT_TERMS>>

\\vspace{2em}

{\\small\\color{medgray} Thank you for your business.}

\\end{document}
")

(defun org-invox-export--fill-template (template props)
  "Fill TEMPLATE with values from PROPS."
  (let ((result template)
        (esc #'org-invox-export--latex-escape))
    ;; Simple replacements
    (dolist (mapping '(("<<INVOICE_NUMBER>>" . :invoice_number)
                       ("<<INVOICE_DATE>>" . :invoice_date)
                       ("<<DUE_DATE>>" . :due_date)
                       ("<<INVOICE_PERIOD>>" . :invoice-period)
                       ("<<SUBTOTAL>>" . :subtotal)
                       ("<<TAX_LABEL>>" . :tax-label)
                       ("<<TAX_RATE>>" . :tax-rate)
                       ("<<TAX_AMOUNT>>" . :tax-amount)
                       ("<<TOTAL>>" . :total)
                       ("<<CURRENCY>>" . :currency)
                       ("<<PAYMENT_TERMS>>" . :payment-terms)))
      (let ((val (or (plist-get props (cdr mapping)) "")))
        (setq result (replace-regexp-in-string
                      (regexp-quote (car mapping))
                      (funcall esc val)
                      result t t))))
    ;; From block: the example block already contains the full formatted address
    ;; (name, company, address, email, phone) written by org-invox-create.
    ;; Use it directly to avoid duplicating fields that are also in properties.
    (let* ((addr (or (plist-get props :from-address) ""))
           (addr-lines (split-string addr "\n" t "[ \t]+"))
           (from-block
            (if addr-lines
                (mapconcat esc addr-lines "\\\\\\\\\n")
              ;; Fallback when no example block is present
              (mapconcat esc
                         (cl-remove-if #'string-empty-p
                                       (list (or (plist-get props :from-name) "")
                                             (or (plist-get props :from-company) "")
                                             (or (plist-get props :from-email) "")
                                             (or (plist-get props :from-phone) "")))
                         "\\\\\\\\\n"))))
      (setq result (replace-regexp-in-string
                    (regexp-quote "<<FROM_BLOCK>>")
                    from-block result t t)))
    ;; To block: same approach — use the example block directly.
    (let* ((to-addr (or (plist-get props :to-address) ""))
           (to-addr-lines (split-string to-addr "\n" t "[ \t]+"))
           (to-block
            (if to-addr-lines
                (mapconcat esc to-addr-lines "\\\\\\\\\n")
              ;; Fallback when no example block is present
              (mapconcat esc
                         (cl-remove-if #'string-empty-p
                                       (list (or (plist-get props :to-contact)
                                                 (plist-get props :to-name) "")
                                             (or (plist-get props :to-company) "")
                                             (or (plist-get props :to-email) "")))
                         "\\\\\\\\\n"))))
      (setq result (replace-regexp-in-string
                    (regexp-quote "<<TO_BLOCK>>")
                    to-block result t t)))
    ;; Currency symbol (don't escape the $)
    (let ((sym (or (plist-get props :currency-symbol)
                   org-invox-default-currency-symbol)))
      (setq result (replace-regexp-in-string
                    (regexp-quote "<<CURRENCY_SYM>>")
                    (if (string= sym "$") "\\$" (funcall esc sym))
                    result t t)))
    ;; Line items
    (let ((items (plist-get props :line-items))
          (lines ""))
      (dolist (item items)
        (setq lines (concat lines
                            (format "%s & %s & %s & %s & %s \\\\\n"
                                    (funcall esc (plist-get item :num))
                                    (funcall esc (plist-get item :description))
                                    (funcall esc (plist-get item :hours))
                                    (funcall esc (plist-get item :rate))
                                    (funcall esc (plist-get item :amount))))))
      (setq result (replace-regexp-in-string
                    (regexp-quote "<<LINE_ITEMS>>")
                    lines result t t)))
    result))

;;;###autoload
(defun org-invox-export-pdf ()
  "Export the current invoice buffer to PDF via LaTeX."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((props (org-invox-export--parse-invoice))
         (template (if (and org-invox-latex-template-file
                            (file-exists-p org-invox-latex-template-file))
                       (with-temp-buffer
                         (insert-file-contents org-invox-latex-template-file)
                         (buffer-string))
                     (org-invox-export--default-template)))
         (filled (org-invox-export--fill-template template props))
         (org-file (buffer-file-name))
         (base-name (file-name-sans-extension org-file))
         (tex-file (concat base-name ".tex"))
         (pdf-file (concat base-name ".pdf"))
         (default-directory (file-name-directory org-file)))
    ;; Write .tex file
    (with-temp-file tex-file
      (insert filled))
    ;; Compile with pdflatex (run twice for references)
    (let ((compile-cmd (format "pdflatex -interaction=nonstopmode -output-directory=%s %s"
                               (shell-quote-argument (file-name-directory tex-file))
                               (shell-quote-argument tex-file))))
      (message "Compiling invoice PDF...")
      (shell-command compile-cmd)
      (shell-command compile-cmd))  ; Second pass for lastpage
    ;; Clean up auxiliary files
    (dolist (ext '(".aux" ".log" ".out"))
      (let ((f (concat base-name ext)))
        (when (file-exists-p f)
          (delete-file f))))
    (if (file-exists-p pdf-file)
        (progn
          (message "Invoice PDF created: %s" pdf-file)
          (when (y-or-n-p "Open PDF? ")
            (org-open-file pdf-file)))
      (user-error "PDF compilation failed. Check that pdflatex is installed"))))

(provide 'org-invox-export)
;;; org-invox-export.el ends here
