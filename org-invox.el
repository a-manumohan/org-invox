;;; org-invox.el --- Invoice management for contractors using Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 Manu Mohan

;; Author: Manu Mohan <me@manumohan.net>
;; Maintainer: Manu Mohan <me@manumohan.net>
;; URL: https://github.com/a-manumohan/org-invox
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: outlines tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-invox provides a complete invoicing workflow for software
;; contractors and freelancers using Org mode.
;;
;; Features:
;; - Contract-based invoice management (one contract per client)
;; - Auto-incrementing invoice numbers with configurable format
;; - Configurable tax rates (HST, GST, VAT, etc.) per contract
;; - Configurable payment terms per contract
;; - PDF export via LaTeX with a professional template
;; - Plain Org records for bookkeeping
;; - Master index file per contract with links to all invoices
;;
;; Quick Start:
;;
;;   1. Configure your business details:
;;      (setq org-invox-from-name "Your Name"
;;            org-invox-from-company "Your Company"
;;            org-invox-from-address "Your Address"
;;            org-invox-from-email "you@example.com")
;;
;;   2. Create a new contract:
;;      M-x org-invox-new-contract
;;
;;   3. Create an invoice:
;;      M-x org-invox-create
;;
;;   4. Export to PDF:
;;      M-x org-invox-export-pdf
;;
;;   5. Mark as paid:
;;      M-x org-invox-mark-paid

;;; Code:

(require 'org)
(require 'cl-lib)

;;; Customization Group

(defgroup org-invox nil
  "Invoice management for contractors using Org mode."
  :group 'org
  :prefix "org-invox-")

;;; Business Details (From)

(defcustom org-invox-from-name ""
  "Your full name for invoices."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-from-company ""
  "Your company/business name for invoices.
Leave empty if operating under personal name."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-from-address ""
  "Your business address for invoices.
Use \\n for line breaks."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-from-email ""
  "Your business email for invoices."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-from-phone ""
  "Your business phone for invoices."
  :type 'string
  :group 'org-invox)

;;; Invoice Defaults

(defcustom org-invox-root-directory "~/invoices"
  "Root directory for all invoice contracts and files."
  :type 'directory
  :group 'org-invox)

(defcustom org-invox-default-tax-rate 13.0
  "Default tax rate as a percentage (e.g., 13.0 for 13% HST)."
  :type 'float
  :group 'org-invox)

(defcustom org-invox-default-tax-label "HST"
  "Default label for the tax (e.g., HST, GST, VAT, Sales Tax)."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-default-payment-terms "Net 30"
  "Default payment terms (e.g., Net 15, Net 30, Due on Receipt)."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-default-currency "CAD"
  "Default currency code for invoices."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-default-currency-symbol "$"
  "Default currency symbol for display."
  :type 'string
  :group 'org-invox)

(defcustom org-invox-number-format "INV-%Y-%04d"
  "Format string for invoice numbers.
%Y is replaced with the 4-digit year.
%04d is replaced with the zero-padded sequence number.
Examples:
  \"INV-%Y-%04d\"  => INV-2026-0001
  \"INV-%04d\"     => INV-0001
  \"%Y%02d\"       => 202601"
  :type 'string
  :group 'org-invox)

(defcustom org-invox-latex-template-file nil
  "Path to a custom LaTeX template file.
If nil, the built-in template is used."
  :type '(choice (const :tag "Built-in template" nil)
                 (file :tag "Custom template file"))
  :group 'org-invox)

(defcustom org-invox-date-format "%B %d, %Y"
  "Format string for dates on invoices (e.g., \"March 15, 2026\")."
  :type 'string
  :group 'org-invox)

;;; Internal Functions

(defun org-invox--ensure-root ()
  "Ensure the invoice root directory exists."
  (let ((dir (expand-file-name org-invox-root-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun org-invox--contract-dir (slug)
  "Return the contract directory for SLUG."
  (expand-file-name slug (org-invox--ensure-root)))

(defun org-invox--index-file (slug)
  "Return the index.org path for contract SLUG."
  (expand-file-name "index.org" (org-invox--contract-dir slug)))

(defun org-invox--slugify (name)
  "Convert NAME into a filesystem-safe slug."
  (let ((slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" name))))
    (replace-regexp-in-string "^-\\|-$" "" slug)))

(defun org-invox--read-index-property (index-file property)
  "Read a PROPERTY from the top-level heading in INDEX-FILE."
  (with-temp-buffer
    (insert-file-contents index-file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:" (upcase property)) nil t)
      (string-trim (buffer-substring-no-properties (point) (line-end-position))))))

(defun org-invox--read-contract-property (index-file property)
  "Read a PROPERTY from the Contract Details heading in INDEX-FILE."
  (with-temp-buffer
    (insert-file-contents index-file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward "^\\* Contract Details" nil t)
      (org-entry-get (point) property))))

(defun org-invox--format-address-block (name company address email phone)
  "Format NAME, COMPANY, ADDRESS, EMAIL, and PHONE into a readable address block."
  (string-join
   (cl-remove-if #'string-empty-p
                 (list (or name "")
                       (if (and company (not (string-empty-p company))
                                (not (string= company name)))
                           company "")
                       (or address "")
                       (if (and email (not (string-empty-p email)))
                           (concat "Email: " email) "")
                       (if (and phone (not (string-empty-p phone)))
                           (concat "Phone: " phone) "")))
   "\n"))

(defun org-invox--next-invoice-number (index-file)
  "Get the next invoice number from INDEX-FILE and increment it."
  (let* ((next-num (string-to-number
                    (or (org-invox--read-index-property index-file "NEXT_INVOICE_NUM")
                        "1")))
         (year (format-time-string "%Y"))
         (fmt org-invox-number-format)
         (invoice-number (replace-regexp-in-string
                          "%Y" year
                          (replace-regexp-in-string
                           "%0?[0-9]*d"
                           (lambda (match)
                             (format match next-num))
                           fmt))))
    ;; Increment the counter in the index file
    (with-current-buffer (find-file-noselect index-file)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+NEXT_INVOICE_NUM:.*$" nil t)
          (replace-match (format "#+NEXT_INVOICE_NUM: %d" (1+ next-num)))
        (goto-char (point-max))
        (insert (format "\n#+NEXT_INVOICE_NUM: %d" (1+ next-num))))
      (save-buffer))
    invoice-number))

(defun org-invox--list-contracts ()
  "Return a list of (name . slug) for all contracts."
  (let ((root (org-invox--ensure-root))
        contracts)
    (dolist (dir (directory-files root t "^[^.]"))
      (when (and (file-directory-p dir)
                 (file-exists-p (expand-file-name "index.org" dir)))
        (let* ((index (expand-file-name "index.org" dir))
               (name (or (org-invox--read-index-property index "TITLE")
                         (file-name-nondirectory dir))))
          (push (cons name (file-name-nondirectory dir)) contracts))))
    (nreverse contracts)))

(defun org-invox--select-contract ()
  "Prompt user to select a contract.  Return (name . slug)."
  (let ((contracts (org-invox--list-contracts)))
    (unless contracts
      (user-error "No contracts found. Create one with `org-invox-new-contract'"))
    (let* ((names (mapcar #'car contracts))
           (choice (completing-read "Select contract: " names nil t)))
      (assoc choice contracts))))

(defun org-invox--compute-totals (hours rate tax-rate)
  "Compute subtotal, tax, and total from HOURS, RATE, and TAX-RATE."
  (let* ((subtotal (* hours rate))
         (tax (* subtotal (/ tax-rate 100.0)))
         (total (+ subtotal tax)))
    (list :subtotal subtotal :tax tax :total total)))

(defun org-invox--format-date (time)
  "Format TIME using `org-invox-date-format'."
  (format-time-string org-invox-date-format time))

(defun org-invox--add-to-index (index-file invoice-number invoice-file
                                           period-start period-end total status)
  "Add an invoice entry for INVOICE-NUMBER to INDEX-FILE.
INVOICE-FILE is the path to the invoice org file.
PERIOD-START and PERIOD-END are the billing period dates.
TOTAL is the invoice total amount.
STATUS is the initial invoice status string."
  (with-current-buffer (find-file-noselect index-file)
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Invoices" nil t)
      (goto-char (point-max))
      (insert "\n* Invoices\n"))
    ;; Go to end of Invoices section
    (org-end-of-subtree t)
    (insert (format "\n** %s
:PROPERTIES:
:INVOICE_FILE: [[file:%s][%s]]
:PERIOD_START: %s
:PERIOD_END: %s
:TOTAL: %.2f
:STATUS: %s
:DATE_CREATED: %s
:END:\n"
                    invoice-number
                    (file-name-nondirectory invoice-file)
                    invoice-number
                    period-start
                    period-end
                    total
                    status
                    (format-time-string "[%F %a]")))
    (save-buffer)))

(defun org-invox--collect-index-invoices (index-file)
  "Return invoice plists from the Invoices section of INDEX-FILE.
Each plist has :total (float), :status, and :date-created."
  (with-temp-buffer
    (insert-file-contents index-file)
    (org-mode)
    (let (invoices)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Invoices" nil t)
        (let ((bound (save-excursion
                       (if (re-search-forward "^\\* " nil t)
                           (line-beginning-position)
                         (point-max)))))
          (while (re-search-forward "^\\*\\* " bound t)
            (let ((total (org-entry-get (point) "TOTAL"))
                  (status (org-entry-get (point) "STATUS"))
                  (date-created (org-entry-get (point) "DATE_CREATED")))
              (when (and total status)
                (push (list :total (string-to-number total)
                            :status status
                            :date-created (or date-created ""))
                      invoices))))))
      (nreverse invoices))))

(defun org-invox--update-index-summary (index-file)
  "Recompute and write the Financial Summary section in INDEX-FILE."
  (let* ((invoices (org-invox--collect-index-invoices index-file))
         (current-year (format-time-string "%Y"))
         (sym (or (org-invox--read-contract-property index-file "CURRENCY_SYMBOL")
                  org-invox-default-currency-symbol))
         (fmt (lambda (n) (format "%s%.2f" sym n)))
         (sum (lambda (lst) (apply #'+ (mapcar (lambda (i) (plist-get i :total)) lst))))
         (paid   (cl-remove-if-not (lambda (i) (string= (plist-get i :status) "Paid")) invoices))
         (unpaid (cl-remove-if     (lambda (i) (string= (plist-get i :status) "Paid")) invoices))
         (ytd    (cl-remove-if-not
                  (lambda (i)
                    (string-match-p (concat "\\[" current-year)
                                    (plist-get i :date-created)))
                  invoices))
         (ytd-paid   (cl-remove-if-not (lambda (i) (string= (plist-get i :status) "Paid")) ytd))
         (ytd-unpaid (cl-remove-if     (lambda (i) (string= (plist-get i :status) "Paid")) ytd))
         (summary
          (format "* Financial Summary
| Metric          | Amount                        |
|-----------------+-------------------------------|
| YTD Invoiced    | %s |
| YTD Paid        | %s |
| YTD Outstanding | %s |
|-----------------+-------------------------------|
| Total Invoiced  | %s |
| Total Paid      | %s |
| Outstanding     | %s |
|-----------------+-------------------------------|
| Invoices        | %d total (%d paid, %d unpaid) |
"
                  (funcall fmt (funcall sum ytd))
                  (funcall fmt (funcall sum ytd-paid))
                  (funcall fmt (funcall sum ytd-unpaid))
                  (funcall fmt (funcall sum invoices))
                  (funcall fmt (funcall sum paid))
                  (funcall fmt (funcall sum unpaid))
                  (length invoices) (length paid) (length unpaid))))
    (with-current-buffer (find-file-noselect index-file)
      (goto-char (point-min))
      (if (re-search-forward "^\\* Financial Summary" nil t)
          (let ((start (progn (beginning-of-line) (point))))
            (forward-line 1)
            (let ((end (if (re-search-forward "^\\* " nil t)
                           (line-beginning-position)
                         (point-max))))
              (delete-region start end)
              (insert summary)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert summary))
      (save-buffer))))

;;; Interactive Commands

;;;###autoload
(defun org-invox-new-contract ()
  "Create a new contract with client details."
  (interactive)
  (let* ((client-name (read-string "Client/Company name: "))
         (client-address (read-string "Client address: "))
         (client-email (read-string "Client email: "))
         (contact-name (read-string "Contact person (or RET to skip): "))
         (service-desc (read-string "Service description (e.g., Software Development Services): "
                                    "Software Development Services"))
         (rate (read-number "Hourly rate: "))
         (currency (read-string "Currency code: " org-invox-default-currency))
         (currency-sym (read-string "Currency symbol: " org-invox-default-currency-symbol))
         (tax-rate (read-number "Tax rate (%): " org-invox-default-tax-rate))
         (tax-label (read-string "Tax label: " org-invox-default-tax-label))
         (payment-terms (read-string "Payment terms: " org-invox-default-payment-terms))
         (slug (org-invox--slugify client-name))
         (contract-dir (org-invox--contract-dir slug))
         (index-file (org-invox--index-file slug)))
    (when (file-exists-p index-file)
      (user-error "Contract '%s' already exists at %s" client-name contract-dir))
    (make-directory contract-dir t)
    (with-temp-file index-file
      (insert (format "#+TITLE: %s
#+NEXT_INVOICE_NUM: 1

* Contract Details
:PROPERTIES:
:CLIENT_NAME: %s
:CLIENT_ADDRESS: %s
:CLIENT_EMAIL: %s
:CONTACT_NAME: %s
:SERVICE_DESCRIPTION: %s
:RATE: %.2f
:CURRENCY: %s
:CURRENCY_SYMBOL: %s
:TAX_RATE: %.2f
:TAX_LABEL: %s
:PAYMENT_TERMS: %s
:END:

* Invoices
"
                      client-name
                      client-name
                      client-address
                      client-email
                      (if (string-empty-p contact-name) client-name contact-name)
                      service-desc
                      rate
                      currency
                      currency-sym
                      tax-rate
                      tax-label
                      payment-terms)))
    (find-file index-file)
    (message "Contract created for '%s' at %s" client-name contract-dir)))

;;;###autoload
(defun org-invox-create ()
  "Create a new invoice for a contract."
  (interactive)
  (let* ((contract (org-invox--select-contract))
         (slug (cdr contract))
         (index-file (org-invox--index-file slug))
         ;; Read contract properties
         (client-name (org-invox--read-contract-property index-file "CLIENT_NAME"))
         (client-address (org-invox--read-contract-property index-file "CLIENT_ADDRESS"))
         (client-email (org-invox--read-contract-property index-file "CLIENT_EMAIL"))
         (contact-name (org-invox--read-contract-property index-file "CONTACT_NAME"))
         (service-desc (org-invox--read-contract-property index-file "SERVICE_DESCRIPTION"))
         (rate (string-to-number (org-invox--read-contract-property index-file "RATE")))
         (currency (or (org-invox--read-contract-property index-file "CURRENCY")
                       org-invox-default-currency))
         (currency-sym (or (org-invox--read-contract-property index-file "CURRENCY_SYMBOL")
                           org-invox-default-currency-symbol))
         (tax-rate (string-to-number
                    (or (org-invox--read-contract-property index-file "TAX_RATE")
                        (number-to-string org-invox-default-tax-rate))))
         (tax-label (or (org-invox--read-contract-property index-file "TAX_LABEL")
                        org-invox-default-tax-label))
         (payment-terms (or (org-invox--read-contract-property index-file "PAYMENT_TERMS")
                            org-invox-default-payment-terms))
         ;; Prompt for variable fields
         (hours (read-number "Hours worked: "))
         (period-start (org-read-date nil nil nil "Period start: "))
         (period-end (org-read-date nil nil nil "Period end: "))
         ;; Compute
         (totals (org-invox--compute-totals hours rate tax-rate))
         (subtotal (plist-get totals :subtotal))
         (tax (plist-get totals :tax))
         (total (plist-get totals :total))
         ;; Invoice number and file
         (invoice-number (org-invox--next-invoice-number index-file))
         (invoice-file (expand-file-name
                        (concat invoice-number ".org")
                        (org-invox--contract-dir slug)))
         (invoice-date (org-invox--format-date (current-time)))
         (due-date (org-invox--format-date
                    (time-add (current-time)
                              (days-to-time
                               (cond
                                ((string-match "Net \\([0-9]+\\)" payment-terms)
                                 (string-to-number (match-string 1 payment-terms)))
                                ((string-match-p "Due on Receipt" payment-terms) 0)
                                (t 30)))))))
    ;; Create the invoice org file
    (with-temp-file invoice-file
      (insert (format "#+TITLE: Invoice %s
#+INVOICE_NUMBER: %s
#+INVOICE_DATE: %s
#+DUE_DATE: %s
#+STATUS: Unpaid

* Invoice Details
:PROPERTIES:
:INVOICE_NUMBER: %s
:INVOICE_DATE: %s
:DUE_DATE: %s
:PERIOD_START: %s
:PERIOD_END: %s
:PAYMENT_TERMS: %s
:STATUS: Unpaid
:END:

** From
:PROPERTIES:
:FROM_NAME: %s
:FROM_COMPANY: %s
:FROM_EMAIL: %s
:FROM_PHONE: %s
:END:
#+begin_example
%s
#+end_example

** To
:PROPERTIES:
:TO_NAME: %s
:TO_COMPANY: %s
:TO_EMAIL: %s
:TO_CONTACT: %s
:END:
#+begin_example
%s
#+end_example

** Line Items
| # | Description | Hours | Rate (%s) | Amount (%s) |
|---+-------------+-------+-----------+-------------|
| 1 | %s | %.2f | %.2f | %.2f |
|---+-------------+-------+-----------+-------------|
|   | *Subtotal*  |       |           | *%.2f* |
|   | *%s (%s%%)*  |       |           | *%.2f* |
|   | *TOTAL (%s)* |       |           | *%.2f* |

** Invoice Period
%s to %s

** Payment Terms
%s
"
                      invoice-number
                      invoice-number
                      invoice-date
                      due-date
                      ;; Properties
                      invoice-number
                      invoice-date
                      due-date
                      period-start
                      period-end
                      payment-terms
                      ;; From properties
                      org-invox-from-name
                      org-invox-from-company
                      org-invox-from-email
                      org-invox-from-phone
                      ;; From address block
                      (org-invox--format-address-block
                       org-invox-from-name
                       org-invox-from-company
                       org-invox-from-address
                       org-invox-from-email
                       org-invox-from-phone)
                      ;; To properties
                      contact-name
                      client-name
                      client-email
                      contact-name
                      ;; To address block
                      (org-invox--format-address-block
                       contact-name
                       client-name
                       client-address
                       client-email
                       nil)
                      ;; Line items
                      currency-sym currency-sym
                      service-desc hours rate subtotal
                      subtotal
                      tax-label (format "%.1f" tax-rate) tax
                      currency total
                      ;; Period
                      period-start period-end
                      ;; Terms
                      payment-terms)))
    ;; Update the index
    (org-invox--add-to-index index-file invoice-number invoice-file
                             period-start period-end total "Unpaid")
    (org-invox--update-index-summary index-file)
    ;; Open the invoice
    (find-file invoice-file)
    (message "Invoice %s created (Total: %s%.2f)" invoice-number currency-sym total)))

;;;###autoload
(defun org-invox-mark-paid ()
  "Mark the current invoice buffer as paid."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    ;; Update status in file keyword
    (goto-char (point-min))
    (when (re-search-forward "^#\\+STATUS:.*$" nil t)
      (replace-match "#+STATUS: Paid"))
    ;; Update status in properties
    (goto-char (point-min))
    (when (re-search-forward "^\\* Invoice Details" nil t)
      (org-entry-put (point) "STATUS" "Paid")
      (org-entry-put (point) "PAID_DATE" (format-time-string "[%F %a]")))
    (save-buffer))
  ;; Also update the index file if we can find it
  (let* ((invoice-file (buffer-file-name))
         (invoice-dir (file-name-directory invoice-file))
         (index-file (expand-file-name "index.org" invoice-dir))
         (invoice-number (save-excursion
                           (goto-char (point-min))
                           (when (re-search-forward "^#\\+INVOICE_NUMBER: \\(.+\\)$" nil t)
                             (match-string 1)))))
    (when (and (file-exists-p index-file) invoice-number)
      (with-current-buffer (find-file-noselect index-file)
        (goto-char (point-min))
        (when (re-search-forward (format "^\\*\\* %s" (regexp-quote invoice-number)) nil t)
          (org-entry-put (point) "STATUS" "Paid")
          (org-entry-put (point) "PAID_DATE" (format-time-string "[%F %a]")))
        (save-buffer))))
  (when (file-exists-p index-file)
    (org-invox--update-index-summary index-file))
  (message "Invoice marked as paid."))

;;;###autoload
(defun org-invox-open-contract ()
  "Open the index file for a contract."
  (interactive)
  (let* ((contract (org-invox--select-contract))
         (slug (cdr contract))
         (index-file (org-invox--index-file slug)))
    (find-file index-file)))

;;;###autoload
(defun org-invox-list-unpaid ()
  "List all unpaid invoices across all contracts."
  (interactive)
  (let ((contracts (org-invox--list-contracts))
        (unpaid-list '()))
    (dolist (contract contracts)
      (let* ((slug (cdr contract))
             (contract-dir (org-invox--contract-dir slug)))
        (dolist (file (directory-files contract-dir t "\\.org$"))
          (unless (string= (file-name-nondirectory file) "index.org")
            (with-temp-buffer
              (insert-file-contents file)
              (when (re-search-forward "^#\\+STATUS: Unpaid" nil t)
                (goto-char (point-min))
                (let ((inv-num (when (re-search-forward "^#\\+INVOICE_NUMBER: \\(.+\\)$" nil t)
                                 (match-string 1)))
                      (inv-due (progn
                                 (goto-char (point-min))
                                 (when (re-search-forward "^#\\+DUE_DATE: \\(.+\\)$" nil t)
                                   (match-string 1)))))
                  (push (list (car contract) inv-num inv-due file) unpaid-list))))))))
    (if (null unpaid-list)
        (message "No unpaid invoices found.")
      (let ((buf (get-buffer-create "*Org Invoice - Unpaid*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "Unpaid Invoices\n")
          (insert (make-string 60 ?=) "\n\n")
          (dolist (inv unpaid-list)
            (insert (format "%-25s  %-15s  Due: %s\n  %s\n\n"
                            (nth 0 inv) (nth 1 inv) (nth 2 inv) (nth 3 inv))))
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buf)))))

;;;###autoload
(defun org-invox-update-summary ()
  "Refresh the Financial Summary section in a contract's index file."
  (interactive)
  (let* ((contract (org-invox--select-contract))
         (index-file (org-invox--index-file (cdr contract))))
    (org-invox--update-index-summary index-file)
    (message "Financial summary updated.")))

(provide 'org-invox)
;;; org-invox.el ends here
