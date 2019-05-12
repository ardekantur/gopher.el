;;; gopher.el --- easily access and navigate Gopher servers

;; Copyright (C) 2011 Matthew Snyder

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Matthew Snyder <matthew.c.snyder@gmail.com>
;;         and the gopher.el authors (see AUTHORS.org)
;; URL: http://github.com/ardekantur/gopher.el
;; Package-Version: 20190211.1742
;; Package-Requires: ((emacs "24.4") (w3m "0"))
;; Version: 0.0.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; gopher.el allows you to navigate Gopher servers.

;; "M-x gopher" prompts you for an address.  <TAB> and <M-TAB> navigate
;; between links on a directory listing, while <[> and <]> navigate
;; between text documents.  <RET> opens the link at the cursor's
;; position.  You can navigate up through the directory tree with <u>.
;;
;; There is primitive history support.  <B> navigates backwards
;; and <F> forwards through the history.

(require 'cl-lib)
(require 'shr)
(require 'w3m nil t); optional

;;; Code:

(defconst gopher-available-content-types
  '(("0" . plain-text)
    ("1" . directory-listing)
    ("i" . informational-message)
    ("g" . gif)
    ("h" . html)
    ("I" . generic-image)
    ("7" . search-query)
    ("w" . write)))

(defconst gopher-coding
  '((gif binary binary)
    (generic-image binary binary)
    (t utf-8 utf-8))
  "Encoding to use for the process based on content-type.
This is an alist with elements of the form
\(CONTENT-TYPE DECODING ENCODING) where CONTENT-TYPE is one of
the values of `gopher-available-content-types' and DECODING and ENCODING
are coding systems suitable for `set-process-coding-system'.
The CONTENT-TYPE t is the default when no match is found.")

(defconst gopher-faces
  '((directory-listing . font-lock-builtin-face)
    (informational-message . font-lock-comment-face)
    (gif . font-lock-variable-name-face)
    (generic-image . font-lock-string-face)
    (html . font-lock-type-name-face)
    (write . font-lock-warning-face)))

(defvar gopher-buffer-name "*gopher*")
(defvar gopher-back-buffer-name "*gopher-back-buffer*")

;; silence compiler warnings
(defvar gopher-current-address)
(defvar gopher-current-data)
(defvar bookmark-make-record-function)
(declare-function bookmark-prop-get "bookmark" (bookmark-name-or-record prop))
(declare-function w3m-anchor (&optional position))

(defgroup gopher nil
  "Gopher server navigation"
  :group 'hypermedia)

(defvar gopher-history-ring nil
  "List of URLs visited in gopher.")

(defcustom gopher-history-ring-max 60
  "Maximum length of gopher history ring before oldest elements are thrown away."
  :type 'integer
  :group 'gopher)

(defun gopher-get-matching (function content-type)
  "Return a FUNCTION specific to handling CONTENT-TYPE.

Function is either 'sentinel' or 'filter'."
  (let ((name (intern (concat "gopher-" function "-" (symbol-name content-type)))))
    (if (fboundp name)
        name
      (intern (concat "gopher-" function)))))

(defun gopher-goto-address (address)
  "Go to the gopher.el ADDRESS."
  (gopher-goto-url (nth 0 address)
                   (nth 1 address)
                   (nth 2 address)
                   nil nil t))

(defun gopher-refresh-current-address ()
  "Refresh the current gopher URL."
  (interactive)
  (gopher-goto-address gopher-current-address))

(defun gopher-get-content-type (line-data)
  "Return the content type of the LINE-DATA."
  (let ((content-type (assoc (cl-getf line-data :item-type) gopher-available-content-types)))
    (if content-type
        (cdr content-type)
      nil)))

(defun gopher-get-face (content-type)
  "Return the face associated with CONTENT-TYPE."
  (let ((face (assoc content-type gopher-faces)))
    (if face
        (cdr face)
      nil)))

(defun gopher (address)
  "Launch the Gopher browser, asking the user for ADDRESS to navigate to."
  (interactive "MGopher URL: ")
  (let* ((split-address (split-string (replace-regexp-in-string "^gopher:\/\/" "" address) "/"))
         (split-url (split-string (car split-address) ":"))
	 (hostname (car split-url))
	 (port (nth 1 split-url))
         (selector (mapconcat 'identity (cdr split-address) "/")))
    (if (< (length selector) 1)
        (gopher-goto-url hostname port nil)
      (gopher-goto-url hostname port selector))))

(define-minor-mode gopher-tls-mode
  "Toggle TLS for Gopher."
  nil " TLS" :global t :require 'gopher)

(defun gopher-goto-url (&optional hostname port selector content-type
                                  search-argument no-history)
  "Go to the URL described by the arguments.
HOSTNAME, PORT, SELECTOR, and CONTENT-TYPE all represent their expected URI
parts.  SEARCH-ARGUMENT is passed to the gopher URL.  If NO-HISTORY
is true, this URL is not added to the history stack."
  (interactive)
  (if (get-buffer gopher-back-buffer-name)
      (kill-buffer gopher-back-buffer-name))
  (let ((content-type (or content-type 'directory-listing))
        (port (or port "70")))
   (unless no-history (gopher-history-new hostname port selector gopher-tls-mode))
   (condition-case nil
       (progn
         (let* ((args (append (list
			       "gopher"
			       (get-buffer-create gopher-back-buffer-name)
			       hostname
			       (string-to-number port)
			       :type (if gopher-tls-mode 'tls 'plain))))
	        (process (apply 'open-network-stream args)))
           (set-process-sentinel process (gopher-get-matching "sentinel" content-type))
           (set-process-filter process (gopher-get-matching "filter" content-type))
           (apply 'set-process-coding-system process
	          (cdr (or (assoc content-type gopher-coding)
		           (assoc t gopher-coding))))
           (process-send-string process (gopher-prepare-request selector search-argument)))
         (gopher-prepare-buffer hostname port selector)
         (gopher-back-buffer-swap))
     (error
      (message "failed to connect")
      (kill-buffer gopher-back-buffer-name)))))

(defun gopher-prepare-request (selector search-argument)
  "Construct a well-formed Gopher request from SELECTOR and SEARCH-ARGUMENT, if available."
  (cond
   ((and selector search-argument) (format "%s\t%s\r\n" selector search-argument))
   (selector (format "%s\r\n" selector))
   (t "\r\n")))

(defun gopher-prepare-buffer (hostname port selector)
  "Prepare the back-buffer for rendering a Gopher response.
The metadata of the request, specifically HOSTNAME, PORT, and
SELECTOR, are set buffer-locally."
  (with-current-buffer gopher-back-buffer-name
    (gopher-mode)
    (setq gopher-current-address (list hostname port selector)
          gopher-current-data nil
          line-spacing 3)
    (insert "\n\n")))

(defun gopher-back-buffer-swap ()
  "Swap the back buffer to become the main buffer."
  (if (get-buffer gopher-buffer-name)
      (kill-buffer gopher-buffer-name))
  (with-current-buffer gopher-back-buffer-name
    (rename-buffer gopher-buffer-name))
  (set-window-buffer (selected-window) gopher-buffer-name))

(defun gopher-format-address (address)
  "Return a properly formatted Gopher address.
ADDRESS is a list of (HOSTNAME PORT SELECTOR)."
  (let ((hostname (nth 0 address))
        (port (nth 1 address))
        (selector (nth 2 address)))
    (cond
     ((and selector
           (not (zerop (length selector)))
           (string= "/" (substring selector 0 1)))
      (format "%s:%s%s" hostname port selector))
     (selector
      (format "%s:%s/%s" hostname port selector))
     (t
      (format "%s:%s" hostname port)))))

(defun gopher-process-line (line)
  "Split the response LINE into its components.
Return a list of these components keyed by name."
  (let* ((lineparts (split-string line "\t"))
         (item-type (substring (nth 0 lineparts) 0 1))
         (display-string (substring (nth 0 lineparts) 1))
         (selector (nth 1 lineparts))
         (hostname (nth 2 lineparts))
         (port (nth 3 lineparts)))
    (list :item-type item-type
          :display-string display-string
          :selector selector
          :hostname hostname
          :port port)))

(defun gopher-filter (proc string)
  "Process filter for the Gopher response data STRING.
Insert straight into the buffer.  PROC is the parent process."
  (with-current-buffer gopher-buffer-name
    (insert string)))

(defun gopher-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun gopher-filter-gif (proc string)
  "Process filter for GIF data from STRING.
Continue concatenating.  PROC is the parent process."
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-generic-image (proc string)
  "Process filter for generic image data from STRING.
Continue concatenating.  PROC is the parent process."
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-filter-directory-listing (proc string)
  "Process filter for a directory listing from STRING.
Continue concatenating.  PROC is the parent process."
  (with-current-buffer gopher-buffer-name
    (setq gopher-current-data (concat gopher-current-data string))))

(defun gopher-display-line (line)
  "Render the given LINE in the browser buffer."
  (if (or
       (zerop (length line))
       (string-match "^\.$" line))
      ""
    (let* ((line-data (gopher-process-line line))
           (indent (apply 'propertize "     " line-data)))
      (concat indent (gopher-format-line line-data) "\n"))))

(defun gopher-format-line (line-data)
  "Format LINE-DATA properly."
  (let ((content-type (gopher-get-content-type line-data)))
    (if (and content-type (gopher-get-face content-type))
        (propertize (cl-getf line-data :display-string)
                    'face (gopher-get-face content-type))
      (cl-getf line-data :display-string))))

(defun gopher-sentinel (proc msg)
  "Process sentinel for the network connection to the server.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-directory-listing (proc msg)
  "Process sentinel for directory listings.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let* ((lines (split-string gopher-current-data "\n")))
        (mapc (lambda (line) (insert (gopher-display-line line))) lines))
      (gopher-finish-buffer))))

(defalias 'gopher-sentinel-search-query 'gopher-sentinel-directory-listing)
(defalias 'gopher-filter-search-query 'gopher-filter-directory-listing)

(defun gopher-sentinel-plain-text (proc msg)
  "Process sentinel for plain text.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (gopher-finish-buffer))))

(defun gopher-sentinel-html (proc msg)
  "Process sentinel for HTML content.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (shr-render-region (point-min) (point-max) (current-buffer))
      (gopher-finish-buffer))))

(defun gopher-sentinel-gif (proc msg)
  "Process sentinel for GIF data.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (insert "     ")
      (insert-image (create-image gopher-current-data 'gif 'data))
      (gopher-finish-buffer))))

(defun gopher-sentinel-generic-image (proc msg)
  "Process sentinel for generic image data.
MSG is the status returned by the process, PROC."
  (when (string= msg "connection broken by remote peer\n")
    (with-current-buffer gopher-buffer-name
      (let ((image-type (image-type-from-data gopher-current-data)))
        (if image-type
            (progn
              (insert "     ")
              (insert-image (create-image
                             gopher-current-data image-type 'data))
              (gopher-finish-buffer))
          (error "Could not determine image type for %s"
		 (gopher-format-address gopher-current-address)))))))

(defun gopher-finish-buffer ()
  "Finalize the buffer for the user to interact with."
  (setq buffer-read-only t)
  (goto-char (point-min))
  (gopher-remove-dos-eol)
  (message "Loaded %s."
	   (gopher-format-address gopher-current-address)))

(defun gopher-goto-url-at-point (&optional arg)
  "Navigate to the URL at point, or the URL contained in ARG, if present."
  (interactive)
  (move-beginning-of-line nil)
  (let* ((properties (text-properties-at (point)))
         (content-type (gopher-get-content-type properties)))
    (cond ((eq content-type 'html)
           (browse-url (replace-regexp-in-string "^/?URL:" "" (cl-getf properties :selector))))
          ((eq content-type 'search-query)
           (call-interactively 'gopher-goto-search))
          (t (gopher-goto-url (cl-getf properties :hostname)
                              (cl-getf properties :port)
                              (cl-getf properties :selector)
                              content-type)))))

(defun gopher-goto-parent (&optional arg)
  "Navigate to the parent of the current address, or ARG, if present."
  (interactive)
  (let* ((address gopher-current-address)
         (hostname (nth 0 address))
         (port (nth 1 address))
         (selector (nth 2 address)))
    (gopher-goto-url hostname port (gopher-selector-parent selector))))

(defun gopher-goto-search (search-argument)
  "Make a request to the server with the string SEARCH-ARGUMENT."
  (interactive "MSearch argument: ")
  (let* ((properties (text-properties-at (point)))
         (content-type (gopher-get-content-type properties)))
    (gopher-goto-url (cl-getf properties :hostname)
		     (cl-getf properties :port)
                     (cl-getf properties :selector)
                     content-type search-argument)))

(defun gopher-bookmark-handler (record)
  "Go to a gopher bookmark RECORD."
  (gopher-goto-address (bookmark-prop-get record 'address))
  (switch-to-buffer gopher-buffer-name))

(defun gopher-bookmark-make-record ()
  "Return a bookmark record for the current gopher page."
  `((address . ,gopher-current-address)
    (location . ,(gopher-format-address gopher-current-address))
    (handler . gopher-bookmark-handler)))

(define-derived-mode gopher-mode fundamental-mode "Gopher"
  (set (make-local-variable 'gopher-current-data) nil)
  (set (make-local-variable 'gopher-current-address) nil)
  (set (make-local-variable 'bookmark-make-record-function) #'gopher-bookmark-make-record))

(defvar gopher-current-data nil)
(defvar gopher-current-address nil)

(defun gopher-pop-last (list)
  "Pop the last item from LIST."
  (cl-remove-if (lambda (x) t) list :count 1 :from-end t))

(defun gopher-selector-parent (selector)
  "Return the parent selector of SELECTOR.
Unreliable, e.g. foo.com:70/1/bar becomes foo.com:70/1/, which is useless."
  (mapconcat 'identity (gopher-pop-last (split-string selector "/")) "/"))

(defun gopher-open-w3m-url ()
  "Open this URL in Gopher."
  (interactive)
  (unless (functionp 'w3m-anchor)
    (error "You need to install the w3m package for this"))
  (gopher (w3m-anchor)))

(defmacro gopher-navigate (content-type &optional reverse)
  "Move the cursor to the next line with the CONTENT-TYPE.
With optional argument REVERSE, move the cursor to the previous line instead."
  `(defun ,(intern (concat "gopher-" (symbol-name (if reverse 'previous 'next)) "-" (symbol-name content-type))) ()
     (interactive)
     (forward-line ,(if reverse -1 1))
     (move-beginning-of-line nil)
     (while (and (not (= (line-number-at-pos)
                         (line-number-at-pos ,(if reverse
                                                  `(point-min)
                                                `(point-max)))))
                 (not (eq ',content-type (gopher-get-content-type (text-properties-at (point))))))
       (forward-line ,(if reverse -1 1)))))

(defun gopher-history-current-item (n &optional do-not-move)
  "Rotate the gopher history by N places, and then return that item.
If N is zero, does nothing.

If optional argument DO-NOT-MOVE is non-nil, don't actually
move the remembered point in history, just navigate to that
location."
  (or gopher-history-ring (error "History list is empty"))
  (let ((Nth-history-element
         (nthcdr n gopher-history-ring)))
    (unless do-not-move
      (setq gopher-history-ring Nth-history-element))
    (car Nth-history-element)))

(defun gopher-history-new (hostname port selector type &optional replace)
  "Make (list HOSTNAME PORT SELECTOR TYPE) the latest item in gopher's history.
Set `gopher-history-ring-pointer' to point to it.  Optional
argument REPLACE non-nil means that this item will replace the
front of the history ring, rather than being added to the list."
  (let ((address (car gopher-history-ring)))
    (let ((replace (or replace
                       (and (equal hostname (nth 0 address))
	                    (equal port (nth 1 address))
	                    (equal selector (nth 2 address)))))
          (entry (list hostname port selector type)))
      (if (and replace gopher-history-ring)
          (setcar gopher-history-ring entry)
        (push entry gopher-history-ring)
        (if (> (length gopher-history-ring)
               gopher-history-ring-max)
            (setcdr (nthcdr (1- gopher-history-ring-max)
                            gopher-history-ring) nil))))))

(defun gopher-history (&optional step)
  "Walk back through gopher's history.

With optional argument STEP, an integer, go that many steps.  If
STEP is negative, move forward through the history.  In case the
TLS mode is different for this history item, bind it locally
without changing the global mode."
  (interactive "p")
  (let* ((address (gopher-history-current-item (or step 1)))
	 (gopher-tls-mode (nth 3 address)))
    (gopher-goto-url (nth 0 address)
		     (nth 1 address)
		     (nth 2 address)
                     nil nil t)))

(defalias 'gopher-history-backwards 'gopher-history)

(defun gopher-history-forward (&optional step)
  "Walk forward through gopher's history.

With optional argument STEP, an integer, go that many steps.
If STEP is negative, move backward through the history"
  (interactive "p")
  (let ((forward-step (if step (* -1 step)
                        -1)))
    (gopher-history step)))

(defun gopher-define-keymaps ()
  "Define keys in ‘gopher-mode-map’."
  (setq gopher-mode-map (make-sparse-keymap))
  (define-key gopher-mode-map "\r" 'gopher-goto-url-at-point)
  (define-key gopher-mode-map "n" (lambda () (interactive) (forward-line)))
  (define-key gopher-mode-map "p" (lambda () (interactive) (forward-line -1)))
  (define-key gopher-mode-map "\t" (gopher-navigate directory-listing))
  (define-key gopher-mode-map "\M-\t" (gopher-navigate directory-listing t))
  (define-key gopher-mode-map "]" (gopher-navigate plain-text))
  (define-key gopher-mode-map "[" (gopher-navigate plain-text t))
  (define-key gopher-mode-map "u" 'gopher-goto-parent)
  (define-key gopher-mode-map "r" 'gopher-refresh-current-address)
  (define-key gopher-mode-map "B" 'gopher-history-backwards)
  (define-key gopher-mode-map "F" 'gopher-history-forward)
  (define-key gopher-mode-map "l" 'gopher-history-backwards)
  (define-key gopher-mode-map "r" 'gopher-history-forward)
  (define-key gopher-mode-map "g" 'gopher-refresh-current-address)
  (define-key gopher-mode-map "G" 'gopher)
  (define-key gopher-mode-map "T" 'gopher-tls-mode)
  (define-key gopher-mode-map "q" 'quit-window))

(gopher-define-keymaps)

(defun gopher-kill-address-at-point ()
  "Kill the address at point, properly combining URL properties."
  (interactive)
  (move-beginning-of-line nil)
  (let* ((properties (text-properties-at (point)))
         (string (mapconcat 'identity (list
                                       (cl-getf properties :hostname)
				       (cl-getf properties :port)
                                       (cl-getf properties :selector)) "/")))
    (kill-new string)
    (message string)))

(provide 'gopher)

;;; gopher.el ends here
