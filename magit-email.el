;;; magit-email --- Send patches as emails from Magit.

;;; Commentary:

;;; Code:

(require 'magit-patch)
(require 'transient)

(defun magit-email--parse-header (header)
  "Parse a HEADER line to a cons of (header . value)."
  (when (string-match-p ": " header)
    (with-temp-buffer
      (insert header)
      (goto-char (point-min))
      (re-search-forward ":")
      (cons (string-trim
             (buffer-substring-no-properties
              (point-at-bol)
              (- (point) 1)))
            (string-trim
             (buffer-substring-no-properties
              (point)
              (point-at-eol)))))))

(defun magit-email--patch-headers (patch)
  "Return an alist of headers for PATCH."
  (let ((raw (with-temp-buffer
               (insert-file-contents patch)
               (re-search-forward "^\n")
               (buffer-substring-no-properties
                (point-min)
                (point)))))
    (remove nil (mapcar #'magit-email--parse-header (split-string raw "\n" t)))))

(defun magit-email--patch-contents (patch)
  "Return the diff of PATCH."
  (with-temp-buffer
    (insert-file-contents patch)
    (diff-mode)
    (font-lock-mode)
    (when (re-search-forward "\n\n" nil)
      (buffer-substring
       (point-at-bol)
       (point-max)))))

(defun magit-email--compose (patch)
  "Compose an email based on PATCH using `compose-mail'."
  (let* ((headers (magit-email--patch-headers patch))
         (subject (cdr (assoc "Subject" headers)))
         (to (cdr (assoc "To" headers)))
         (headers (assoc-delete-all "To" (assoc-delete-all "Subject" headers))))
    (compose-mail to subject headers)
    (save-excursion
      (message-goto-body)
      (insert "\n" (magit-email--patch-contents patch)))
    (if (not to)
        (message-goto-to)
      (message-goto-body))))

;; This function is a modified version of `magit-patch-create'.
;;;###autoload
(defun magit-email-patch-send (range args files)
  "Send email for commits in RANGE.
Uses the same semantics as `magit-patch-create' in
magit-patch.el."
  (interactive
   (cons (if-let ((revs (magit-region-values 'commit t)))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (magit-read-range-or-commit
                         "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (let ((args (transient-args 'magit-patch-create)))
           (list (-filter #'stringp args)
                 (cdr (assoc "--" args))))))
  (with-temp-buffer
    (magit-process-git (current-buffer) "format-patch" range args "--" files)
    (let ((patches (nreverse (butlast (split-string (buffer-string) "\n")))))
      (mapc #'magit-email--compose patches)
      (mapc #'delete-file patches))))

;;;###autoload
(with-eval-after-load 'magit-patch
  (transient-append-suffix 'magit-patch-create "c"
    '(1 "m" "Mail patch" magit-email-patch-send)))

(provide 'magit-email)
;;; magit-email.el ends here
