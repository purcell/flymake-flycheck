;;; flymake-flycheck.el --- Use flycheck checkers as flymake backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, languages, tools
;; Homepage: https://github.com/purcell/flymake-flycheck
;; Version: 0.2
;; Package-Requires: ((flycheck "31") (emacs "26.1"))

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

;; The core of the package is the ability to wrap a single checker
;; into a flymake diagnostic function which could be added to
;; `flymake-diagnostic-functions':

;;    (flymake-flycheck-diagnostic-function-for 'sh-shellcheck)

;; Flycheck has the convenient notion of "available" checkers, which is
;; determined at runtime according to mode and availability of necessary
;; tools, as well as config for explicitly "chained" checkers.

;; Accordingly, you can obtain the diagnostic functions for all checkers
;; that flycheck would enable in the current buffer like this:

;;    (flymake-flycheck-all-chained-diagnostic-functions)

;; In practical terms, most users will want to simply enable all
;; available checkers whenever `flymake-mode' is enabled:

;;    (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)

;; If you find that `flymake' is now running `flycheck' checkers which
;; are redundant because there's already a `flymake' equivalent, simply
;; add those checkers to the `flycheck-disabled-checkers' variable, e.g.

;;    (add-to-list 'flycheck-disabled-checkers 'sh-shellcheck)

;; Some caveats:

;; * Flycheck UI packages will have no idea of what the checkers are
;;   doing, because they are run without flycheck's coordination.
;; * Flycheck's notion of "chained checkers" is not handled
;;   automatically, so although multiple chained checkers can be used,
;;   they will all be executed simultaneously even if earlier checkers
;;   fail.  This could either be considered a feature, or lead to
;;   redundant confusing messages.

;;; Code:

(require 'flycheck)
(require 'flymake)

(defvar flymake-flycheck-debug nil
  "When non-nil, output some debug messages.")

(defun flymake-flycheck--debug (msg &rest args)
  "Outputs debug messages if `flymake-flycheck-debug' is non-nil.
MSG and ARGS are passed to `message'."
  (when flymake-flycheck-debug
    (apply 'message (concat "[flymake-flycheck] (%s) " msg) (buffer-name) args)))

;;;###autoload
(defun flymake-flycheck-all-available-diagnostic-functions ()
  "Return a list of diagnostic functions for all usable checkers.
These might end up providing duplicate functionality, e.g. both
dash and bash might be used to check a `sh-mode' buffer if both are
found to be installed.

Usually you will want to use `flymake-flycheck-all-chained-diagnostic-functions' instead."
  (mapcar #'flymake-flycheck-diagnostic-function-for
          (seq-filter #'flycheck-may-use-checker flycheck-checkers)))

;;;###autoload
(defun flymake-flycheck-all-chained-diagnostic-functions ()
  "Return a list of diagnostic functions for the current checker chain."
  (when-let ((start (or flycheck-checker (seq-find #'flycheck-may-use-checker flycheck-checkers))))
    (let (checkers
          (nexts (list start)))
      (while nexts
        (setq checkers (append checkers nexts))
        (setq nexts (seq-filter #'flycheck-may-use-checker
                                (seq-mapcat #'flycheck-get-next-checkers nexts))))
      (mapcar #'flymake-flycheck-diagnostic-function-for (seq-uniq checkers)))))

;;;###autoload
(defun flymake-flycheck-auto ()
  "Activate all available flycheck checkers in the current buffer."
  (setq-local flymake-diagnostic-functions
              (seq-uniq (append flymake-diagnostic-functions
                                (flymake-flycheck-all-chained-diagnostic-functions)))))

(defconst flymake-flycheck--wrapper-prefix "flymake-flycheck:")
(defconst flymake-flycheck--force-redef nil)

;; Remove old wrapper functions, just in case we're reloading this package
(mapatoms (lambda (sym)
            (when (string-prefix-p flymake-flycheck--wrapper-prefix (symbol-name sym))
              (makunbound sym)
              (fmakunbound sym))))

;;;###autoload
(defun flymake-flycheck-diagnostic-function-for (checker)
  "Wrap CHECKER to make a `flymake-diagnostics-functions' backend."
  (let ((fname (intern (concat flymake-flycheck--wrapper-prefix (symbol-name checker))))
        (cur-check-var (intern (concat flymake-flycheck--wrapper-prefix
                                       (symbol-name checker)
                                       "--current-check"))))
    (unless (or (fboundp fname) flymake-flycheck--force-redef)
      (set-default cur-check-var nil)
      (make-variable-buffer-local cur-check-var)
      (fset fname (apply-partially 'flymake-flycheck--wrapper checker cur-check-var)))
    fname))

(defun flymake-flycheck--wrapper (checker cur-check-var report-fn &rest _)
  "Wrap CHECKER, maintaining the current check in CUR-CHECK-VAR.
With these args partially applied, the result is a standard
flymake diagnostic function which accepts REPORT-FN etc."
  (when-let ((current-check (symbol-value cur-check-var)))
    (flymake-flycheck--debug "interrupting defunct syntax check for %s" checker)
    (flycheck-syntax-check-interrupt current-check)
    (set-variable cur-check-var nil))
  (flymake-flycheck--debug "start syntax check for %s" checker)
  (set-variable cur-check-var
                (flycheck-syntax-check-new
                 :buffer (current-buffer)
                 :checker checker
                 :context nil
                 :working-directory (flycheck-compute-working-directory checker)))
  (flycheck-syntax-check-start
   (symbol-value cur-check-var)
   (lambda (status &optional data)
     (flymake-flycheck--debug "received status %S from %s" status checker)
     (pcase status
       ('errored (funcall report-fn
                          :panic
                          :explanation (format "Flycheck checker %s reported error %S" checker data)))
       ('finished (funcall report-fn
                           (mapcar 'flymake-flycheck--translate-error data)
                           :region (cons (point-min) (point-max))))
       ('interrupted (flymake-flycheck--debug "checker %s reported being interrupted %S" checker data))
       ('suspicious (flymake-flycheck--debug "checker %s reported suspicious result %S" checker data))
       (_ (flymake-flycheck--debug "unexpected status from checker %s: %S" checker status))))))

(defun flymake-flycheck--translate-error (err)
  "Translate flycheck CHECKER error ERR into a flymake diagnostic."
  (flycheck-error-with-buffer err
    ;; TODO: handle id, group, filename
    (flymake-make-diagnostic
     (flycheck-error-buffer err)
     (flycheck-error-pos err)
     (pcase (flycheck--exact-region err)
       (`(,beg . ,end) end)
       (_ (1+ (flycheck-error-pos err))))
     (pcase (flycheck-error-level err)
       ('error 'flymake-error)
       ('warning 'flymake-warning)
       ('info 'flymake-note)
       (_
        (flymake-flycheck--debug "Translating unknown error level %s to note" (flycheck-error-level err))
        'flymake-note))
     (flycheck-error-message err))))


(provide 'flymake-flycheck)
;;; flymake-flycheck.el ends here
