;;; flymake-flycheck.el --- Use flycheck checkers as flymake backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, languages, tools
;; Homepage: https://github.com/purcell/flymake-flycheck
;; Version: 0.1-pre
;; Package-Requires: ((flycheck "31") (emacs "24.1"))

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

;; WARNING: EARLY PREVIEW CODE, SUBJECT TO CHANGE

;; This package provides support for running any flycheck checker as a
;; flymake diagnostic backend.  The effect is that flymake will
;; control when the checker runs, and flymake will receive its errors.

;; For example, to enable a couple of flycheck checkers in a bash
;; buffer, the following code is sufficient:

;; (setq-local flymake-diagnostic-functions
;;             (list (flymake-flycheck-diagnostic-function-for 'sh-shellcheck)
;;                   (flymake-flycheck-diagnostic-function-for 'sh-posix-bash)))

;; TODO:

;; - Make it easy to enable all available checkers for a buffer

;; Caveats:

;; - Flycheck UI packages will have no idea of what the checkers are doing,
;;   because they are run without flycheck's coordination.
;; - Flycheck's notion of "chained checkers" is not handled automatically

;;; Code:

(require 'flycheck)
(require 'flymake)

(defun flymake-flycheck-diagnostic-function-for (checker)
  "Wrap CHECKER to make a `flymake-diagnostics-functions' backend."
  (lambda (report-fn &rest _)
    (when (flycheck-running-p)
      (flycheck-syntax-check-interrupt flycheck-current-syntax-check))
    ;; Based on `flycheck-start-current-syntax-check'
    (setq flycheck-current-syntax-check
          (flycheck-syntax-check-new
           :buffer (current-buffer)
           :checker checker
           :context nil
           :working-directory (flycheck-compute-working-directory checker)))
    (message "start syntax check")
    (flycheck-syntax-check-start
     flycheck-current-syntax-check
     (lambda (status &optional data)
       (message "receive status %S" status)
       (pcase status
         ('errored (funcall report-fn
                            :panic
                            :explanation (format "flycheck checker %s reported error %S" checker data)))
         ('finished (funcall report-fn
                             (mapcar #'flymake-flycheck--translate-error data)
                             :region (cons (point-min) (point-max))))
         ('interrupted (message "flycheck checker %s reported being interrupted %S" checker data))
         ('suspcicious (message "flycheck checker %s reported suspicious result %S" checker data)))))))

(defun flymake-flycheck--translate-error (err)
  "Translate flycheck error ERR into a flymake diagnostic."
  (flycheck-error-with-buffer err
    ;; TODO: handle id, group, filename
    (flymake-make-diagnostic
     (flycheck-error-buffer err)
     (flycheck-error-pos err)
     (pcase (flycheck--exact-region err)
       (`(,beg . ,end) end)
       (t (1+ (flycheck-error-pos err))))
     (flycheck-error-level err)
     (flycheck-error-message err))))

(put 'note 'flymake-category 'flymake-note)
(put 'warning 'flymake-category 'flymake-warning)
(put 'error 'flymake-category 'flymake-error)


(provide 'flymake-flycheck)
;;; flymake-flycheck.el ends here
