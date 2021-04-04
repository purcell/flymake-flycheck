;;; flymake-flycheck.el --- Use flycheck checkers as flymake backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience, languages, tools
;; Homepage: https://github.com/purcell/flymake-flycheck
;; Version: 0.1-pre
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

(defvar flymake-flycheck-debug nil
  "When non-nil, output some debug messages.")

(defun flymake-flycheck--debug (msg &rest args)
  "Outputs debug messages if `flymake-flycheck-debug' is non-nil.
MSG and ARGS are passed to `message'."
  (when flymake-flycheck-debug
    (apply 'message (concat "[flymake-flycheck] " msg) args)))

;;;###autoload
(defun flymake-flycheck-diagnostic-function-for (checker)
  "Wrap CHECKER to make a `flymake-diagnostics-functions' backend."
  (apply-partially #'flymake-flycheck--diagnostic-function checker))

(defun flymake-flycheck--diagnostic-function (checker report-fn &rest _)
  "Run CHECKER passing its translated results to a Flymake REPORT-FN."
  ;; TODO: interrupt any check currently running for this checker,
  ;; using `flycheck-syntax-check-interrupt'
  (flymake-flycheck--debug "start syntax check for %s" checker)
  (flycheck-syntax-check-start
   (flycheck-syntax-check-new
    :buffer (current-buffer)
    :checker checker
    :context nil
    :working-directory (flycheck-compute-working-directory checker))
   (lambda (status &optional data)
     (flymake-flycheck--debug "received status %S from %s" status checker)
     (pcase status
       ('errored (funcall report-fn
                          :panic
                          :explanation (format "Flycheck checker %s reported error %S" checker data)))
       ('finished (funcall report-fn
                           (mapcar #'flymake-flycheck--translate-error data)
                           :region (cons (point-min) (point-max))))
       ('interrupted (flymake-flycheck--debug "checker %s reported being interrupted %S" checker data))
       ('suspicious (flymake-flycheck--debug "checker %s reported suspicious result %S" checker data))
       (_ (flymake-flycheck--debug "unexpected status from checker %s: %S" checker status))))))

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
