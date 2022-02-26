;;; evil-rus.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022
;;
;; Author:  <https://github.com/shoo>
;; Maintainer:  <shoo@T8>
;; Created: February 27, 2022
;; Modified: February 27, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/shoo/evil-rus
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;;###autoload

(defun khaoos-capture-input-mode-raw-key (key)
  "Function captures an input key ignoring the current input method.
Doesn't work for complex input methods which use event loops."
  (setq khaoos-input-method-last-raw-key (char-to-string key)))

(defun khaoos-activate-input-method (input-method)
  "Defines an advise for a function which implements current input method."
  ;; We don't bother ourselves to remove the advise when we deactivate the input method.
  ;; The chances are high that we'll reuse it.
  ;; We may end up with several advices for different input methods if an user uses them.
  ;; It doesn't matter as the only one work at the moment.
  ;; I saw a case when input-method-function was equal to 'list'! So there is addition check
  ;; on current-input-method
  (if (and current-input-method input-method-function)
      (advice-add input-method-function :before #'khaoos-capture-input-mode-raw-key)))

(advice-add 'activate-input-method :after #'khaoos-activate-input-method)

(defcustom khaoos-evil-escape-ignore-input-method nil
  "If non-nil then the key sequence can be entered ignoring the current input method if any."
  :type 'boolean
  :group 'evil-escape)

(defun khaoos-evil-escape-p ()
  "Return non-nil if evil-escape can run.
Edited by khaoos to implement the ability of ignoring the input method"
  (and evil-escape-key-sequence
       (not evil-escape-inhibit)
       (or (window-minibuffer-p)
           (bound-and-true-p isearch-mode)
           (memq major-mode '(ibuffer-mode
                              image-mode))
           (evil-escape--is-magit-buffer)
           (and (fboundp 'helm-alive-p) (helm-alive-p))
           (or (not (eq 'normal evil-state))
               (not (eq 'evil-force-normal-state
                        (lookup-key evil-normal-state-map [escape])))))
       (not (memq major-mode evil-escape-excluded-major-modes))
       (not (memq evil-state evil-escape-excluded-states))
       (or (not evil-escape-enable-only-for-major-modes)
           (memq major-mode evil-escape-enable-only-for-major-modes))
       (or (equal (this-command-keys) (evil-escape--first-key))
           (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal khaoos-input-method-last-raw-key (evil-escape--first-key))) ;;khaoos+
           (and evil-escape-unordered-key-sequence
                (or (equal (this-command-keys) (evil-escape--second-key))))
           (and evil-escape-unordered-key-sequence ;;khaoos+
                khaoos-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal khaoos-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
       (not (cl-reduce (lambda (x y) (or x y))
                       (mapcar 'funcall evil-escape-inhibit-functions)
                       :initial-value nil))))

(defun khaoos-evil-escape-pre-command-hook ()
  "evil-escape pre-command hook.
Edited by khaoos to implement the ability of ignoring the input method"
  (with-demoted-errors "evil-escape: Error %S"
      (when (khaoos-evil-escape-p)
        (let* ((modified (buffer-modified-p))
               (inserted (evil-escape--insert))
               (fkey (elt evil-escape-key-sequence 0))
               (skey (elt evil-escape-key-sequence 1))
               (evt (read-event nil nil evil-escape-delay)))
          (when inserted (evil-escape--delete))
          (set-buffer-modified-p modified)
          (cond
           ((and (characterp evt)
                 (or (and (or (equal (this-command-keys) (evil-escape--first-key)) ;;khaoos*
                              (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                                   current-input-method ;;khaoos+
                                   (equal khaoos-input-method-last-raw-key (evil-escape--first-key)))) ;;khaoos+
                          (char-equal evt skey))
                     (and evil-escape-unordered-key-sequence
                          (or (equal (this-command-keys) (evil-escape--second-key)) ;;khaoos*
                              (and khaoos-evil-escape-ignore-input-method ;;khaoos+
                                   current-input-method ;;khaoos+
                                   (equal khaoos-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
                          (char-equal evt fkey))))
            (evil-repeat-stop)
            (when (evil-escape-func) (setq this-command (evil-escape-func))))
           ((null evt))
           (t (setq unread-command-events
                    (append unread-command-events (list evt)))))))))

(advice-add 'evil-escape-pre-command-hook :override #'khaoos-evil-escape-pre-command-hook)

(defun khaoos-evil-read-key-respect-input-method (evil-read-key-result)
  "Gets the result of evil-read-key function and converts it according the current input method
which at the moment could be a method of a family of quail input methods"
  (if (and (characterp evil-read-key-result)
           current-input-method
           (equal input-method-function 'quail-input-method))
    (let* ((translated-key-list (quail-lookup-key (char-to-string evil-read-key-result)))
           (translated-key (if (equal (length translated-key-list) 1)
                               (car translated-key-list)
                               evil-read-key-result)))
          translated-key)
    evil-read-key-result))

(advice-add 'evil-read-key :filter-return 'khaoos-evil-read-key-respect-input-method)

(defun khaoos-run-evil-command-respect-input-method (evil-command)
  "Runs interactively evil command evil-command which now respects the current input method"
  ;; if we are in the mode which prohibits input method we do a trick
  (if (and evil-input-method (not current-input-method))
      (evil-without-input-method-hooks
        (activate-input-method evil-input-method)
        (condition-case err
            (call-interactively evil-command)
          (error
            (inactivate-input-method)
            (signal (car err) (cdr err))))
        (inactivate-input-method))
    (call-interactively evil-command)))

;; (with-eval-after-load "evil-macros"
  (evil-define-operator khaoos-evil-replace ()
    "Wrapper of evil-replace to make it respect input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-replace))

  (evil-define-motion khaoos-evil-find-char ()
    "Wrapper of evil-find-char to make it respect input method"
    :type inclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-find-char))

  (evil-define-motion khaoos-evil-find-char-to ()
    "Wrapper of evil-find-char-to to make it respect input method"
    :type inclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-find-char-to))

  (evil-define-motion khaoos-evil-find-char-backward ()
    "Wrapper of evil-find-char-backward to make it respect input method"
    :type exclusive
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-find-char-backward))

  (evil-define-motion khaoos-evil-find-char-to-backward ()
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'evil-find-char-to-backward))

  (evil-define-operator khaoos--insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (let ((a (read-char "Input a character to insert:" t)))
      (insert-char a)))

  (evil-define-operator khaoos-insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'khaoos--insert-one-char))

  (evil-define-operator khaoos--append-one-char ()
    "Switches to insert mode just to append one character"
    (interactive)
    (let ((a (read-char "Input a character to append:" t)))
      (unless (eolp) (forward-char))
      (insert-char a)
		  (unless (eolp) (backward-char))))

  (evil-define-operator khaoos-append-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'khaoos--append-one-char))
;; )

;; (with-eval-after-load "evil-integration"
  (defun khaoos-avy-goto-char ()
    "Make `evil-avy-go-to-char' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-char))

  (evil-define-avy-motion khaoos-avy-goto-char inclusive)

  (defun khaoos-avy-goto-char-2 ()
    "Make `evil-avy-go-to-char-2' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-char-2))

  (evil-define-avy-motion khaoos-avy-goto-char-2 inclusive)

  (defun khaoos-avy-goto-word-or-subword-1 ()
    "Make `evil-avy-go-to-char' respect the current input method"
    (interactive)
    (khaoos-run-evil-command-respect-input-method 'avy-goto-word-or-subword-1))

  (evil-define-avy-motion khaoos-avy-goto-word-or-subword-1 exclusive)
;; )
(provide 'evil-rus)
;;; evil-rus.el ends here
