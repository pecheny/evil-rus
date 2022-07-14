;;; evil-rus.el --- Alternative input methods support for evil mode -*- lexical-binding: t; -*-
;; Author: khaoos-abominable, pecheny
;; Maintainer:  pecheny
;; Version: 0.0.1
;; Keywords: evil-mode, input-method, emulations, i18n
;; Homepage: https://github.com/pecheny/evil-rus
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Evil operators ignore your input method and treat argument as if it was typed in default english layout.
;; This package aimed to fix this issue. Though it may have limitations.
;; Original author khaoos-abominable had done the solution in personal config.
;; https://github.com/khaoos-abominable/dotfiles/blob/master/spacemacs/dotspacemacs
;; This version is extracted for use as a package especcially for doom users. Tested with doom and russian-computer input-method.
;;
;; Add to packages.el:
;;
;; (package! projectile
;;   :recipe (:host github :repo "pecheny/evil-rus" ))
;;
;; and to config.el:
;;
;; (define-key evil-normal-state-map (kbd "r") 'evil-rus-evil-replace)
;; (define-key evil-motion-state-map (kbd "f") 'evil-rus-evil-find-char)
;; (define-key evil-motion-state-map (kbd "t") 'evil-rus-evil-find-char-to)
;; (define-key evil-motion-state-map (kbd "F") 'evil-rus-evil-find-char-backward)
;; (define-key evil-motion-state-map (kbd "T") 'evil-rus-evil-find-char-to-backward)
;;
;; and on your preferences :
;;
;; (map! :vn "gss" 'evil-evil-rus-avy-goto-char-2)
;; (map! :vn "gsd" 'evil-evil-rus-avy-goto-word-or-subword-1)
;; (map! :vn "gsx" 'evil-evil-rus-avy-goto-char)
;; (define-key evil-normal-state-map (kbd "M-i") 'evil-rus-insert-one-char)
;; (define-key evil-normal-state-map (kbd "M-a") 'evil-rus-append-one-char)
;;
;; See more: https://github.com/emacs-evil/evil/issues/605
;;
;;; Code:

(require 'evil-escape)
(require 'quail)

(defvar evil-rus-input-method-last-raw-key nil
  "The last key pressed with an input method switched on but ignoring conversion of the input method.")

;;;###autoload
(defun evil-rus-capture-input-mode-raw-key (key)
  "Function captures an input key ignoring the current input method.
Doesn't work for complex input methods which use event loops."
  (setq evil-rus-input-method-last-raw-key (char-to-string key)))

;;;###autoload
(defun evil-rus-activate-input-method (input-method)
  "Defines an advise for a function which implements current input method."
  ;; We don't bother ourselves to remove the advise when we deactivate the input method.
  ;; The chances are high that we'll reuse it.
  ;; We may end up with several advices for different input methods if an user uses them.
  ;; It doesn't matter as the only one work at the moment.
  ;; I saw a case when input-method-function was equal to 'list'! So there is addition check
  ;; on current-input-method
  (if (and current-input-method input-method-function)
      (advice-add input-method-function :before #'evil-rus-capture-input-mode-raw-key)))

;;;###autoload
(advice-add 'activate-input-method :after #'evil-rus-activate-input-method)

(defcustom evil-rus-evil-escape-ignore-input-method nil
  "If non-nil then the key sequence can be entered ignoring the current input method if any."
  :type 'boolean
  :group 'evil-escape)

;;;###autoload
(defun evil-rus-evil-escape-p ()
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
           (and evil-rus-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal evil-rus-input-method-last-raw-key (evil-escape--first-key))) ;;khaoos+
           (and evil-escape-unordered-key-sequence
                (or (equal (this-command-keys) (evil-escape--second-key))))
           (and evil-escape-unordered-key-sequence ;;khaoos+
                evil-rus-evil-escape-ignore-input-method ;;khaoos+
                current-input-method ;;khaoos+
                (equal evil-rus-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
       (not (cl-reduce (lambda (x y) (or x y))
                       (mapcar 'funcall evil-escape-inhibit-functions)
                       :initial-value nil))))

;;;###autoload
(defun evil-rus-evil-escape-pre-command-hook ()
  "`evil-escape' pre-command hook.
Edited by khaoos to implement the ability of ignoring the input method"
  (with-demoted-errors "evil-escape: Error %S"
      (when (evil-rus-evil-escape-p)
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
                              (and evil-rus-evil-escape-ignore-input-method ;;khaoos+
                                   current-input-method ;;khaoos+
                                   (equal evil-rus-input-method-last-raw-key (evil-escape--first-key)))) ;;khaoos+
                          (char-equal evt skey))
                     (and evil-escape-unordered-key-sequence
                          (or (equal (this-command-keys) (evil-escape--second-key)) ;;khaoos*
                              (and evil-rus-evil-escape-ignore-input-method ;;khaoos+
                                   current-input-method ;;khaoos+
                                   (equal evil-rus-input-method-last-raw-key (evil-escape--second-key)))) ;;khaoos+
                          (char-equal evt fkey))))
            (evil-repeat-stop)
            (when (evil-escape-func) (setq this-command (evil-escape-func))))
           ((null evt))
           (t (setq unread-command-events
                    (append unread-command-events (list evt)))))))))

(advice-add 'evil-escape-pre-command-hook :override #'evil-rus-evil-escape-pre-command-hook)

;;;###autoload
(defun evil-rus-evil-read-key-respect-input-method (evil-read-key-result)
  "Get the result of evil-read-key function and convert it according the current input method which at the moment could be a method of a family of quail input methods."
  (if (and (characterp evil-read-key-result)
           current-input-method
           (equal input-method-function 'quail-input-method))
    (let* ((translated-key-list (quail-lookup-key (char-to-string evil-read-key-result)))
           (translated-key (if (equal (length translated-key-list) 1)
                               (car translated-key-list)
                               evil-read-key-result)))
          translated-key)
    evil-read-key-result))

(advice-add 'evil-read-key :filter-return 'evil-rus-evil-read-key-respect-input-method)

;;;###autoload
(defun evil-rus-run-evil-command-respect-input-method (evil-command)
  "Run interactively evil command evil-command which now respects the current input method."
  ;; if we are in the mode which prohibits input method we do a trick
  (if (and evil-input-method (not current-input-method))
      (evil-without-input-method-hooks
        (activate-input-method evil-input-method)
        (condition-case err
            (call-interactively evil-command)
          (error
            (deactivate-input-method)
            (signal (car err) (cdr err))))
        (deactivate-input-method))
    (call-interactively evil-command)))


;; (with-eval-after-load "evil-macros"
;;;###autoload (autoload 'evil-rus-evil-replace "evil-rus" nil t)
  (evil-define-operator evil-rus-evil-replace ()
    "Wrapper of evil-replace to make it respect input method"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-replace))

  (evil-define-motion evil-rus-evil-find-char ()
    "Wrapper of evil-find-char to make it respect input method"
    :type inclusive
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-find-char))

  (evil-define-motion evil-rus-evil-find-char-to ()
    "Wrapper of evil-find-char-to to make it respect input method"
    :type inclusive
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-find-char-to))

  (evil-define-motion evil-rus-evil-find-char-backward ()
    "Wrapper of evil-find-char-backward to make it respect input method"
    :type exclusive
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-find-char-backward))

  (evil-define-motion evil-rus-evil-find-char-to-backward ()
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-find-char-to-backward))

  (evil-define-operator evil-rus--insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (let ((a (read-char "Input a character to insert:" t)))
      (insert-char a)))

;;;###autoload (autoload 'evil-rus-insert-one-char "evil-rus" nil t)
  (evil-define-operator evil-rus-insert-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-rus--insert-one-char))

  (evil-define-operator evil-rus--append-one-char ()
    "Switches to insert mode just to append one character"
    (interactive)
    (let ((a (read-char "Input a character to append:" t)))
      (unless (eolp) (forward-char))
      (insert-char a)
		  (unless (eolp) (backward-char))))

;;;###autoload (autoload 'evil-rus-append-one-char  "evil-rus" nil t)
  (evil-define-operator evil-rus-append-one-char ()
    "Switches to insert mode just to input one character"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'evil-rus--append-one-char))
;; )

 ;; (with-eval-after-load "evil-integration"
;;;###autoload
  (defun evil-rus-avy-goto-char ()
    "Make `evil-avy-go-to-char' respect the current input method"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'avy-goto-char))

  (evil-define-avy-motion evil-rus-avy-goto-char inclusive)

;;;###autoload
  (defun evil-rus-avy-goto-char-2 ()
    "Make `evil-avy-go-to-char-2' respect the current input method"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'avy-goto-char-2))

  (evil-define-avy-motion evil-rus-avy-goto-char-2 inclusive)

;;;###autoload
  (defun evil-rus-avy-goto-word-or-subword-1 ()
    "Make `avy-goto-word-or-subword-1' respect the current input method"
    (interactive)
    (evil-rus-run-evil-command-respect-input-method 'avy-goto-word-or-subword-1))

  (evil-define-avy-motion evil-rus-avy-goto-word-or-subword-1 exclusive)


(provide 'evil-rus)
;;; evil-rus.el ends here
