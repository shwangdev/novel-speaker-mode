;; -*- Emacs-lisp -*-
;; -*- coding: utf8; -*-

;; version 1.0
;; auther: xiang wang ( wxjeacen@gmail.com)

(require 'url)

(defgroup novel-speaker nil
  "Novel speaker mode group."
  :group 'entertainment)

(defcustom novel-speaker-cmd "/usr/local/bin/mplayer"
  "default novel speaker command."
  :type 'string
  :group 'novel-speaker)

(defcustom novel-speaker-language "zh-CN"
  "default speaker language is zh-CN"
  :type 'string
  :group 'novel-speaker)


(defcustom novel-speaker-use-proxy nil
  "default novel speaker proxy use"
  :type 'boolean
  :group 'novel-speaker)

(defcustom novel-speaker-proxy-host nil
  "novel speaker proxy host. only support http proxy."
  :type 'string
  :group 'novel-speaker)

(defcustom novel-speaker-proxy-port nil
  "novel speaker proxy port"
  :type 'number
  :group 'novel-speaker)

(defvar novel-speaker-current-status nil "novel speaker current status.")
(defvar novel-speaker-current-process nil "novel speaker current process.")
(defvar novel-speaker-current-buffer-name nil "novel speaker current buffer name.")

(defun novel-speaker-enable-proxy ()
  (interactive)
  (setq novel-speaker-use-proxy t)
  (if (not (and novel-speaker-proxy-host novel-speaker-proxy-port))
      (progn
        (message "novel speaker proxy is not set yet.")
        )))

(defun novel-speaker-disable-proxy ()
  (interactive)
  (setq novel-speaker-use-proxy nil))

(defun novel-speaker-set-proxy (host port)
  (interactive "sProxy Host:\nnProxy Port:" )
  (setq novel-speaker-proxy-host host)
  (setq novel-speaker-proxy-port port)
  (message "novel speaker proxy is %s:%d" host port))



(defun novel-speaker-select-sentence ()

  (unless novel-speaker-current-buffer-name
    (setq novel-speaker-current-buffer-name (current-buffer))
    )

  (with-current-buffer novel-speaker-current-buffer-name
    (forward-sentence)
    (backward-sentence)
    (push-mark)
    (forward-sentence)
    (buffer-substring (region-beginning) (region-end))
    )
  )


(defun novel-speaker-say-sentence (sentence)
  (unless (and novel-speaker-current-process
               (process-live-p novel-speaker-current-process))
    (message "%s" sentence)
    (setq novel-speaker-current-process
          (start-process "novel-speaker-process"
                         nil
                         novel-speaker-cmd
                         (concat (if novel-speaker-use-proxy
                                     (concat "http_proxy://"
                                             novel-speaker-proxy-host ":"
                                             (number-to-string
                                              novel-speaker-proxy-port)
                                             "/")
                                   "")
                                 (url-encode-url (concat
                                          "http://translate.google.com/translate_tts?ie=UTF-8&q="
                                          sentence
                                          "&tl=" novel-speaker-language
                                          "&total=1&index0&client=t"
                                          ))
                                 )
                         ))
    (set-process-sentinel novel-speaker-current-process
                          'novel-speaker-process-sentinel)
    )
  )

(defun novel-speaker-process-sentinel (proc change)
  (when (string-match "\\(finished\\|Exiting\\)" change)
    (let ((sentence (novel-speaker-select-sentence)))
      (if (and (string-match "running" novel-speaker-current-status)
               (not (eq "" sentence)))
          (novel-speaker-say-sentence sentence)
        ))
    )
  )

(defun novel-speaker-start ()
  "start novel speaker"
  (interactive)
  (let ((sentence (novel-speaker-select-sentence)))
    (setq novel-speaker-current-status "running")
    (novel-speaker-say-sentence sentence)
    )
  )

(defun novel-speaker-kill-process ()
  (when (and novel-speaker-current-process (process-live-p
                                            novel-speaker-current-process))
    (delete-process novel-speaker-current-process)
    (setq novel-speaker-current-process nil)
    ))

(defun novel-speaker-stop ()
  "stop novel speaker"
  (interactive)
  (progn
    (setq novel-speaker-current-status "stop")
    (novel-speaker-kill-process))
)


(defvar novel-speaker-mode-map nil)

(unless novel-speaker-mode-map
  (setq novel-speaker-mode-map (make-sparse-keymap)))

(define-minor-mode novel-speaker-mode
  "This is the  novel-speaker mode for mac os."
  :lighter "NovelSpeak"
  :group 'novel-speaker
  :keymap novel-speaker-mode-map
  )

(provide 'novel-speaker-mode)
