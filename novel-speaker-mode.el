;; -*- Emacs-lisp -*-
;; -*- coding: utf8; -*-

;; version 1.0
;; auther: xiang wang ( wxjeacen@gmail.com)

(require 'url)
(require 'json)
(require 'url-http)
(require 'helm-files)

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

(defcustom novel-speaker-app-id "3258863"
  "baidu app id"
  :type 'string
  :group 'novel-speaker)

(defcustom novel-speaker-app-key "U2bNAXUG0XuZ88gv9bEcGT5H"
  "baidu app key"
  :type 'string
  :group 'novel-speaker)

(defcustom novel-speaker-app-secret-key "A4oC5gxvK6GCdCEfntAgEECXP74RLgib"
  "baidu app secrte key"
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
(defvar novel-speaker-oauth-token nil "baidu oauth token")

(defconst  novel-speaker-oauth-url (concat
                                  "https://openapi.baidu.com/oauth/2.0/token?grant_type=client_credentials&client_id="
                                  novel-speaker-app-key
                                  "&client_secret="
                                  novel-speaker-app-secret-key
                                  )
  )

(defun novel-speaker-open-file ()
  (interactive)
  (helm-find-files-1 "~/" )
  (setq novel-speaker-current-buffer-name (current-buffer))
  )

(defun novel-speaker-enable-proxy ()
  (interactive)
  (if (not (and novel-speaker-proxy-host novel-speaker-proxy-port))
      (progn
        (message "novel speaker proxy is not set yet.")
        )
    (let ((schema (concat novel-speaker-proxy-host ":" (number-to-string novel-speaker-proxy-port)) ))
      (message schema)
      (setq url-proxy-services (cons (cons "https" schema) nil))
      (setq novel-speaker-use-proxy t)
      (novel-speaker-parse-oauth  (novel-speaker-send-url novel-speaker-oauth-url))
      )
    )
  )


(defun novel-speaker-disable-proxy ()
  (interactive)
  (setq novel-speaker-use-proxy nil))

(defun novel-speaker-set-proxy (host port)
  (interactive "sProxy Host:\nnProxy Port:" )
  (setq novel-speaker-proxy-host host)
  (setq novel-speaker-proxy-port port)

  (message "novel speaker proxy is %s:%d" host port))


(defun novel-speaker-send-url (url &optional url-args callback callback-args)
  (let ((url-request-method "GET"))
    (if url-args
        (setq url-request-data (mapconcat #'(lambda (arg)
                                              (concat (url-hexify-string (car arg))
                                                      "="
                                                      (url-hexify-string (cdr arg))))
                                          url-args "&")))
    (if callback
        (url-retrieve url callback callback-args)
      (url-retrieve-synchronously url)))
  )




(defun novel-speaker-parse-oauth (json-buffer)
  (let (json-start
        json-end
        json
        (json-object-type 'hash-table)
        )
    (with-current-buffer json-buffer
      (goto-char (point-min))
      ( if (not (search-forward "200 OK"))
          (message "oath failed.")
        (progn
            (goto-char (point-max))
            (backward-sentence)
            (setq json-start (line-beginning-position))
            (setq json-end (line-end-position))
            (let (
                  (my-hash  (json-read-from-string (decode-coding-string
                                      (buffer-substring json-start json-end)
                                      'utf-8))))
              (if (hash-table-p my-hash)
                  (progn
                    ;;
                    (message "oauth success.")
                    (setq novel-speaker-oauth-token (gethash "access_token" my-hash))
                    )
                (message "Invalid oauth response data."))
              )
          )
        )
      )
    )
  )



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

(defun novel-speaker-show-next-sentence ()
  ;; when text to audio server is not running,
  ;;then just show sentence in min buffer
  (interactive)
  (if (not
       novel-speaker-current-buffer-name)
      (message "No text file selected to read.")
    (if (and
         novel-speaker-current-buffer-name
         ( or (eq nil novel-speaker-current-status)
              (not (string-match "running" novel-speaker-current-status)
                   )
              )
         )
        (message "%s" (novel-speaker-select-sentence))
        )
    )
  )

(defun novel-speaker-say-sentence-loop (sentence)
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
                                          "http://tsn.baidu.com/text2audio?tex="
                                          sentence
                                          "&lan=zh&cuid=xxxxxx&ctp=1&tok="
                                          novel-speaker-oauth-token
                                          "&spd=7"
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
          (novel-speaker-say-sentence-loop sentence)
        ))
    )
  )

(defun novel-speaker-start ()
  "start novel speaker"
  (interactive)
  (let ((sentence (novel-speaker-select-sentence)))
    (novel-speaker-parse-oauth  (novel-speaker-send-url novel-speaker-oauth-url))
    (setq novel-speaker-current-status "running")
    (novel-speaker-say-sentence-loop sentence)
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


;;(defvar novel-speaker-mode-map nil)

;;(unless novel-speaker-mode-map
;;  (setq novel-speaker-mode-map (make-sparse-keymap)))

(define-minor-mode novel-speaker-mode
  "This is the  novel-speaker mode for mac os."
  :lighter "NovelSpeak"
  :group 'novel-speaker
  :keymap  (let ((map  (make-sparse-keymap)))
             (define-key map (kbd "\C-c \C-n b" ) 'novel-speaker-start)
             (define-key map (kbd "\C-c \C-n e" ) 'novel-speaker-stop)
             (define-key map (kbd "\C-c \C-n p" ) 'novel-speaker-set-proxy)
             map)
  )

(global-set-key (kbd "<M-return>") 'novel-speaker-show-next-sentence)

(add-hook 'text-mode-hook 'novel-speaker-mode)
(provide 'novel-speaker-mode)
