;; Clean Emacs 30 config
(global-set-key (kbd "C-c t") (lambda () (interactive) (message "Eval worked!")))

;; Package setup
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Basic preferences
(prefer-coding-system 'utf-8)
(setq mac-command-modifier 'meta)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
(load-theme 'deeper-blue t)
(set-face-attribute 'default nil :height 150)
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

;; Tab bar
(tab-bar-mode 1)
(set-face-attribute 'tab-bar-tab nil
                    :background "gray40"
                    :foreground "white"
                    :weight 'bold)
(set-face-attribute 'tab-bar-tab-inactive nil
                    :background "gray70"
                    :foreground "gray40"
                    :weight 'normal
                    :box nil)
(global-set-key (kbd "M-]") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-[") #'tab-bar-switch-to-prev-tab)

(defun my/tab-new-and-rename ()
  "Create a new tab, then immediately rename it."
  (interactive)
  (tab-bar-new-tab)
  (call-interactively #'tab-bar-rename-tab))

(global-set-key (kbd "M-n") #'my/tab-new-and-rename)
(global-set-key (kbd "S-<left>")  #'tab-bar-move-tab-backward)
(global-set-key (kbd "S-<right>") (lambda () (interactive) (tab-bar-move-tab 1)))

;; Helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

;; Ranger
(require 'ranger)
(define-key ranger-normal-mode-map (kbd "+") #'dired-create-directory)
(ranger-override-dired-mode t)
(setq helm-descbinds-window-style 'same-window)
(setq ranger-preview-file t)
(setq ranger-dont-show-binary t)
(setq ranger-width-preview 0.30)
(setq ranger-width-parents 0.30)
(defun r ()
  "Shortcut to open ranger file manager."
  (interactive)
  (ranger))

;; Global settings
(setq line-number-mode t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(show-paren-mode t)
(global-auto-revert-mode t)
(setq auto-revert-interval 1)           ; Check for file changes every 1 second
(setq auto-revert-use-notify t)         ; Use file system notifications when available
(delete-selection-mode 1)
(global-visual-line-mode t)
(setq-default word-wrap t)
(add-to-list 'exec-path "/usr/local/bin")

;; Custom keybindings
(defun my/eval-buffer-stay ()
  "Eval current buffer, then keep focus on the current buffer/window."
  (interactive)
  (let ((win (selected-window))
        (buf (current-buffer)))
    (eval-buffer)
    (when (window-live-p win) (select-window win))
    (when (buffer-live-p buf) (switch-to-buffer buf))
    (message "Eval worked!")))

(global-set-key (kbd "C-c e") #'my/eval-buffer-stay)
(global-set-key (kbd "M-k") 'kill-current-buffer)
(global-set-key (kbd "M-o") 'next-multiframe-window)
(global-set-key (kbd "M-i") 'previous-multiframe-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "C-h") 'helm-show-kill-ring)
(global-set-key [C-wheel-up] 'text-scale-increase)
(global-set-key [C-wheel-down] 'text-scale-decrease)

;; Python settings
(require 'python)
(setq python-shell-interpreter "/opt/homebrew/bin/python3")
(setq python-shell-completion-native-disabled-interpreters '("python3"))

;; IDO mode
(ido-mode t)
(setq ido-separator ";")
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)

;; LaTeX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Org-babel setup
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (latex . t)))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Org-mode settings
(require 'org)
(define-key org-mode-map (kbd "S-<left>")  #'tab-bar-move-tab-backward)
(define-key org-mode-map (kbd "S-<right>") (lambda () (interactive) (tab-bar-move-tab 1)))

;; Org: start truncated (tables display correctly), toggle with C-c w
(setq org-startup-truncated t)
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))

;; Toggle between truncated (for tables) and word-wrapped (for prose)
(defun my/toggle-wrap ()
  "Toggle between truncated lines and visual-line-mode word wrap."
  (interactive)
  (if visual-line-mode
      (progn (visual-line-mode -1)
             (setq truncate-lines t)
             (message "Truncated (good for tables)"))
    (visual-line-mode 1)
    (message "Word wrap (good for prose)")))
(global-set-key (kbd "C-c w") 'my/toggle-wrap)

;; Open org links in new tab
(defun my/org-open-at-point-in-new-tab ()
  "Open org link at point in a new tab."
  (interactive)
  (let ((link (org-element-context)))
    (when (eq (org-element-type link) 'link)
      (let ((path (org-element-property :path link)))
        (tab-bar-new-tab)
        (org-open-at-point)
        (delete-other-windows)))))

(define-key org-mode-map (kbd "C-c C-o") #'my/org-open-at-point-in-new-tab)
(setq org-footnote-define-inline +1)
(setq org-startup-indented t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; Org TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w)" "MEETING-SCHEDULED(m)" "RECIPES(r)" "NOTES(o)" "|" "DONE(d)" "MEETING-DONE(D)" "CANCELED(c)" "JOURNAL(j)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
        ("NEXT" . "yellow")
        ("DONE" . "green")
        ("IN-PROGRESS" . "orange")
        ("WAITING" . "magenta")
        ("MEETING-SCHEDULED" . "orange")
        ("CANCELED" . "LightCoral")
        ("NOTES" . "gray")
        ("RECIPES" . "seashell1")
        ("MEETING-DONE" . "DeepSkyBlue")
        ("JOURNAL" . "OliveDrab")))

(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))
(setq org-log-done 'time)

;; gptel (OpenAI/LLM)
(unless (package-installed-p 'gptel)
  (package-refresh-contents)
  (package-install 'gptel))
(require 'gptel)
(require 'auth-source)
(setq auth-sources '("~/.authinfo"))
(setq gptel-api-key #'gptel-api-key-from-auth-source)
(setq gptel-model 'gpt-5.2)
(setq gptel-default-mode 'org-mode)
(setq gptel-prompt-prefix-alist
      '((org-mode . "* ")
        (markdown-mode . "### ")))
;; Remove response prefix - let system message handle formatting
(setq gptel-response-prefix-alist '((org-mode . "** ")
                                    (markdown-mode . "")))

;; Quick model switcher (C-c m)
(defun gptel-switch-model ()
  "Quickly switch between preset models."
  (interactive)
  (let* ((models '("gpt-5.2" "gpt-4o" "gpt-4o-mini" "o1" "o1-mini" "o3-mini"))
         (choice (completing-read "Model: " models nil t)))
    (setq gptel-model (intern choice))
    (message "Switched to %s" choice)))

(global-set-key (kbd "C-c m") 'gptel-switch-model)
;; Make gptel always open at bottom
(defun my/gptel-bottom ()
  "Open gptel buffer at bottom of frame as a sticky window."
  (interactive)
  (let* ((name (read-buffer "gptel buffer: " "*ChatGPT*"))
         (buf (save-window-excursion (gptel name))))
    (when buf
      (delete-other-windows)
      (split-window-below (floor (* 0.6 (window-height))))
      (other-window 1)
      (switch-to-buffer buf)
      (set-window-parameter (selected-window) 'no-delete-other-windows t)
      (local-set-key (kbd "C-x 1") 'my/toggle-side-fullscreen))))
(global-set-key (kbd "C-c g") 'my/gptel-bottom)

(defun my/toggle-side-fullscreen ()
  "Toggle between side window and full screen for current buffer."
  (interactive)
  (if (= (length (window-list)) 1)
      (my/reattach-bottom)
    (delete-other-windows)))

(defun my/reattach-bottom ()
  "Reattach current buffer to a sticky window at the bottom."
  (interactive)
  (let ((buf (current-buffer)))
    (delete-other-windows)
    (split-window-below (floor (* 0.6 (window-height))))
    (other-window 1)
    (switch-to-buffer buf)
    (set-window-parameter (selected-window) 'no-delete-other-windows t)))
(global-set-key (kbd "C-c b") 'my/reattach-bottom)
(global-set-key (kbd "C-c RET") 'gptel-send)

;; MCP (Model Context Protocol) for gptel
(add-to-list 'load-path "~/.emacs.d/site-lisp/mcp")
(require 'mcp)
(require 'mcp-hub)

;; Set GitHub token from .authinfo for MCP
(let ((auth (car (auth-source-search :host "api.github.com" :require '(:secret)))))
  (when auth
    (let ((secret (plist-get auth :secret)))
      (setenv "GITHUB_PERSONAL_ACCESS_TOKEN"
              (if (functionp secret) (funcall secret) secret)))))

;; MCP server configuration
(setq mcp-hub-servers
      '(("github" . (:command "mcp-server-github" :args ()))
        ("filesystem" . (:command "mcp-server-filesystem"
                         :args ("/Users/tedmellors")))))

;; Connect MCP tools to gptel
;; Wrapper to convert mcp-hub plists to gptel-tool structs
(defun my/mcp-tools-to-gptel ()
  "Convert MCP hub tools to gptel-tool structs."
  (when (fboundp 'mcp-hub-get-all-tool)
    (mapcar (lambda (tool-plist)
              (apply #'gptel-make-tool tool-plist))
            (mcp-hub-get-all-tool :categoryp t))))

(defun my/refresh-gptel-mcp-tools ()
  "Refresh gptel tools from MCP servers."
  (interactive)
  (setq gptel-tools (my/mcp-tools-to-gptel))
  (message "Loaded %d MCP tools into gptel" (length gptel-tools)))

;; Auto-load MCP tools after servers start
(with-eval-after-load 'mcp-hub
  (advice-add 'mcp-hub-start-all-server :after
              (lambda (&rest _)
                (run-with-timer 2 nil #'my/refresh-gptel-mcp-tools))))

;; Start MCP servers on Emacs startup and load tools into gptel
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 1 nil #'mcp-hub-start-all-server)
            (run-with-timer 4 nil #'my/refresh-gptel-mcp-tools)))

;; claude-code-ide dependencies
(dolist (pkg '(vterm websocket web-server))
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; Let Emacs handle M-] in vterm for tab switching
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-]") nil))

;; claude-code-ide
(add-to-list 'load-path "~/.emacs.d/site-lisp/claude-code-ide")
(require 'claude-code-ide)
(setq claude-code-ide-terminal-backend 'vterm)
(setq claude-code-ide-window-side 'bottom)
(setq claude-code-ide-window-height 20)
(setq claude-code-ide-debug t)
(setq claude-code-ide-use-ide-diff nil)  ; Prevent ediff from opening in new tab on edits
(global-set-key (kbd "C-c C-'") 'claude-code-ide-menu)

;; SpecFlow - spec-driven development workflow
(add-to-list 'load-path "~/.emacs.d/specflow/src")
(require 'specflow)

;; Custom gptel prompt for Claude prompt engineering
(with-eval-after-load 'gptel
  (add-to-list 'gptel-directives
               '(claude-prompt . "You write *one ready-to-send prompt for Claude* to implement a requested change in the GIDEON repo.

*Repo context Claude must follow*
- High-level context: =docs/overview.org=
- Project-wide tasks: =./todo.org=
- Module workflow/rules: =modules/<MODULE>/CLAUDE.md=
- Module spec + tasks: =modules/<MODULE>/spec.org= and =modules/<MODULE>/todo.org=

*Rules you must enforce in the Claude prompt*
- Work on a feature branch (never directly on =main=).
- Stay within one module unless the user explicitly approves otherwise.
- Read the files above before coding; do not guess paths or behavior.
- Keep diffs minimal; run relevant tests.
- Update TODOs at the end: both =./todo.org= and =modules/<MODULE>/todo.org=.
- At completion, Claude must report: summary, files changed, tests run/results, and proposed next step.

*Your output format (always)*
Return exactly one section:

*Prompt for Claude (copy/paste)*
- Context (module, goal)
- Pre-work (files to read)
- Tasks (spec → implementation → tests → docs if needed)
- Stop point (where Claude must stop and wait for approval)
- Completion report requirements

*When information is missing*
Ask up to 5 clarifying questions instead of guessing (e.g., module name, desired stop point, acceptance criteria).")))

;; Open todo.org on startup
(find-file "/Users/tedmellors/Documents/Documents_Ted_Mac_air/TRM/00_Admin/MASTER_TASKS.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
