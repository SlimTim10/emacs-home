(progn
  (find-file "todo.org") ; Top left window
  (org-cycle)
  (split-window-horizontally)
  (window-resize nil -100 t) ; Horizonal size
  (split-window-vertically)
  (other-window 1)
  (setq magit-status-buffer-switch-function 'switch-to-buffer) ; Open magit-status in same window
  (magit-status ".") ; Bottom left window
  (other-window 1)
  (find-file "playback.c") ; Middle window
  (goto-line 537)
  (recenter-top-bottom)
  (split-window-horizontally)
  (window-resize nil 0 t) ; Horizontal resize
  (other-window 1)
  (find-file "lcd.c") ; Right window
  (other-window -1) ; Focus middle window
)
