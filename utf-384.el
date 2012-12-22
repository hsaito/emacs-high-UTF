;; UTF-384 support for Emacs
;;
;; Based on the code by:
;; http://albinina.sakura.ne.jp/
;;
;; Modifications:
;; - Changed name of each coding system so it is consistent with the rest.
;; 
;; Modified by Hideki Saito <hidekis@gmail.com>
;; 
;; Installation:
;; (require 'utf-384)
;;

;;
;; With BOM
;;

(defun utf-384-be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384-be-unix-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384-be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (insert #xFEFF)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384-be-dos-post-read-conversion (len)
  (if (zerop (following-char))
      (progn
        (forward-char)
        (if (= (following-char) #xFEFF)
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-384-le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384-le-unix-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384-le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (insert #xFEFF)
  (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384-le-dos-post-read-conversion (len)
  (if (= (following-char) #xFEFF)
      (progn
        (forward-char)
        (if (zerop (following-char))
            (progn
              (backward-char)
              (delete-char 2)))))
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; Without BOM

(defun utf-384be-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384be-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384be-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
          (insert #x000D)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert hi)
          (insert lo))
      (progn
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)
        (forward-char)))))

(defun utf-384be-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((hi (following-char))
          lo c)
      (delete-char 1)
      (setq lo (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

(defun utf-384le-unix-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384le-unix-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (insert c)))
  (goto-char 0)
  len)

(defun utf-384le-dos-pre-write-conversion (from to)
  (set-buffer-multibyte t)
  (goto-char 0)
  (while (not (eobp))
    (if (eolp)
        (progn
          (insert #x000D)
          (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))
    (if (> (following-char) #xFFFF)
        (let* ((c (following-char))
               (hi (lsh (logand c #x1F0000) -16))
               (lo (logand c #xFFFF)))
          (delete-char 1)
          (insert lo)
          (insert hi))
      (progn
        (forward-char)
        (insert #x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000#x0000)))))

(defun utf-384le-dos-post-read-conversion (len)
  (while (not (eobp))
    (let ((lo (following-char))
          hi c)
      (delete-char 1)
      (setq hi (following-char))
      (delete-char 1)
      (setq c (logior (lsh hi 16) lo))
      (if (/= c #x000D)
          (insert c))))
  (goto-char 0)
  len)

;; With BOM

(define-coding-system 'utf-384be-with-signature-unix
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-unix-pre-write-conversion
  :post-read-conversion 'utf-384-be-unix-post-read-conversion)

(define-coding-system 'utf-384be-with-signature-dos
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-dos-pre-write-conversion
  :post-read-conversion 'utf-384-be-dos-post-read-conversion)

(define-coding-system 'utf-384be-with-signature-mac
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-be-unix-pre-write-conversion
  :post-read-conversion 'utf-384-be-unix-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-unix
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-unix-pre-write-conversion
  :post-read-conversion 'utf-384-le-unix-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-dos
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-dos-pre-write-conversion
  :post-read-conversion 'utf-384-le-dos-post-read-conversion)

(define-coding-system 'utf-384le-with-signature-mac
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384-le-unix-pre-write-conversion
  :post-read-conversion 'utf-384-le-unix-post-read-conversion)

;; Without BOM

(define-coding-system 'utf-384be-unix
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-unix-pre-write-conversion
  :post-read-conversion 'utf-384be-unix-post-read-conversion)

(define-coding-system 'utf-384be-dos
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-dos-pre-write-conversion
  :post-read-conversion 'utf-384be-dos-post-read-conversion)

(define-coding-system 'utf-384be-mac
  "UTF-384 (big endian)"
  :coding-type 'utf-16
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384be-unix-pre-write-conversion
  :post-read-conversion 'utf-384be-unix-post-read-conversion)

(define-coding-system 'utf-384le-unix
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-unix-pre-write-conversion
  :post-read-conversion 'utf-384le-unix-post-read-conversion)

(define-coding-system 'utf-384le-dos
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'unix
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-dos-pre-write-conversion
  :post-read-conversion 'utf-384le-dos-post-read-conversion)

(define-coding-system 'utf-384le-mac
  "UTF-384 (little endian)"
  :coding-type 'utf-16
  :endian 'little
  :mnemonic ?U
  :eol-type 'mac
  :charset-list '(unicode)
  :pre-write-conversion 'utf-384le-unix-pre-write-conversion
  :post-read-conversion 'utf-384le-unix-post-read-conversion)

(provide 'utf-384)
