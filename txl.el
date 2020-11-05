
(require 'request)
(require 'guess-language)

(defgroup txl nil
  "Use online machine translation services."
  :group 'text)

(defcustom txl-api 'deepl
  "API used for retrieving translations.  Currently only 'deepl
  is supported."
  :type 'symbol)
  
(defvar txl-deepl-api-url "https://api.deepl.com/v2/translate"
  "URL of the translation API.")

(defcustom txl-api-authentication-key ""
  "The authentication key used to access the translation API."
  :type 'string)

(defun txl-deepl-translate-string (text target-lang &rest more-target-langs)
  (message "Contacting DeepL ...")
  (let* ((response (request
                     txl-deepl-api-url
                     :type "GET"
                     :encoding 'utf-8
                     :sync t
                     :parser 'buffer-string
                     :params `(("auth_key"    . ,txl-api-authentication-key)
                               ("text"        . ,text)
                               ("target_lang" . ,target-lang))))
         (response (json-read-from-string (request-response-data response)))
         (translations (cdr (assoc 'translations response)))
         (translation (cdr (assoc 'text (aref translations 0)))))
    (if more-target-langs
        (apply 'txl-deepl-translate-string translation (car more-target-langs) (cdr more-target-langs))
      translation)))

(defun txl-translate-string (text target-lang &rest more-target-langs)
  (pcase txl-api
    ('deepl (apply 'txl-deepl-translate-string text target-lang more-target-langs))
    (_      (error "No such translation API.  Please check customization variable `txl-api'."))))

;; (txl-translate-string "This a test is, said Yoda." "DE")
;; (txl-translate-string "This a test is, said Yoda." "DE" "EN")
         
(defun txl-translate (target-lang &rest more-target-langs)
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point))))
         (text (buffer-substring-no-properties beginning end)))
    (apply 'txl-translate-string text target-lang more-target-langs)))

;; This a test is, said Yoda.

(defun txl-guess-language ()
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point)))))
    (guess-language-region beginning end)))

(defun txl-replace-with-string (string)
  (let* ((beginning (if (region-active-p) (region-beginning) (save-excursion (guess-language-backward-paragraph) (point))))
         (end       (if (region-active-p) (region-end)       (save-excursion (guess-language-forward-paragraph) (point)))))
    (delete-region beginning end)
    (insert string)))

(defun txl-translate-to-other-language ()
  (interactive)
  (message "%s"
   (pcase (txl-guess-language)
     ('de (txl-translate "EN"))
     ('en (txl-translate "DE")))))

(defun txl-translate-roundtrip ()
  (interactive)
  (let* ((source-lang (txl-guess-language))
         (route (if (eq source-lang 'de) '("EN" "DE") '("DE" "EN")))
         (translation (apply 'txl-translate route)))
  (message "%s" translation)))

;; This is a test in the English language.

;; Dies ist ein Test in deutscher Sprache.
  
(defun txl-replace-with-other-language ()
  (interactive)
  (let* ((source-lang (txl-guess-language))
         (route (if (eq source-lang 'de) "EN" "DE"))
         (translation (txl-translate route)))
    (txl-replace-with-string translation)))

(defun txl-replace-with-roundtrip ()
  (interactive)
  (let* ((source-lang (txl-guess-language))
         (route (if (eq source-lang 'de) '("EN" "DE") '("DE" "EN")))
         (translation (apply 'txl-translate route)))
    (txl-replace-with-string translation)))

;; For testing:  This a new test is, said Yoda.

(provide 'txl)
