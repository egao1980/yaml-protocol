(in-package #:yaml-protocol)

;;; Native YAML 1.2.2 event parser. No FFI. Events are the parse product.
;;; Functions are spec productions (https://yaml.org/spec/1.2.2/). [n] is
;;; the 1.2.2 production number. Structure follows spec chapters 5–9.
;;;
;;; Parameters: indent n, context via :flow / :key
;;;   :flow nil + :key nil        → block-in / block-out
;;;   :key :implicit              → block-key / flow-key (one-line)
;;;   :flow t                     → flow-in / flow-out
;;; Chomp t ∈ {strip, clip, keep}.
;;; Prefixes: c- indicator, s- space, ns- non-space, b- break, l- line,
;;; nb- non-break, e- empty.

(defstruct (ys (:constructor %make-ys))
  (text "" :type string)
  (pos 0 :type fixnum)
  (len 0 :type fixnum)
  (events nil)
  (tag-handles nil)
  (parent-indent -1 :type fixnum)
  (yaml-directive-p nil))

(defun make-ys (text)
  (%make-ys :text text :pos 0 :len (length text)
            :events (make-array 32 :adjustable t :fill-pointer 0)
            :tag-handles (make-hash-table :test #'equal)))

(defun reset-tag-handles (ys)
  (let ((ht (ys-tag-handles ys)))
    (clrhash ht)
    (setf (ys-yaml-directive-p ys) nil
          (gethash "!" ht) "!"
          (gethash "!!" ht) "tag:yaml.org,2002:")))

(defun ys-eof-p (ys)
  (>= (ys-pos ys) (ys-len ys)))

(defun ys-peek (ys &optional (n 0))
  (let ((i (+ (ys-pos ys) n)))
    (when (< i (ys-len ys))
      (char (ys-text ys) i))))

(defun ys-next (ys)
  (when (< (ys-pos ys) (ys-len ys))
    (prog1 (char (ys-text ys) (ys-pos ys))
      (incf (ys-pos ys)))))

(defun ys-column (ys)
  (let ((text (ys-text ys))
        (pos (ys-pos ys)))
    (loop for i from (1- pos) downto 0
          when (char= (char text i) #\Newline)
            return (- pos i 1)
          finally (return pos))))

(defun emit (ys kind &key (implicit t) flow-p anchor tag style value)
  (vector-push-extend
   (make-yaml-event :kind kind :implicit implicit :flow-p flow-p
                    :anchor (and anchor (plusp (length anchor)) anchor)
                    :tag tag :style (or style :plain) :value value)
   (ys-events ys)))

(defmacro with-ys-checkpoint ((ys) &body body)
  "Restore pos and event fill-pointer after a speculative parse."
  (let ((pos (gensym "POS"))
        (fp (gensym "FP"))
        (s (gensym "YS")))
    `(let* ((,s ,ys)
            (,pos (ys-pos ,s))
            (,fp (fill-pointer (ys-events ,s))))
       (unwind-protect (progn ,@body)
         (setf (ys-pos ,s) ,pos
               (fill-pointer (ys-events ,s)) ,fp)))))

(defun fail-parse (ys fmt &rest args)
  (error 'yaml-parse-error
         :message (format nil "~A (pos ~D)"
                          (apply #'format nil fmt args)
                          (ys-pos ys))))

;;;; ch. 5 Character Set

(defun s-space-p (c)
  "[31] s-space ::= #x20"
  (eql c #\Space))

(defun s-tab-p (c)
  "[32] s-tab ::= #x9"
  (eql c #\Tab))

(defun s-white-p (c)
  "[33] s-white ::= s-space | s-tab"
  (or (eql c #\Space) (eql c #\Tab)))

(defun b-break-p (c)
  "[28] b-break — CR | LF; CRLF is consumed in [29] b-as-line-feed"
  (or (eql c #\Newline) (eql c #\Return)))

(defun ns-char-p (c)
  "[34] ns-char ::= nb-char - s-white"
  (and c (not (s-white-p c)) (not (b-break-p c))))

(defun ns-dec-digit-p (c)
  "[35] ns-dec-digit"
  (and c (char<= #\0 c #\9)))

(defun ns-word-char-p (c)
  "[38] ns-word-char ::= ns-dec-digit | ns-ascii-letter | '-'"
  (and c (or (alphanumericp c) (char= c #\-))))

(defun c-indicator-p (c)
  "[22] c-indicator"
  (member c '(#\- #\? #\: #\, #\[ #\] #\{ #\} #\# #\& #\* #\!
              #\| #\> #\' #\" #\% #\@ #\`)))

(defun c-flow-indicator-p (c)
  "[23] c-flow-indicator ::= ',' | '[' | ']' | '{' | '}'"
  (member c '(#\, #\[ #\] #\{ #\})))

;;;; ch. 6 Structural Characters — indentation, separation, comments

(defun s-indent (ys)
  "[63] s-indent(n) — spaces only. Consumes all leading s-space on the line."
  (loop while (s-space-p (ys-peek ys)) do (ys-next ys)))

(defun s-separate-in-line (ys)
  "[66] s-separate-in-line ::= s-white+ | /* start of line */"
  (loop while (s-white-p (ys-peek ys)) do (ys-next ys)))

(defun b-as-line-feed (ys)
  "[29] b-as-line-feed. Consumes CRLF / CR / LF. Returns T if a break was eaten."
  (cond
    ((eql (ys-peek ys) #\Return)
     (ys-next ys)
     (when (eql (ys-peek ys) #\Newline)
       (ys-next ys))
     t)
    ((eql (ys-peek ys) #\Newline)
     (ys-next ys)
     t)
    (t nil)))

(defun c-nb-comment-text-p (ys)
  "[75] c-nb-comment-text starts with `#` only after s-white or b-char (9JBA)."
  (and (eql (ys-peek ys) #\#)
       (or (zerop (ys-pos ys))
           (let ((prev (char (ys-text ys) (1- (ys-pos ys)))))
             (or (s-white-p prev) (b-break-p prev))))))

(defun c-nb-comment-text (ys)
  "[75] c-nb-comment-text ::= '#' nb-char*"
  (when (eql (ys-peek ys) #\#)
    (loop until (or (ys-eof-p ys) (b-break-p (ys-peek ys)))
          do (ys-next ys))))

(defun s-b-comment (ys)
  "[77] s-b-comment — `#` only after s-separate-in-line."
  (when (s-white-p (ys-peek ys))
    (s-separate-in-line ys)
    (when (eql (ys-peek ys) #\#)
      (c-nb-comment-text ys)))
  (or (ys-eof-p ys) (b-break-p (ys-peek ys)) (null (ys-peek ys))))

(defun s-l-comments (ys)
  "[79] s-l-comments. Also used where [80] s-separate allows a break.
   A tab at column 0 is s-indent (illegal) unless the line is a flow node (6CA3).
   A tab after spaces on the same line is s-separate-in-line (DK95)."
  (loop
    (when (and (zerop (ys-column ys))
               (eql (ys-peek ys) #\Tab))
      (let ((saved (ys-pos ys)))
        (s-separate-in-line ys)
        (let ((c (ys-peek ys)))
          (unless (or (null c) (b-break-p c)
                      (member c '(#\[ #\] #\{ #\}))
                      (c-nb-comment-text-p ys))
            (setf (ys-pos ys) saved)
            (fail-parse ys "tab cannot be used as indentation")))
        (setf (ys-pos ys) saved)))
    (s-separate-in-line ys)
    (cond
      ((c-nb-comment-text-p ys) (c-nb-comment-text ys))
      ((b-as-line-feed ys))
      (t (return)))))

(defun at-bol-p (ys)
  (or (zerop (ys-pos ys))
      (let ((c (char (ys-text ys) (1- (ys-pos ys)))))
        (or (char= c #\Newline) (char= c #\Return)))))

(defun c-forbidden-p (ys)
  "[206] c-forbidden ::= <start-of-line> (--- | ...) (s-white | b-char | EOF)"
  (and (at-bol-p ys)
       (or (at-marker-p ys "---")
           (at-marker-p ys "..."))))

(defun at-marker-p (ys marker)
  (and (at-bol-p ys)
       (let ((n (length marker)))
         (loop for i below n
               unless (eql (ys-peek ys i) (char marker i))
                 return nil
               finally (let ((c (ys-peek ys n)))
                         (return (or (null c) (s-white-p c) (b-break-p c)
                                     (eql c #\#))))))))

(defun consume-marker (ys marker)
  (dotimes (i (length marker))
    (ys-next ys))
  (s-separate-in-line ys)
  (when (c-nb-comment-text-p ys)
    (c-nb-comment-text ys)))

(defun consume-end-marker (ys)
  "[205] l-document-suffix. Trailing tokens on the same line are invalid (3HFZ)."
  (consume-marker ys "...")
  (unless (or (ys-eof-p ys) (b-break-p (ys-peek ys)))
    (fail-parse ys "invalid content after document end marker")))

(defun ns-plain-safe-p (c flow)
  "[129] ns-plain-safe-in ::= ns-char - c-flow-indicator
   [128] ns-plain-safe-out ::= ns-char"
  (and (ns-char-p c)
       (not (and flow (c-flow-indicator-p c)))))

(defun ns-plain-first-p (ys flow)
  "[126] ns-plain-first(c) — `-` `?` `:` only if followed by ns-plain-safe."
  (let ((c (ys-peek ys)))
    (cond
      ((null c) nil)
      ((member c '(#\- #\? #\:))
       (ns-plain-safe-p (ys-peek ys 1) flow))
      ((c-indicator-p c) nil)
      (t (ns-char-p c)))))

;;;; ch. 6.8–6.9 Node Properties / Alias

(defun c-byte-order-mark (ys)
  "[3] c-byte-order-mark ::= #xFEFF"
  (when (eql (ys-peek ys) #\ufeff)
    (ys-next ys)))

(defun parse-hex (ys n)
  (let ((acc 0))
    (dotimes (i n acc)
      (let ((c (ys-next ys)))
        (unless (hex-digit-p c)
          (fail-parse ys "bad hex escape"))
        (setf acc (+ (* acc 16)
                     (digit-char-p c 16)))))))

(defun percent-decode (s)
  (with-output-to-string (o)
    (loop with i = 0
          with n = (length s)
          while (< i n)
          do (let ((c (char s i)))
               (if (and (char= c #\%) (<= (+ i 3) n)
                        (hex-digit-p (char s (1+ i)))
                        (hex-digit-p (char s (+ i 2))))
                   (progn
                     (write-char (code-char
                                  (+ (* 16 (digit-char-p (char s (1+ i)) 16))
                                     (digit-char-p (char s (+ i 2)) 16)))
                                 o)
                     (incf i 3))
                   (progn
                     (write-char c o)
                     (incf i)))))))

(defun resolve-tag (ys raw)
  "Resolve a tag token (!foo, !!str, !e!name, !<uri>, !) against handles."
  (cond
    ((or (null raw) (string= raw "")) nil)
    ((and (>= (length raw) 2)
          (char= (char raw 0) #\<)
          (char= (char raw (1- (length raw))) #\>))
     (subseq raw 1 (1- (length raw))))
    ((string= raw "!") "!")
    ((and (>= (length raw) 2) (char= (char raw 0) #\!)
          (char= (char raw 1) #\!))
     (concatenate 'string (gethash "!!" (ys-tag-handles ys) "tag:yaml.org,2002:")
                  (percent-decode (subseq raw 2))))
    ((char= (char raw 0) #\!)
     (let* ((rest (subseq raw 1))
            (bang (position #\! rest)))
       (if bang
           (let* ((handle (concatenate 'string "!" (subseq rest 0 (1+ bang))))
                  (suffix (percent-decode (subseq rest (1+ bang))))
                  (prefix (gethash handle (ys-tag-handles ys))))
             (unless prefix
               (fail-parse ys "undefined tag handle ~A" handle))
             (concatenate 'string prefix suffix))
           (let ((prefix (gethash "!" (ys-tag-handles ys) "!")))
             (concatenate 'string prefix (percent-decode rest))))))
    (t raw)))

(defun ns-tag-char-p (c)
  "[40] ns-tag-char — ns-uri-char minus '!' / c-flow-indicator. `:` allowed."
  (and c (not (s-white-p c)) (not (b-break-p c))
       (not (member c '(#\[ #\] #\{ #\} #\,)))))

(defun c-verbatim-tag (ys)
  "[98] c-verbatim-tag ::= '!' '<' ns-uri-char+ '>'
   First `!` already consumed."
  (ys-next ys)                          ; <
  (let ((uri (with-output-to-string (o)
               (loop for c = (ys-peek ys)
                     until (or (null c) (char= c #\>))
                     do (write-char (ys-next ys) o)))))
    (unless (eql (ys-next ys) #\>)
      (fail-parse ys "unterminated verbatim tag"))
    (concatenate 'string "<" uri ">")))

(defun c-ns-shorthand-tag (ys)
  "[99] c-ns-shorthand-tag ::= c-tag-handle ns-tag-char+
   First `!` already consumed. `!!` is [91] c-secondary-tag-handle."
  (cond
    ((eql (ys-peek ys) #\!)
     (ys-next ys)
     (let ((name (with-output-to-string (o)
                   (loop for c = (ys-peek ys)
                         while (ns-tag-char-p c)
                         do (write-char (ys-next ys) o)))))
       (concatenate 'string "!!" name)))
    (t
     (let ((name (with-output-to-string (o)
                   (loop for c = (ys-peek ys)
                         while (ns-tag-char-p c)
                         do (write-char (ys-next ys) o)))))
       (concatenate 'string "!" name)))))

(defun c-ns-tag-property (ys)
  "[97] c-ns-tag-property ::= c-verbatim-tag | c-ns-shorthand-tag | c-non-specific-tag
   First `!` already consumed. [100] c-non-specific-tag is `!` alone."
  (cond
    ((eql (ys-peek ys) #\<) (c-verbatim-tag ys))
    (t (c-ns-shorthand-tag ys))))

(defun ns-anchor-char-p (c)
  "[102] ns-anchor-char ::= ns-char - c-flow-indicator. `:` is allowed (2SXE)."
  (and c (not (s-white-p c)) (not (b-break-p c))
       (not (member c '(#\[ #\] #\{ #\} #\,)))))

(defun ns-anchor-name (ys)
  "[103] ns-anchor-name ::= ns-anchor-char+"
  (let ((name (with-output-to-string (o)
                (loop for c = (ys-peek ys)
                      while (ns-anchor-char-p c)
                      do (write-char (ys-next ys) o)))))
    (when (zerop (length name))
      (fail-parse ys "empty anchor/alias"))
    name))

(defun c-ns-anchor-property (ys)
  "[101] c-ns-anchor-property ::= '&' ns-anchor-name
   `&` already consumed."
  (ns-anchor-name ys))

(defun c-ns-alias-node (ys)
  "[104] c-ns-alias-node ::= '*' ns-anchor-name"
  (ys-next ys)
  (emit ys :alias :value (ns-anchor-name ys)))

(defun c-ns-properties (ys &key (indent -1))
  "[96] c-ns-properties(n,c) — optional &anchor / !tag (either order, at most one).
   May continue after a line break via s-separate(n+1) (9KAX, BU8L) unless the
   next `&`/`!` starts a mapping key (4JVG). Continuation must be indented
   past n (H7J7)."
  (let ((anchor nil)
        (tag nil)
        (any nil))
    (loop
      (s-separate-in-line ys)
      (cond
        ((eql (ys-peek ys) #\&)
         (when anchor
           (fail-parse ys "duplicate anchor"))
         (ys-next ys)
         (setf anchor (c-ns-anchor-property ys)
               any t))
        ((eql (ys-peek ys) #\!)
         (when tag
           (fail-parse ys "duplicate tag"))
         (ys-next ys)
         (setf tag (resolve-tag ys (c-ns-tag-property ys))
               any t))
        ((and any
              (or (b-break-p (ys-peek ys))
                  (c-nb-comment-text-p ys)
                  (ys-eof-p ys)))
         (let ((saved (ys-pos ys)))
           (when (c-nb-comment-text-p ys)
             (c-nb-comment-text ys))
           (b-as-line-feed ys)
           (s-indent ys)
           (unless (and (or (eql (ys-peek ys) #\&) (eql (ys-peek ys) #\!))
                        (> (ys-column ys) indent)
                        (not (ns-s-block-map-implicit-key-p ys))
                        (not (c-sequence-entry-p ys)))
             (setf (ys-pos ys) saved)
             (return))))
        (t (return))))
    (values anchor tag)))

(defun s-separate-after-properties (ys)
  "[80]/[81] s-separate after [96] c-ns-properties.
   Returns T if a line break was consumed.
   Same-line properties belong to the next node (often a mapping key);
   properties + break belong to the following collection (2SXE vs 26DV)."
  (s-separate-in-line ys)
  (when (c-nb-comment-text-p ys)
    (c-nb-comment-text ys))
  (cond
    ((or (b-break-p (ys-peek ys)) (ys-eof-p ys))
     (b-as-line-feed ys)
     (loop
       (s-indent ys)
       (cond
         ((eql (ys-peek ys) #\#)
          (c-nb-comment-text ys)
          (b-as-line-feed ys))
         ((b-break-p (ys-peek ys))
          (b-as-line-feed ys))
         (t (return))))
     t)
    (t nil)))

;;;; ch. 7 Flow Styles

(defun c-ns-esc-char (ys)
  "[62] c-ns-esc-char. `\\` already consumed. Returns a char or :escaped-break
   ([112] s-double-escaped)."
  (let ((e (ys-next ys)))
    (when (null e)
      (fail-parse ys "unterminated escape"))
    (cond
      ((char= e #\0) #\Nul)
      ((char= e #\a) #\Bel)
      ((char= e #\b) #\Backspace)
      ((char= e #\t) #\Tab)
      ((char= e #\n) #\Newline)
      ((char= e #\v) #\Vt)
      ((char= e #\f) #\Page)
      ((char= e #\r) #\Return)
      ((char= e #\e) #\Esc)
      ((char= e #\") #\")
      ((char= e #\/) #\/)
      ((char= e #\\) #\\)
      ((char= e #\N) (code-char #x85))
      ((char= e #\_) (code-char #xa0))
      ((char= e #\L) (code-char #x2028))
      ((char= e #\P) (code-char #x2029))
      ((char= e #\x) (code-char (parse-hex ys 2)))
      ((char= e #\u) (code-char (parse-hex ys 4)))
      ((char= e #\U) (code-char (parse-hex ys 8)))
      ((or (char= e #\Newline) (char= e #\Return))
       (when (and (char= e #\Return) (eql (ys-peek ys) #\Newline))
         (ys-next ys))
       :escaped-break)
      ((s-white-p e) e)                  ; `\` + tab (3RLN/01), not `\.` (55WF)
      (t (fail-parse ys "unknown escape \\~A" e)))))

(defun s-flow-folded (ys chars indent)
  "[74] s-flow-folded(n). Trailing s-space on the line is discarded; an
   escaped tab already in CHARS is ns-esc-char, not line s-white (DE56)."
  (loop while (and chars (s-space-p (car chars)))
        do (pop chars))
  (b-as-line-feed ys)
  (let ((empty 0))
    (loop
      (s-indent ys)
      (when (and (eql (ys-peek ys) #\Tab)
                 (>= indent 0)
                 (< (ys-column ys) indent))
        (fail-parse ys "tab used as indentation"))
      (s-separate-in-line ys)
      (cond
        ((b-break-p (ys-peek ys))
         (b-as-line-feed ys)
         (incf empty))
        (t (return))))
    (s-separate-in-line ys)
    (let ((c (ys-peek ys)))
      (when (and (not (null c))
                 (not (b-break-p c))
                 (>= indent 0)
                 (< (ys-column ys) indent))
        (fail-parse ys "wrong indent in flow/quoted")))
    (if (plusp empty)
        (dotimes (i empty chars)
          (push #\Newline chars))
        (progn (push #\Space chars) chars))))

(defun c-double-quoted (ys &key (indent -1) single-line)
  "[109] c-double-quoted(n,c) / [110] nb-double-text.
   Trailing s-white before a break is [66] s-separate-in-line (DE56/04);
   escaped tabs are already [62] ns-esc-char and stay (DE56/00)."
  (unless (eql (ys-next ys) #\")
    (fail-parse ys "expected \""))
  (let ((chars '())
        (pending '()))
    (flet ((flush-pending ()
             (setf chars (append pending chars)
                   pending nil)))
      (loop
        (let ((c (ys-peek ys)))
          (cond
            ((null c) (fail-parse ys "unterminated double-quoted string"))
            ((char= c #\")
             (flush-pending)
             (ys-next ys)
             (return (coerce (nreverse chars) 'string)))
            ((c-forbidden-p ys)
             (fail-parse ys "document marker inside double-quoted scalar"))
            ((char= c #\\)
             (flush-pending)
             (ys-next ys)
             (let ((x (c-ns-esc-char ys)))
               (if (eq x :escaped-break)
                   ;; [112] s-double-escaped → [69] s-flow-line-prefix = s-indent.
                   ;; Leading spaces are prefix (565N); `\`+space after that is content (NP9H).
                   (loop while (s-space-p (ys-peek ys))
                         do (ys-next ys))
                   (push x chars))))
            ((b-break-p c)
             (when single-line
               (fail-parse ys "multiline implicit key"))
             (setf pending nil)
             (setf chars (s-flow-folded ys chars indent)))
            ((s-white-p c)
             (push (ys-next ys) pending))
            (t
             (flush-pending)
             (push (ys-next ys) chars))))))))

(defun c-single-quoted (ys &key (indent -1) single-line)
  "[120] c-single-quoted(n,c) / [121] nb-single-text. [117] c-quoted-quote = ''."
  (unless (eql (ys-next ys) #\')
    (fail-parse ys "expected '"))
  (let ((chars '()))
    (loop
      (let ((c (ys-peek ys)))
        (cond
          ((null c) (fail-parse ys "unterminated single-quoted string"))
          ((char= c #\')
           (ys-next ys)
           (if (eql (ys-peek ys) #\')
               (push (ys-next ys) chars)
               (return (coerce (nreverse chars) 'string))))
          ((c-forbidden-p ys)
           (fail-parse ys "document marker inside single-quoted scalar"))
          ((b-break-p c)
           (when single-line
             (fail-parse ys "multiline implicit key"))
           (setf chars (s-flow-folded ys chars indent)))
          (t
           (push (ys-next ys) chars)))))))

(defun plain-stop-p (c flow)
  "[129]/[132] — flow indicators end ns-plain. Breaks are [133] vs [135]."
  (or (null c)
      (and flow (c-flow-indicator-p c))))

(defun colon-ends-plain-p (ys flow)
  (let ((n (ys-peek ys 1)))
    (or (null n) (s-white-p n) (b-break-p n)
        (and flow (c-flow-indicator-p n)))))

(defun ns-plain (ys &key flow (indent -1) single-line)
  "[133] ns-plain-one-line (block-key / flow-key) vs
   [135] ns-plain-multi-line (flow-in / flow-out / block-in).
   After a break, s-white (including tab) is s-line-prefix, not content (HS5T).
   `-` starts a new block seq only at col <= n (AB8U)."
  (let ((chars '()))
    (loop
      (let ((c (ys-peek ys)))
        (cond
          ((and (b-break-p c) single-line)
           (return))
          ((b-break-p c)
           (when (c-forbidden-p ys)
             (return))
           (let ((saved (ys-pos ys))
                 (saved-chars chars))
             (loop while (and chars (s-white-p (car chars)))
                   do (pop chars))
             (b-as-line-feed ys)
             (let ((empty 0)
                   (ok t))
               (loop
                 (s-separate-in-line ys)
                 (cond
                   ((b-break-p (ys-peek ys))
                    (b-as-line-feed ys)
                    (incf empty))
                   (t (return))))
               (s-separate-in-line ys)
               (let ((col (ys-column ys))
                     (n (ys-peek ys)))
                 (when (or (null n)
                           (if flow (< col indent) (<= col indent))
                           (c-forbidden-p ys)
                           (c-nb-comment-text-p ys)
                           (and flow (c-flow-indicator-p n))
                           (and (eql n #\-)
                                (<= col indent)
                                (let ((x (ys-peek ys 1)))
                                  (or (null x) (s-white-p x) (b-break-p x))))
                           (and (eql n #\?)
                                (<= col indent)
                                (let ((x (ys-peek ys 1)))
                                  (or (null x) (s-white-p x) (b-break-p x))))
                           (and (eql n #\:)
                                (<= col indent)
                                (let ((x (ys-peek ys 1)))
                                  (or (null x) (s-white-p x) (b-break-p x)))))
                   (setf (ys-pos ys) saved
                         chars saved-chars
                         ok nil)))
               (if ok
                   (if (plusp empty)
                       (dotimes (i empty) (push #\Newline chars))
                       (push #\Space chars))
                   (return)))))
          ((plain-stop-p c flow)
           (return))
          ((and (eql c #\:) (colon-ends-plain-p ys flow))
           (return))
          ((and (s-white-p c) (eql (ys-peek ys 1) #\#))
           (return))
          ((eql c #\#)
           (if (and chars (s-white-p (car chars)))
               (return)
               (push (ys-next ys) chars)))
          (t
           (push (ys-next ys) chars)))))
    (loop while (and chars (s-white-p (car chars)))
          do (pop chars))
    (coerce (nreverse chars) 'string)))

;;;; ch. 8 Block Styles

(defun c-b-block-header (ys)
  "[162] c-b-block-header — chomp/indent then s-b-comment.
   `#` without s-white is invalid (X4QW)."
  (let ((chomp :clip)
        (explicit-indent nil))
    (loop
      (let ((c (ys-peek ys)))
        (cond
          ((eql c #\-) (ys-next ys) (setf chomp :strip))
          ((eql c #\+) (ys-next ys) (setf chomp :keep))
          ((and c (char<= #\1 c #\9))
           (setf explicit-indent (digit-char-p (ys-next ys))))
          (t (return)))))
    (unless (s-b-comment ys)
      (fail-parse ys "trailing junk after block scalar header"))
    (let ((header-break (b-as-line-feed ys)))
      (values chomp explicit-indent header-break))))

(defun l+block-scalar (ys &key (indent -1))
  "[170]/[174] c-l+literal / c-l+folded. Parent n may be -1 (l-bare-document).
   Tab after s-indent is nb-char content (96NN), not s-indent.
   `#` at content-indent is content (DK3J). [206] c-forbidden ends the scalar."
  (let ((kind (ys-next ys)))
    (unless (or (char= kind #\|) (char= kind #\>))
      (fail-parse ys "expected block scalar"))
    (multiple-value-bind (chomp explicit-indent header-break)
        (c-b-block-header ys)
      (let* ((parent indent)
             (content-indent (and explicit-indent (+ (max parent 0) explicit-indent)))
             (max-empty 0)
             (lines '()))
        (loop
          (when (ys-eof-p ys)
            (return))
          (when (c-forbidden-p ys)
            (return))
          (let ((col 0))
            (loop while (s-space-p (ys-peek ys))
                  do (ys-next ys) (incf col))
            (cond
              ((and (eql (ys-peek ys) #\Tab)
                    (if content-indent
                        (< col content-indent)
                        (<= col parent)))
               (fail-parse ys "tab used as indentation"))
              ((or (ys-eof-p ys) (b-break-p (ys-peek ys)))
               (cond
                 ((and content-indent (> col content-indent))
                  (push (make-string (- col content-indent) :initial-element #\Space)
                        lines))
                 (t
                  (setf max-empty (max max-empty col))
                  (push "" lines)))
               (b-as-line-feed ys))
              ((and content-indent (< col content-indent)
                    (eql (ys-peek ys) #\#))
               (return))
              (t
               (unless content-indent
                 (when (<= col parent)
                   (decf (ys-pos ys) col)
                   (return))
                 (when (and (plusp max-empty) (< col max-empty))
                   (fail-parse ys "block scalar content indented less than preceding empty line"))
                 (setf content-indent col))
               (when (< col content-indent)
                 (decf (ys-pos ys) col)
                 (return))
               (let ((extra (- col content-indent)))
                 (push (concatenate 'string
                                    (make-string extra :initial-element #\Space)
                                    (with-output-to-string (o)
                                      (loop until (or (ys-eof-p ys)
                                                      (b-break-p (ys-peek ys)))
                                            do (write-char (ys-next ys) o))))
                       lines)
                 (b-as-line-feed ys))))))
        (setf lines (nreverse lines))
        (let ((text (if (char= kind #\|)
                        (%join-literal lines)
                        (%join-folded lines))))
          (values (%apply-chomp text chomp :header-break header-break)
                  (if (char= kind #\|) :literal :folded)))))))

(defun %join-literal (lines)
  "[171] l-nb-literal-text — newline after every source line, then [165] chomp."
  (if (null lines)
      ""
      (with-output-to-string (o)
        (dolist (line lines)
          (write-string line o)
          (write-char #\Newline o)))))

(defun %join-folded (lines)
  "[176] l-nb-diff-lines / [172]–[175]. Extra blank after more-indent only
   when the next non-empty line is folded, not more-indented (7T8X vs 6VJK)."
  (with-output-to-string (out)
    (let ((prev-empty t)
          (prev-more nil)
          (blank-after-more nil)
          (first t))
      (dolist (line lines)
        (let ((empty (zerop (length line)))
              (more (and (plusp (length line))
                         (s-white-p (char line 0)))))
          (cond
            (empty
             (write-char #\Newline out)
             (when prev-more
               (setf blank-after-more t))
             (setf prev-empty t prev-more nil))
            (more
             (unless first
               (write-char #\Newline out))
             (write-string line out)
             (setf prev-empty nil first nil prev-more t blank-after-more nil))
            (t
             (unless first
               (cond
                 (blank-after-more (write-char #\Newline out))
                 (prev-empty)
                 (prev-more (write-char #\Newline out))
                 (t (write-char #\Space out))))
             (write-string line out)
             (setf prev-empty nil first nil prev-more nil blank-after-more nil))))))))

(defun %apply-chomp (text chomp &key header-break)
  "[163]–[167] c-chomping-indicator / l-chomped-empty.
   Empty keep keeps the header break when it exists (K858 vs 2G84/03)."
  (flet ((strip-nl (s)
           (let ((end (length s)))
             (loop while (and (plusp end) (char= (char s (1- end)) #\Newline))
                   do (decf end))
             (subseq s 0 end))))
    (ecase chomp
      (:keep
       (cond
         ((zerop (length text))
          (if header-break (string #\Newline) ""))
         ((char= (char text (1- (length text))) #\Newline) text)
         (t (concatenate 'string text (string #\Newline)))))
      (:strip (strip-nl text))
      (:clip
       (let ((stripped (strip-nl text)))
         (if (zerop (length stripped))
             ""
             (concatenate 'string stripped (string #\Newline))))))))

(defun c-sequence-entry-p (ys)
  "[4] c-sequence-entry `-` as [184] c-l-block-seq-entry (followed by s-white / b-break / '#')."
  (and (eql (ys-peek ys) #\-)
       (let ((n (ys-peek ys 1)))
         (or (null n) (s-white-p n) (b-break-p n) (eql n #\#)))))

(defun c-mapping-key-p (ys &optional flow)
  "[5] c-mapping-key `?` as an explicit key. In flow, `?` may sit against
   `,` `]` `}` (DFF7)."
  (and (eql (ys-peek ys) #\?)
       (let ((n (ys-peek ys 1)))
         (or (null n) (s-white-p n) (b-break-p n) (eql n #\#)
             (and flow (c-flow-indicator-p n))))))

(defun ns-s-block-map-implicit-key-p (ys)
  "[193] ns-s-block-map-implicit-key lookahead — implicit key may be a
   flow node (LX3P, Q9WF). Also true for [189] explicit `?`."
  (or (c-mapping-key-p ys)
      (with-ys-checkpoint (ys)
        (when (or (eql (ys-peek ys) #\&) (eql (ys-peek ys) #\!))
          (c-ns-properties ys)
          (s-separate-in-line ys))
        (cond
          ((eql (ys-peek ys) #\")
           (ignore-errors (c-double-quoted ys)))
          ((eql (ys-peek ys) #\')
           (ignore-errors (c-single-quoted ys)))
          ((eql (ys-peek ys) #\*)
           (ys-next ys)
           (ignore-errors (ns-anchor-name ys)))
          ((eql (ys-peek ys) #\[)
           (let ((start (ys-pos ys)))
             (handler-case (c-flow-sequence ys)
               (yaml-parse-error ()
                 (return-from ns-s-block-map-implicit-key-p nil)))
             (when (loop for i from start below (ys-pos ys)
                         thereis (b-break-p (char (ys-text ys) i)))
               (return-from ns-s-block-map-implicit-key-p nil))))
          ((eql (ys-peek ys) #\{)
           (let ((start (ys-pos ys)))
             (handler-case (c-flow-mapping ys)
               (yaml-parse-error ()
                 (return-from ns-s-block-map-implicit-key-p nil)))
             (when (loop for i from start below (ys-pos ys)
                         thereis (b-break-p (char (ys-text ys) i)))
               (return-from ns-s-block-map-implicit-key-p nil))))
          (t
           (loop
             (let ((c (ys-peek ys)))
               (cond
                 ((or (null c) (b-break-p c))
                  (return))
                 ((and (eql c #\#)
                       (or (zerop (ys-pos ys))
                           (s-white-p (char (ys-text ys) (1- (ys-pos ys))))))
                  (return))
                 ((and (eql c #\:) (colon-ends-plain-p ys nil))
                  (return))
                 (t (ys-next ys)))))))
        (s-separate-in-line ys)
        (eql (ys-peek ys) #\:))))

(defun emit-scalar (ys value &key anchor tag style)
  (emit ys :scalar :anchor anchor :tag tag :style (or style :plain)
        :value (or value "")))

(defun ns-flow-pair-p (ys)
  "[150] ns-flow-pair lookahead. [149] adjacent `:` after a JSON key (9MMW)."
  (let ((saved (ys-pos ys))
        (depth 0)
        (after-json nil))
    (unwind-protect
         (progn
           (s-l-comments ys)
           (when (c-mapping-key-p ys t)
             (return-from ns-flow-pair-p t))
           (loop
             (let ((c (ys-peek ys)))
               (cond
                 ((null c) (return nil))
                 ((eql c #\")
                  (or (ignore-errors (c-double-quoted ys)) (ys-next ys))
                  (when (zerop depth) (setf after-json t)))
                 ((eql c #\')
                  (or (ignore-errors (c-single-quoted ys)) (ys-next ys))
                  (when (zerop depth) (setf after-json t)))
                 ((eql c #\[)
                  (incf depth)
                  (setf after-json nil)
                  (ys-next ys))
                 ((eql c #\])
                  (when (minusp (decf depth)) (return nil))
                  (ys-next ys)
                  (when (zerop depth) (setf after-json t)))
                 ((eql c #\{)
                  (incf depth)
                  (setf after-json nil)
                  (ys-next ys))
                 ((eql c #\})
                  (when (minusp (decf depth)) (return nil))
                  (ys-next ys)
                  (when (zerop depth) (setf after-json t)))
                 ((and (zerop depth) (eql c #\:)
                       (or after-json (colon-ends-plain-p ys t)))
                  (return t))
                 ((and (zerop depth) (member c '(#\, #\] #\})))
                  (return nil))
                 (t
                  (setf after-json nil)
                  (ys-next ys))))))
      (setf (ys-pos ys) saved))))

(defun colon-after-break-p (ys)
  "True when `:` is the first non-white on its line (DK4H)."
  (let ((text (ys-text ys))
        (i (1- (ys-pos ys))))
    (loop while (and (>= i 0) (s-white-p (char text i)))
          do (decf i))
    (or (minusp i) (b-break-p (char text i)))))

(defun e-node (ys &key anchor tag)
  "[105] e-scalar / [106] e-node"
  (emit-scalar ys "" :anchor anchor :tag tag))

(defun ns-flow-pair (ys &key (indent -1))
  "[150] ns-flow-pair(n,c). Multiline plain is allowed (8KB6);
   `:` after a break is not (DK4H)."
  (emit ys :mapping-start :flow-p t)
  (if (c-mapping-key-p ys t)
      (progn
        (ys-next ys)
        (s-l-comments ys)
        (when (c-forbidden-p ys)
          (fail-parse ys "document marker in flow"))
        (if (member (ys-peek ys) '(#\: #\, #\] #\}))
            (e-node ys)
            (ns-flow-node ys :indent indent :key :explicit)))
      (ns-flow-node ys :indent indent))
  (s-l-comments ys)
  (unless (eql (ys-peek ys) #\:)
    (fail-parse ys "expected : in flow pair"))
  (when (colon-after-break-p ys)
    (fail-parse ys "implicit key must be on one line"))
  (ys-next ys)
  (s-l-comments ys)
  (if (member (ys-peek ys) '(#\, #\] #\}))
      (e-node ys)
      (ns-flow-node ys :indent indent))
  (emit ys :mapping-end))

(defun s-flow-line-prefix-ok (ys indent)
  "[69] s-flow-line-prefix(n) ::= s-indent(n) s-separate-in-line?
   Closers may sit at n."
  (let ((c (ys-peek ys)))
    (when (and c
               (not (member c '(#\] #\})))
               (>= indent 0)
               (< (ys-column ys) indent))
      (fail-parse ys "wrong indent in flow"))))

(defun ns-flow-seq-entry (ys &key (indent -1))
  "[139] ns-flow-seq-entry(n,c) ::= ns-flow-pair | ns-flow-node"
  (if (ns-flow-pair-p ys)
      (ns-flow-pair ys :indent indent)
      (ns-flow-node ys :indent indent)))

(defun c-flow-sequence (ys &key anchor tag (indent -1))
  "[137] c-flow-sequence(n,c) / [138] ns-s-flow-seq-entries"
  (unless (eql (ys-next ys) #\[)
    (fail-parse ys "expected ["))
  (emit ys :sequence-start :flow-p t :anchor anchor :tag tag)
  (s-l-comments ys)
  (loop
    (s-l-comments ys)
    (when (c-forbidden-p ys)
      (fail-parse ys "document marker in flow"))
    (s-flow-line-prefix-ok ys indent)
    (when (eql (ys-peek ys) #\])
      (ys-next ys)
      (emit ys :sequence-end)
      (return))
    (when (eql (ys-peek ys) #\,)
      (fail-parse ys "empty entry in flow sequence"))
    (ns-flow-seq-entry ys :indent indent)
    (s-l-comments ys)
    (cond
      ((eql (ys-peek ys) #\])
       (ys-next ys)
       (emit ys :sequence-end)
       (return))
      ((eql (ys-peek ys) #\,)
       (ys-next ys)
       (s-l-comments ys)
       (when (eql (ys-peek ys) #\,)
         (fail-parse ys "empty entry in flow sequence"))
       (when (eql (ys-peek ys) #\])
         (ys-next ys)
         (emit ys :sequence-end)
         (return)))
      (t (fail-parse ys "expected , or ] in flow sequence")))))

(defun ns-flow-map-explicit-entry (ys &key (indent -1))
  "[143] ns-flow-map-explicit-entry(n,c)"
  (ys-next ys)                          ; [5] c-mapping-key
  (s-l-comments ys)
  (if (member (ys-peek ys) '(#\: #\, #\}))
      (e-node ys)
      (ns-flow-node ys :indent indent :key :explicit))
  (s-l-comments ys)
  (if (eql (ys-peek ys) #\:)
      (progn
        (ys-next ys)
        (s-l-comments ys)
        (if (member (ys-peek ys) '(#\, #\}))
            (e-node ys)
            (ns-flow-node ys :indent indent)))
      (e-node ys)))

(defun ns-flow-map-implicit-entry (ys &key (indent -1))
  "[144] ns-flow-map-implicit-entry / [145] yaml-key / [146] empty-key /
   [148] json-key. [149] adjacent value after JSON key is valid."
  (if (and (eql (ys-peek ys) #\:) (colon-ends-plain-p ys t))
      (e-node ys)
      (ns-flow-node ys :indent indent))
  (s-l-comments ys)
  (cond
    ((eql (ys-peek ys) #\:)
     (ys-next ys)
     (s-l-comments ys)
     (if (member (ys-peek ys) '(#\, #\}))
         (e-node ys)
         (ns-flow-node ys :indent indent)))
    ((member (ys-peek ys) '(#\, #\}))
     (e-node ys))
    (t (fail-parse ys "expected : , or } in flow mapping"))))

(defun ns-flow-map-entry (ys &key (indent -1))
  "[142] ns-flow-map-entry(n,c)"
  (if (c-mapping-key-p ys t)
      (ns-flow-map-explicit-entry ys :indent indent)
      (ns-flow-map-implicit-entry ys :indent indent)))

(defun c-flow-mapping (ys &key anchor tag (indent -1))
  "[140] c-flow-mapping(n,c) / [141] ns-s-flow-map-entries"
  (unless (eql (ys-next ys) #\{)
    (fail-parse ys "expected {"))
  (emit ys :mapping-start :flow-p t :anchor anchor :tag tag)
  (s-l-comments ys)
  (loop
    (s-l-comments ys)
    (when (c-forbidden-p ys)
      (fail-parse ys "document marker in flow"))
    (s-flow-line-prefix-ok ys indent)
    (when (eql (ys-peek ys) #\})
      (ys-next ys)
      (emit ys :mapping-end)
      (return))
    (ns-flow-map-entry ys :indent indent)
    (s-l-comments ys)
    (cond
      ((eql (ys-peek ys) #\})
       (ys-next ys)
       (emit ys :mapping-end)
       (return))
      ((eql (ys-peek ys) #\,)
       (ys-next ys))
      (t (fail-parse ys "expected , or } in flow mapping")))))

(defun l+block-sequence (ys &key anchor tag)
  "[183] l+block-sequence(n)"
  (let ((indent (ys-column ys)))
    (emit ys :sequence-start :anchor anchor :tag tag)
    (loop
      (s-indent ys)
      (when (or (ys-eof-p ys) (c-forbidden-p ys))
        (return))
      (let ((col (ys-column ys)))
        (cond
          ((< col indent)
           (return))
          ((> col indent)
           (fail-parse ys "bad sequence indent"))
          ((not (c-sequence-entry-p ys))
           (return))
          (t
           (ys-next ys)                 ; [4] c-sequence-entry
           (cond
             ((or (ys-eof-p ys) (b-break-p (ys-peek ys)) (eql (ys-peek ys) #\#))
              (c-nb-comment-text ys)
              (b-as-line-feed ys)
              (s-l-comments ys)
              (if (and (not (ys-eof-p ys))
                       (not (c-forbidden-p ys))
                       (> (ys-column ys) indent))
                  (s-l+block-node ys :indent indent :in-seq t)
                  (e-node ys)))
             ((s-white-p (ys-peek ys))
              (%tab-then-block-indicator ys)
              (s-separate-in-line ys)
              (if (or (ys-eof-p ys) (b-break-p (ys-peek ys)) (eql (ys-peek ys) #\#))
                  (progn
                    (c-nb-comment-text ys)
                    (b-as-line-feed ys)
                    (s-l-comments ys)
                    (if (and (not (ys-eof-p ys))
                             (> (ys-column ys) indent)
                             (not (c-forbidden-p ys)))
                        (s-l+block-node ys :indent indent :in-seq t)
                        (e-node ys)))
                  (s-l+block-node ys :indent indent :in-seq t)))
             (t
              (decf (ys-pos ys))
              (return))))))
      (s-l-comments ys))
    (emit ys :sequence-end)))

(defun c-l-block-map-implicit-value (ys indent)
  "[194] c-l-block-map-implicit-value. Same-line value is not
   s-l+block-collection (needs a newline). Compact `key: - item` (5U3A)
   and `a: b: c` (ZCZ6) are invalid. Flow collections on the same line remain valid."
  (when (c-sequence-entry-p ys)
    (fail-parse ys "block sequence on the same line as mapping key"))
  (when (and (ns-s-block-map-implicit-key-p ys)
             (not (member (ys-peek ys) '(#\{ #\[))))
    (fail-parse ys "block mapping on the same line as mapping key"))
  (s-l+block-node ys :indent indent))

(defun block-value-here-p (ys indent)
  "After `key:\\n`, a same-indent block sequence is the value (`key:\\n- item`).
   A same-indent mapping key is the next entry, not the value (6KGN)."
  (and (not (ys-eof-p ys))
       (not (at-marker-p ys "---"))
       (not (at-marker-p ys "..."))
       (let ((col (ys-column ys)))
         (or (> col indent)
             (and (= col indent) (c-sequence-entry-p ys))))))

(defun %tab-then-block-indicator (ys)
  "[63] s-indent is spaces. A tab in the separator before a block
   indicator or implicit key is an error (Y79Y)."
  (let ((saw-tab nil)
        (saved (ys-pos ys)))
    (loop while (s-white-p (ys-peek ys))
          do (when (eql (ys-peek ys) #\Tab)
               (setf saw-tab t))
             (ys-next ys))
    (when (and saw-tab (or (c-sequence-entry-p ys)
                           (c-mapping-key-p ys)
                           (ns-s-block-map-implicit-key-p ys)
                           (eql (ys-peek ys) #\:)))
      (fail-parse ys "tab used as indentation"))
    (setf (ys-pos ys) saved)))

(defun c-l-block-map-explicit-entry (ys indent)
  "[189] c-l-block-map-explicit-entry / [190] explicit-key / [191] explicit-value"
  (ys-next ys)                          ; [5] c-mapping-key
  (%tab-then-block-indicator ys)
  (s-separate-in-line ys)
  (when (c-nb-comment-text-p ys)
    (c-nb-comment-text ys))
  (cond
    ((or (ys-eof-p ys) (b-break-p (ys-peek ys)))
     (b-as-line-feed ys)
     (s-l-comments ys)
     (if (block-value-here-p ys indent)
         (s-l+block-node ys :indent indent :key :explicit)
         (e-node ys)))
    (t (s-l+block-node ys :indent indent :key :explicit)))
  (s-l-comments ys)
  (s-indent ys)
  (if (and (= (ys-column ys) indent) (eql (ys-peek ys) #\:))
      (progn
        (ys-next ys)
        (%tab-then-block-indicator ys)
        (s-separate-in-line ys)
        (when (c-nb-comment-text-p ys)
          (c-nb-comment-text ys))
        (cond
          ((or (ys-eof-p ys) (b-break-p (ys-peek ys)))
           (b-as-line-feed ys)
           (s-l-comments ys)
           (if (block-value-here-p ys indent)
               (s-l+block-node ys :indent indent)
               (e-node ys)))
          (t (s-l+block-node ys :indent indent))))
      (e-node ys)))

(defun ns-l-block-map-implicit-entry (ys indent)
  "[192] ns-l-block-map-implicit-entry / [193] implicit-key / [194] implicit-value"
  (s-l+block-node ys :indent indent :key :implicit)
  (s-separate-in-line ys)
  (unless (eql (ys-peek ys) #\:)
    (fail-parse ys "expected : after mapping key"))
  (ys-next ys)
  (%tab-then-block-indicator ys)
  (s-separate-in-line ys)
  (when (c-nb-comment-text-p ys)
    (c-nb-comment-text ys))
  (cond
    ((or (ys-eof-p ys) (b-break-p (ys-peek ys)))
     (b-as-line-feed ys)
     (s-l-comments ys)
     (if (block-value-here-p ys indent)
         (s-l+block-node ys :indent indent)
         (e-node ys)))
    (t (c-l-block-map-implicit-value ys indent))))

(defun ns-l-block-map-entry (ys indent)
  "[188] ns-l-block-map-entry ::= explicit-entry | implicit-entry"
  (if (c-mapping-key-p ys)
      (c-l-block-map-explicit-entry ys indent)
      (ns-l-block-map-implicit-entry ys indent)))

(defun l+block-mapping (ys &key anchor tag)
  "[187] l+block-mapping(n)"
  (let ((indent (ys-column ys)))
    (emit ys :mapping-start :anchor anchor :tag tag)
    (loop
      (s-indent ys)
      (when (or (ys-eof-p ys) (c-forbidden-p ys))
        (return))
      (let ((col (ys-column ys)))
        (when (< col indent)
          (return))
        (when (c-sequence-entry-p ys)
          (when (> col indent)
            (fail-parse ys "wrong indentation in block sequence"))
          (return))
        (when (eql (ys-peek ys) #\Tab)
          (fail-parse ys "tab used as indentation"))
        (unless (= col indent)
          (when (> col indent)
            (fail-parse ys "bad mapping indent"))
          (return))
        (unless (or (c-mapping-key-p ys) (ns-s-block-map-implicit-key-p ys))
          (return))
        (ns-l-block-map-entry ys indent))
      (s-l-comments ys))
    (emit ys :mapping-end)))

(defun s-l+block-node (ys &key flow (indent -1) key in-seq doc-same-line)
  "[196] s-l+block-node(n,c) / [198] s-l+block-in-block / [161] ns-flow-node.
   KEY :implicit — [154] ns-s-implicit-yaml-key (one-line plain; no block collection).
   KEY :explicit — [5] `?` key; block collections allowed.
   IN-SEQ — same-indent `-` after properties+break is the next item (FH7J, PW8X).
   DOC-SAME-LINE — [200] s-l+block-collection needs s-l-comments; not on `---` line.
   [104] alias nodes do not take properties (SR86)."
  (let ((saved (ys-pos ys))
        (allow-block (and (not (eq key :implicit))
                          (or (not doc-same-line)))))
    (when (eql (ys-peek ys) #\*)
      (when (and (not flow) allow-block (ns-s-block-map-implicit-key-p ys))
        (l+block-mapping ys)
        (return-from s-l+block-node))
      (c-ns-alias-node ys)
      (return-from s-l+block-node))
    (multiple-value-bind (anchor tag)
        (c-ns-properties ys :indent indent)
      (let ((broke (s-separate-after-properties ys))
            (c (ys-peek ys)))
        (when (and (or anchor tag) (eql c #\*)
                   (not (ns-s-block-map-implicit-key-p ys)))
          (fail-parse ys "alias node cannot have properties"))
        (when (and broke (or (eql c #\&) (eql c #\!))
                   (<= (ys-column ys) indent))
          (fail-parse ys "properties must be more-indented than parent"))
        (when (and (or anchor tag) (not broke) c
                   (not (s-white-p c))
                   (not (member c '(#\{ #\[ #\| #\> #\" #\' #\*)))
                   (not (and flow (c-flow-indicator-p c)))
                   (not (and (eql c #\:) (colon-ends-plain-p ys flow)))
                   (not (ns-plain-first-p ys flow))
                   (not (and (not flow) (or (c-sequence-entry-p ys)
                                            (ns-s-block-map-implicit-key-p ys)))))
          (fail-parse ys "expected s-separate after properties"))
        (flet ((collection (parse-fn)
                 (if (and (not broke) (or anchor tag))
                     (progn
                       (setf (ys-pos ys) saved)
                       (funcall parse-fn ys))
                     (funcall parse-fn ys :anchor anchor :tag tag)))
               (block-coll-ok ()
                 (and (not flow)
                      (not (eq key :implicit))
                      (or broke (not doc-same-line))))
               (flow-n ()
                 "[197] s-l+flow-in-block(n) uses n+1; [161] ns-flow-node keeps n."
                 (if flow indent (1+ indent))))
          (cond
            ((and flow (c-forbidden-p ys))
             (fail-parse ys "document marker in flow"))
            ((or (null c)
                 (and flow (member c '(#\, #\] #\})))
                 (and (eql c #\:) (colon-ends-plain-p ys flow)
                      (or flow
                          (eq key :implicit)
                          (not (ns-s-block-map-implicit-key-p ys))))
                 (and (not flow)
                      (or (c-forbidden-p ys)
                          (< (ys-column ys) indent))))
             (e-node ys :anchor anchor :tag tag))
            ((and in-seq broke (c-sequence-entry-p ys)
                  (<= (ys-column ys) indent))
             (e-node ys :anchor anchor :tag tag))
            ((or (eql c #\{) (eql c #\[))
             (if (and (block-coll-ok) (ns-s-block-map-implicit-key-p ys))
                 (collection #'l+block-mapping)
                 (if (eql c #\{)
                     (c-flow-mapping ys :anchor anchor :tag tag :indent (flow-n))
                     (c-flow-sequence ys :anchor anchor :tag tag :indent (flow-n)))))
            ((or (eql c #\|) (eql c #\>))
             (multiple-value-bind (text style)
                 (l+block-scalar ys :indent indent)
               (emit-scalar ys text :anchor anchor :tag tag :style style)))
            ((and (block-coll-ok) (c-sequence-entry-p ys))
             (if (and broke (< (ys-column ys) indent))
                 (e-node ys :anchor anchor :tag tag)
                 (collection #'l+block-sequence)))
            ((and (block-coll-ok) (ns-s-block-map-implicit-key-p ys))
             (if (and broke (<= (ys-column ys) indent))
                 (e-node ys :anchor anchor :tag tag)
                 (collection #'l+block-mapping)))
            ((eql c #\")
             (emit-scalar ys (c-double-quoted ys :indent (flow-n)
                                                 :single-line (and (eq key :implicit)
                                                                   (not flow)))
                          :anchor anchor :tag tag :style :double))
            ((eql c #\')
             (emit-scalar ys (c-single-quoted ys :indent (flow-n)
                                                 :single-line (and (eq key :implicit)
                                                                   (not flow)))
                          :anchor anchor :tag tag :style :single))
            ((ns-plain-first-p ys flow)
             (emit-scalar ys (ns-plain ys :flow flow :indent indent
                                       :single-line (eq key :implicit))
                          :anchor anchor :tag tag :style :plain))
            (t
             (fail-parse ys "invalid node"))))))))

(defun ns-flow-node (ys &key (indent -1) key)
  "[161] ns-flow-node(n,c) ::= c-ns-alias-node | ns-flow-yaml-node | c-flow-json-node"
  (s-l+block-node ys :flow t :indent indent :key key))

(defun parse-node (ys &key flow (indent -1) key in-seq doc-same-line)
  (if flow
      (ns-flow-node ys :indent indent :key key)
      (s-l+block-node ys :indent indent :key key
                      :in-seq in-seq :doc-same-line doc-same-line)))

;;;; ch. 9 YAML Documents

(defun ns-yaml-version (ys)
  "[87] ns-yaml-version ::= ns-dec-digit+ '.' ns-dec-digit+"
  (unless (and (ys-peek ys) (digit-char-p (ys-peek ys)))
    (fail-parse ys "bad %YAML version"))
  (loop while (and (ys-peek ys) (digit-char-p (ys-peek ys)))
        do (ys-next ys))
  (unless (eql (ys-peek ys) #\.)
    (fail-parse ys "bad %YAML version"))
  (ys-next ys)
  (unless (and (ys-peek ys) (digit-char-p (ys-peek ys)))
    (fail-parse ys "bad %YAML version"))
  (loop while (and (ys-peek ys) (digit-char-p (ys-peek ys)))
        do (ys-next ys)))

(defun l-directive (ys)
  "[82] l-directive. %TAG updates handles. More than one %YAML is an error (SF5V).
   [87] then only s-b-comment — extra tokens (H7TQ) or `#` without
   s-separate-in-line (MUS6/00) are invalid."
  (unless (eql (ys-next ys) #\%)
    (fail-parse ys "expected %"))
  (let ((name (with-output-to-string (o)
                (loop for c = (ys-peek ys)
                      while (and c (not (s-white-p c)) (not (b-break-p c)))
                      do (write-char (ys-next ys) o)))))
    (cond
      ((string= name "TAG")
       (s-separate-in-line ys)
       (let ((handle (with-output-to-string (o)
                       (loop for c = (ys-peek ys)
                             while (and c (not (s-white-p c)) (not (b-break-p c)))
                             do (write-char (ys-next ys) o)))))
         (s-separate-in-line ys)
         (let ((prefix (with-output-to-string (o)
                         (loop for c = (ys-peek ys)
                               while (and c (not (s-white-p c)) (not (b-break-p c))
                                          (not (eql c #\#)))
                               do (write-char (ys-next ys) o)))))
           (when (or (zerop (length handle)) (zerop (length prefix)))
             (fail-parse ys "bad %TAG"))
           (setf (gethash handle (ys-tag-handles ys)) prefix))))
      ((string= name "YAML")
       (when (ys-yaml-directive-p ys)
         (fail-parse ys "multiple %YAML directives"))
       (setf (ys-yaml-directive-p ys) t)
       (unless (s-white-p (ys-peek ys))
         (fail-parse ys "expected s-separate-in-line after %YAML"))
       (s-separate-in-line ys)
       (ns-yaml-version ys))
      (t
       (loop for c = (ys-peek ys)
             while (and c (not (b-break-p c)))
             do (ys-next ys))))
    (unless (s-b-comment ys)
      (fail-parse ys "trailing junk after directive"))
    (b-as-line-feed ys)))

(defun l-document-prefix (ys)
  "[202] l-document-prefix (comments / empty lines) plus [82] l-directive
   when present — those make [209] l-directive-document, which still needs
   [203] c-directives-end."
  (let ((any nil))
    (loop
      (s-indent ys)
      (cond
        ((and (at-bol-p ys) (eql (ys-peek ys) #\%))
         (setf any t)
         (l-directive ys))
        ((c-nb-comment-text-p ys)
         (c-nb-comment-text ys)
         (b-as-line-feed ys))
        ((b-break-p (ys-peek ys))
         (b-as-line-feed ys))
        (t (return))))
    any))

(defun c-directives-end (ys)
  "[203] c-directives-end ::= '---'"
  (consume-marker ys "---"))

(defun l-document-suffix (ys)
  "[205] l-document-suffix ::= [204] c-document-end s-l-comments"
  (consume-end-marker ys)
  (s-l-comments ys)
  t)

(defun l-any-document (ys)
  "[210] l-any-document = l-directive-document | l-explicit-document | l-bare-document.
   Returns :empty if only a document suffix was consumed, else T."
  (reset-tag-handles ys)
  (let ((had-directives (l-document-prefix ys))
        (explicit-start nil)
        (explicit-end nil)
        (doc-same-line nil))
    (s-l-comments ys)
    (when (at-marker-p ys "---")
      (c-directives-end ys)
      (setf explicit-start t
            doc-same-line (not (or (ys-eof-p ys) (b-break-p (ys-peek ys)))))
      (s-l-comments ys))
    (when (and had-directives (not explicit-start))
      (fail-parse ys "directives require a document start marker"))
    (when (and (not explicit-start)
               (not had-directives)
               (or (ys-eof-p ys)
                   (at-marker-p ys "...")))
      (when (at-marker-p ys "...")
        (l-document-suffix ys))
      (return-from l-any-document :empty))
    (emit ys :document-start :implicit (not explicit-start))
    (cond
      ((or (ys-eof-p ys)
           (at-marker-p ys "---")
           (at-marker-p ys "..."))
       (e-node ys))
      (t
       (s-l+block-node ys :indent -1 :doc-same-line doc-same-line)
       (s-l-comments ys)
       (unless (or (ys-eof-p ys)
                   (at-marker-p ys "---")
                   (at-marker-p ys "..."))
         (fail-parse ys "unexpected content after document"))))
    (s-l-comments ys)
    (when (at-marker-p ys "...")
      (l-document-suffix ys)
      (setf explicit-end t))
    (emit ys :document-end :implicit (not explicit-end))
    (if explicit-end :ended t)))

(defun l-yaml-stream (ys)
  "[211] l-yaml-stream. After a document without l-document-suffix, only
   l-explicit-document may follow — not a bare %YAML (MUS6/01). After empty
   `...`, another l-any-document is allowed (M7A3)."
  (s-l-comments ys)
  (let ((need-suffix nil))
    (loop
      (s-l-comments ys)
      (when (ys-eof-p ys)
        (return))
      (when (and need-suffix (at-bol-p ys) (eql (ys-peek ys) #\%))
        (fail-parse ys "directives require a document end marker"))
      (let ((pos (ys-pos ys))
            (kind (l-any-document ys)))
        (when (= pos (ys-pos ys))
          (return))
        (setf need-suffix (eq kind t))))))

(defun parse-events-from-string (text)
  (let ((ys (make-ys (or text ""))))
    (c-byte-order-mark ys)
    (reset-tag-handles ys)
    (emit ys :stream-start)
    (l-yaml-stream ys)
    (emit ys :stream-end)
    (coerce (ys-events ys) 'list)))

(defun parse-yaml (text &key all)
  (compose-events (parse-events-from-string text) :all all))
