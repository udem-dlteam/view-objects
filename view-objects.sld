(define-library (view-objects)

  (export view-objects)

  (import (github.com/udem-dlteam/view-pdf)
          (gambit))

  (begin

    (define default-font "Courier New")

    (define (object-graph->dot roots options port)
      (let ((sharing (make-table test: eq?))
            (nodes (make-table test: eq?))
            (todo '())
            (arcs '())
            (nb-outgoing-port-num 0))

        (namespace ("##"
                    readtable?
                    closure? closure-length closure-code closure-ref
                    subprocedure-parent subprocedure-id
                    return? interp-procedure?
                    continuation? continuation-frame continuation-denv
                    frame? explode-frame
                    structure? structure-type unchecked-structure-ref
                    type-name type-fields type-super
                    symbol-name symbol-hash
                    keyword-name keyword-hash))

        (define pair-to-list-optimize?
          (cond ((assoc 'pair-to-list-optimize options) => cdr)
                (else #t)))

        (define layout-optimize?
          (cond ((assoc 'layout-optimize options) => cdr)
                (else #f)))

        (define detail-level
          (cond ((assoc 'detail-level options) => cdr)
                (else 0)))

        (define show-cont-denv?
          (cond ((assoc 'show-cont-denv options) => cdr)
                (else #f)))

        (define show-readtable?
          (cond ((assoc 'show-readtable options) => cdr)
                (else #f)))

        (define show-interp-procedures?
          (cond ((assoc 'show-interpreted-procedures options) => cdr)
                (else #f)))

        (define absent-obj "absent")

        (define (pair->list pair)
          (list (car pair) (cdr pair)))

        (define (box->list box)
          (list (unbox box)))

        (define (structure->list struct)
          (if (> detail-level 0)
              (vector->list (##vector-copy struct))
              (let ((lst '()))
                (for-each-field
                 (lambda (field-name value last?)
                   (set! lst (cons value lst)))
                 struct
                 (structure-type struct)
                 #t)
                (reverse lst))))

        (define (for-each-field proc obj type last?)
          (if (not type) ;; have we reached root of inheritance chain?
              1
              (let ((fields (type-fields type)))
                (let loop1 ((i 0)
                            (first #f)
                            (last -1))
                  (let ((i*3 (* i 3)))
                    (if (< i*3 (vector-length fields))
                        (let ((field-attributes
                               (vector-ref fields (+ i*3 1))))
                          (if (= (bitwise-and field-attributes 1) 0)
                              (loop1 (+ i 1)
                                     (or first i)
                                     i)
                              (loop1 (+ i 1)
                                     first
                                     last)))
                        (let ((start
                               (for-each-field
                                proc
                                obj
                                (type-super type)
                                (if first #f last?))))
                          (let loop2 ((i (or first 0)))
                            (if (not (fx< last i))
                                (let* ((i*3
                                        (fx* i 3))
                                       (field-attributes
                                        (vector-ref fields (+ i*3 1))))
                                  (if (= (bitwise-and field-attributes 1) 0)
                                      (let ((field-name
                                             (vector-ref fields i*3)))
                                        (proc (symbol->string field-name)
                                              (unchecked-structure-ref
                                               obj
                                               (+ start i)
                                               type
                                               #f)
                                              (and last?
                                                   (= i last)))))
                                  (loop2 (+ i 1)))))
                          (+ start (quotient (vector-length fields) 3)))))))))

        (define (closure->list closure)
          (let loop ((i (- (closure-length closure) 1))
                     (lst '()))
            (if (= i 0)
                (cons (closure-code closure) lst)
                (loop (- i 1)
                      (cons (closure-ref closure i) lst)))))

        (define (continuation->list continuation)
          (list (continuation-frame continuation)
                (if show-cont-denv?
                    (continuation-denv continuation)
                    "denv")))

        (define (frame->list frame)
          (vector->list (explode-frame frame)))

        (define (symbol->list symbol)
          (if (= detail-level 2)
              (list (symbol-name symbol)
                    (symbol-hash symbol))
              (vector->list (##vector-copy symbol))))

        (define (keyword->list keyword)
          (if (= detail-level 2)
              (list (keyword-name keyword)
                    (keyword-hash keyword))
              (vector->list (##vector-copy keyword))))

        (define (mark referrer #!optional (obj absent-obj))
          (let ((x (table-ref sharing obj #f)))
            (table-set! sharing
                        obj
                        (cons referrer (or x '())))
            (if (not x)
                (mark-recursively obj))))

        (define (mark-recursively #!optional (obj absent-obj))

          (define (mark-objs objs)
            (for-each (lambda (#!optional (x absent-obj))
                        (mark obj x))
                      objs))

          (cond ((pair? obj)
                 (mark-objs (pair->list obj)))
                ((box? obj)
                 (mark-objs (box->list obj)))
                ((vector? obj)
                 (mark-objs (vector->list obj)))
                ((and (readtable? obj)
                      (not show-readtable?)))
                ((structure? obj)
                 (mark-objs (structure->list obj)))
                ((= detail-level 0))
                ((and (procedure? obj)
                      (closure? obj)
                      (or show-interp-procedures?
                          (not (interp-procedure? obj))))
                 (mark-objs (closure->list obj)))
                ((continuation? obj)
                 (mark-objs (continuation->list obj)))
                ((frame? obj)
                 (mark-objs (frame->list obj)))
                ((= detail-level 1))
                ((symbol? obj)
                 (mark-objs (symbol->list obj)))
                ((keyword? obj)
                 (mark-objs (keyword->list obj)))))

        (define (mark-graph)
          (for-each (lambda (root) (mark root (cdr root))) roots))

        (define (single-ref? obj)
          (null? (cdr (table-ref sharing obj))))

        (define (inline? #!optional (obj absent-obj))
          (cond ((or (pair? obj)
                     (box? obj)
                     (vector? obj)
                     (readtable? obj)
                     (structure? obj)
                     (procedure? obj)
                     (return? obj)
                     (continuation? obj)
                     (frame? obj))
                 #f)
                ((string? obj)
                 (single-ref? obj))
                ((or (symbol? obj)
                     (keyword? obj))
                 (= detail-level 0))
                (else
                 #t)))

        (define (register-node! #!optional (node absent-obj))
          (let ((id (string-append
                     "node_"
                     (number->string (table-length nodes)))))
            (table-set! nodes node id)
            id))

        (define (register-root-nodes)
          (for-each register-node! roots))

        (define (out . strs)
          (for-each (lambda (str) (display str port)) strs))

        (define (write-col text outgoing-port-num)
          (if outgoing-port-num
              (out "<td port=\"" (number->string outgoing-port-num) "\">")
              (out "<td>"))
          (out text "</td>"))

        (define (escape str)
          (append-strings
           (map (lambda (c)
                  (case c
                    ((#\<) "&lt;")
                    ((#\>) "&gt;")
                    ((#\[) "&#91;")
                    ((#\]) "&#93;")
                    ((#\&) "&amp;")
                    ((#\") "&quot;")
                    (else  (string c))))
                (string->list str))))

        (define (node-id x referrer-id)
          (or (table-ref nodes x #f)
              (begin
                (set! todo (cons (cons x referrer-id) todo))
                (register-node! x))))

        (define (add-arc! referrer-id outgoing-port-num id attributes)
          (set! arcs
            (cons (vector referrer-id
                          outgoing-port-num
                          id
                          attributes)
                  arcs)))

        (define (write-ref obj referrer-id parent-id)
          (let ((id (node-id obj referrer-id)))
            (if (inline? obj)
                (write-col (escape (object->string obj)) #f)
                (let ((outgoing-port-num (+ 1 nb-outgoing-port-num)))
                  (set! nb-outgoing-port-num outgoing-port-num)
                  (add-arc! referrer-id
                            outgoing-port-num
                            id
                            (if (and layout-optimize?
                                     parent-id
                                     (single-ref? obj))
                                ":w [constraint=false]"
                                ""))
                  (if (and layout-optimize? parent-id)
                      (add-arc! parent-id
                                #f
                                id
                                " [style=invis]"))
                  (write-col (escape " ") outgoing-port-num)))))

        (define (write-refs objs referrer-id parent-id)
          (if (pair? objs)
              (let loop ((objs objs))
                (let ((rest (cdr objs)))
                  (write-ref (car objs) referrer-id parent-id)
                  (if (pair? rest)
                      (begin
                        (write-col (escape " ") #f)
                        (loop rest)))))))

        (define (write-node id attributes thunk)
          (out "  " id " [label = <<table" attributes "><tr>")
          (set! nb-outgoing-port-num 0)
          (thunk)
          (out "</tr></table>>];\n"))

        (define (font face color content)
          (if (or face color)
              (string-append "<font"
                             (if face
                                 (string-append " face=\"" face "\"")
                                 "")
                             (if color
                                 (string-append " color=\"" color "\"")
                                 "")
                             ">"
                             content
                             "</font>")
              content))

        (define default-table-attributes
          " border=\"0\" cellspacing=\"0\" cellpadding=\"0\"")

        (define (write-root-node root)
          (let ((id (node-id root #f)))
            (write-node
             id
             (string-append default-table-attributes " bgcolor=\"gray80\"")
             (lambda ()
               (let ((label (car root)))
                 (if (and label (not (string=? label "")))
                     (write-col
                      (font #f "white" (escape (car root)))
                      #f)))
               (write-ref (cdr root) id #f)))))

        (define (write-obj-node obj referrer-id)
          (if (not (inline? obj))
              (let ((id (node-id obj referrer-id)))
                (write-node
                 id
                 default-table-attributes
                 (lambda ()

                   (define (simple)
                     (write-col (escape (object->string obj)) #f))

                   (cond ((pair? obj)
                          (write-col (escape "(") #f)
                          (write-ref (car obj) id referrer-id)
                          (let loop ((probe (cdr obj)))
                            (cond ((pair? probe)
                                   (if (and pair-to-list-optimize?
                                            (single-ref? probe))
                                       (begin
                                         (write-col (escape " ") #f)
                                         (write-ref (car probe) id referrer-id)
                                         (loop (cdr probe)))
                                       (begin
                                         (write-col (escape " . ") #f)
                                         (write-ref probe id referrer-id))))
                                  ((not (and pair-to-list-optimize?
                                             (null? probe)))
                                   (write-col (escape " . ") #f)
                                   (write-ref probe id referrer-id))))
                          (write-col (escape ")") #f))
                         ((box? obj)
                          (write-col (escape "#&") #f)
                          (write-ref (unbox obj) id referrer-id))
                         ((vector? obj)
                          (write-col (escape "#(") #f)
                          (write-refs (vector->list obj) id referrer-id)
                          (write-col (escape ")") #f))
                         ((and (readtable? obj)
                               (not show-readtable?))
                          (simple))
                         ((structure? obj)
                          (if (> detail-level 0)
                              (write-details obj
                                             id
                                             (structure->list obj)
                                             referrer-id)
                              (begin
                                (write-col (escape "#<") #f)
                                (write-col
                                 (escape (object->string
                                          (type-name (structure-type obj))))
                                 #f)
                                (write-col (escape " #") #f)
                                (write-col
                                 (escape (number->string
                                          (object->serial-number obj)))
                                 #f)
                                (for-each-field
                                 (lambda (field-name value last?)
                                   (write-col (escape " ") #f)
                                   (write-col (escape field-name) #f)
                                   (write-col (escape ": ") #f)
                                   (write-ref value id referrer-id))
                                 obj
                                 (structure-type obj)
                                 #t)
                                (write-col (escape ">") #f))))
                         ((and (> detail-level 0)
                               (procedure? obj)
                               (closure? obj)
                               (or show-interp-procedures?
                                   (not (interp-procedure? obj))))
                          (write-details obj id (closure->list obj) referrer-id))
                         ((and (> detail-level 0)
                               (or (and (procedure? obj)
                                        (not (closure? obj)))
                                   (return? obj)))
                          (let ((num (subprocedure-id obj)))
                            (if (= 0 num)
                                (simple)
                                (begin
                                  (write-detail-prefix obj)
                                  (write-col (escape " #") #f)
                                  (write-col
                                   (escape (number->string
                                            num))
                                   #f)
                                  (write-col (escape " in ") #f)
                                  (write-col
                                   (escape (object->string
                                            (subprocedure-parent obj)))
                                   #f)))))
                         ((and (> detail-level 0)
                               (continuation? obj))
                          (write-details obj id (continuation->list obj) referrer-id))
                         ((and (> detail-level 0)
                               (frame? obj))
                          (write-details obj id (frame->list obj) referrer-id))
                         ((and (> detail-level 1)
                               (symbol? obj))
                          (write-details obj id (symbol->list obj) referrer-id))
                         ((and (> detail-level 1)
                               (keyword? obj))
                          (write-details obj id (keyword->list obj) referrer-id))
                         (else
                          (simple))))))))

        (define (write-detail-prefix obj)
          (write-col (font #f
                           "gray60"
                           (string-append (escape (object->string obj)) "="))
                     #f))

        (define (write-details obj id fields referrer-id)
          (write-detail-prefix obj)
          (write-col (escape "[") #f)
          (write-refs fields id referrer-id)
          (write-col (escape "]") #f))

        (define (write-header)
          (out "digraph \"graph\" {\n"
               "  graph [splines = true overlap = false rankdir = \"TD\"];\n"
               "  node [fontname = \"" default-font "\" shape = \"none\"];\n"))

        (define (write-footer)
          (out "}\n"))

        (define (write-graph)
          (write-header)
          (for-each write-root-node roots)
          (let loop ()
            (if (pair? todo)
                (let ((obj (car todo)))
                  (set! todo (cdr todo))
                  (write-obj-node (car obj) (cdr obj))
                  (loop))))
          (for-each (lambda (arc)
                      (let ((referrer-id       (vector-ref arc 0))
                            (outgoing-port-num (vector-ref arc 1))
                            (id                (vector-ref arc 2))
                            (attributes        (vector-ref arc 3)))
                        (out "  "
                             referrer-id
                             (if outgoing-port-num
                                 (string-append ":"
                                                (number->string outgoing-port-num))
                                 "")
                             ":s -> "
                             id
                             attributes
                             ";\n")))
                    arcs)
          (write-footer))

        (mark-graph)
        (register-root-nodes)
        (write-graph)))

    (define (find-dot-program)
      (call-with-current-continuation
       (lambda (return)
         (for-each (lambda (program)
                     (let ((r (shell-command (string-append program " -V") #t)))
                       (if (and (= 0 (car r))
                                (string? (cdr r))
                                (##string-prefix=? (cdr r)
                                                   "dot - graphviz version "))
                           (return program))))
                   '("dot"))
         #f)))

    (define (file-open? path)
      (let ((r (shell-command (string-append "lsof " path) #t)))
        (= 0 (car r))))

    (define (wait-until-file-no-longer-open path)
      (let loop ()
        (thread-sleep! 1)
        (if (file-open? path)
            (loop))))

    (define (create-temp-dir prefix)
      (let ((tmp-dir (or (getenv "TEMP" #f) (getenv "TMP" #f) "/tmp")))
        (create-temporary-directory (path-expand prefix tmp-dir))))

    (define (view-objects roots #!optional (options '()))
      (let ((dot-program (find-dot-program)))
        (if (not dot-program)
            (error "could not find the 'dot' program (is the 'graphviz' package installed?)")
            (let ((tmp-dir (create-temp-dir "view-objects")))

              (define (cleanup)
                (delete-file-or-directory tmp-dir #t))

              (with-exception-catcher
               (lambda (exc)
                 (cleanup)
                 (raise exc))
               (lambda ()
                 (let* ((dot-file (path-expand "view-objects.dot" tmp-dir))
                        (out-port (open-output-file dot-file)))
                   (object-graph->dot roots options out-port)
                   (close-port out-port)
                   (let ((r (shell-command
                             (string-append dot-program
                                            " -O -Tpdf "
                                            dot-file)
                             #t)))
                     (if (not (= 0 (car r)))
                         (error "execution of 'dot' program terminated abnormally:" (cdr r))
                         (let ((pdf-file (string-append dot-file ".pdf")))
                           (view-pdf pdf-file)
                           ;; delete PDF file when PDF viewer is done
                           (wait-until-file-no-longer-open pdf-file)
                           (cleanup)))))))))))))
