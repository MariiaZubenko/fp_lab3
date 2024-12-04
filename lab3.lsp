; Конструктивний метод
(defun left-to-right (A L R k)
  "Рекурсивний прохід списку зліва направо з обміном елементів, якщо необхідно."
  (if (>= L R)
      (values k A)
      (let* ((current (nth L A))
             (next (nth (1+ L) A))
             (updated-A (if (>= current next)
                            (append (subseq A 0 L)
                                    (list next current)
                                    (nthcdr (+ 2 L) A))
                            A))
             (new-k (if (>= current next) L k)))
        (left-to-right updated-A (1+ L) R new-k))))

; Конструктивний метод
(defun right-to-left (A R L k)
  "Рекурсивний прохід списку справа наліво з обміном елементів, якщо необхідно."
  (if (<= R L)
      (values k A)
      (let* ((prev (nth (1- R) A))
             (current (nth R A))
             (updated-A (if (>= prev current)
                            (append (subseq A 0 (1- R))
                                    (list current prev)
                                    (nthcdr (1+ R) A))
                            A))
             (new-k (if (>= prev current) (1- R) k)))
        (right-to-left updated-A (1- R) L new-k))))

(defun exchange4-rec (A L R)
  "Рекурсивний алгоритм сортування"
  (if (>= L R)
      A
      (multiple-value-bind (k new-A) (left-to-right A L R L)
        (multiple-value-bind (new-k new-A) (right-to-left new-A k L k)
          (exchange4-rec new-A (1+ new-k) k)))))

(defun exchange4-constructive (A)
  "Шейкерне сортування масива A за незменшенням, використовуючи конструктивний метод"
  (exchange4-rec  A 0 (1- (length A))))



(defun check-constructive (name input expected)
  "Execute `exchange4-constructive' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (exchange4-constructive input) expected)
          name))

(defun test-exchange4-constructive ()
  (check-constructive "test 1.1" '(1 2 3 4 5) '(1 2 3 4 5) )
  (check-constructive "test 1.2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-constructive "test 1.3" '(5 4 3 4 2) '(2 3 4 4 5))
  (check-constructive "test 1.4" '(3 2 -4 6 1 5 3 ) '(-4 1 2 3 3 5 6 )))



; Імперативний Метод
(defun exchange4-imperative (A)
  "Шейкерне сортування масиву A за незменшенням, використовуючи імперативний метод."
  (let* ((A-copy (copy-list A))  ; Створюємо копію списку
         (N (length A-copy))     ; Визначаємо довжину копії
         (L 0)
         (R (1- N))
         (k 0))
    (loop while (< L R) do
          ;; Пересуваємо максимальні елементи вправо
          (loop for i from L below R do
                (when (>= (nth i A-copy) (nth (1+ i) A-copy))
                  (rotatef (nth i A-copy) (nth (1+ i) A-copy))
                  (setf k i)))
          (setf R k)  ; Оновлюємо праву межу
          
          ;; Пересуваємо мінімальні елементи вліво
          (loop for i from (1- R) downto L do
                (when (>= (nth i A-copy) (nth (1+ i) A-copy))
                  (rotatef (nth i A-copy) (nth (1+ i) A-copy))
                  (setf k i)))
          (setf L (1+ k)))  ; Оновлюємо ліву межу
    A-copy))  ; Повертаємо відсортовану копію списку


(defun check-imperative (name input expected)
  "Execute `exchange4-imperative' on `input', compare result with `expected' and print
  comparison status."
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal (exchange4-imperative input) expected)
          name))

(defun test-exchange4-imperative ()
  (check-imperative "test 2.1" '(1 2 3 4 5) '(1 2 3 4 5) )
  (check-imperative "test 2.2" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-imperative "test 2.3" '(5 4 3 4 2) '(2 3 4 4 5))
  (check-imperative "test 2.4" '(3 2 -4 6 1 5 3 ) '(-4 1 2 3 3 5 6 )))

(test-exchange4-constructive)
(test-exchange4-imperative)