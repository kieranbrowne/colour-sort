(ns colour-sort.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (let [palette (q/load-image "freud.jpg")
        subject (q/load-image "hands8.jpg")]
        ; pix (q/pixels palette)
        ; shuf (shuffle (vec pix))
    (q/resize palette (q/width) (q/height))
        
    (q/resize subject (.-width palette) (.-height palette))
    ; (dotimes [i (count pix)]
    ;   (aset pix i (nth shuf i)))

    ; (q/update-pixels palette)
    {:palette palette
     :subject subject}))
     

(defn colour-dist [a b]
  (Math/sqrt 
    (+
     (Math/pow (- (q/red a) (q/red b)) 2)
     (Math/pow (- (q/green a) (q/green b)) 2)
     (Math/pow (- (q/blue a) (q/blue b)) 2))))

(defn colour-dist' [a b]
  (Math/sqrt 
    (+
     (Math/pow (- (q/brightness a) (q/brightness b)) 2)
     (Math/abs (- (q/hue a) (q/hue b)))
     (Math/abs (- (q/saturation a) (q/saturation b)))
     ; (Math/pow (- (q/blue a) (q/blue b)) 2)
     )))

(defn adj-col [w h x y]
  (filterv (complement nil?)
          (for [x' [-1 0 1] y' [-1 0 1]]
            (if (= [0 0] [x' y'])
              nil
              (vector (mod (- x x') w) (mod (+ y y') h))))))

  


(mapv #(map mod % [20 2])
  (map #(map + %1 %2) [[1 2] [2 3]] (repeat [50 50])))

(defn pairwise [fn & colls]
  (if (coll? (-> colls first first))
    (apply (partial mapv fn) colls)
    (apply mapv (partial pairwise fn) colls)))


(pairwise mod [[108 2]] [[100 100]])
(pairwise mod [108 2] [100 100])
  

(vector 1 2)


(defn pos-score [img kernel colour x y]
  (let [colour-dist (partial colour-dist' colour)
        ;; adj-col (partial adj-col (.-width img) (.-height img))
        kernel
        (mapv 
          #(vector (mod (+ x (first %)) (.-width img))
                   (mod (+ y (second %)) (.-height img)))
          kernel)]
        
    (reduce +
      (map #(let [[x y] %] (colour-dist (q/get-pixel img x y)))
            kernel))))

(get-in (vec (range 100)) [10])




(defn update-state [state]
  (let [img (:palette state)
        pix (q/pixels img)
        image-score (partial pos-score (:subject state) [[0 0]])
        smooth-score (partial pos-score (:palette state) 
                              (remove #{[0 0]} (for [x' [-9 -4 -2 -1 0 1 2 4 9] y' [9 -4 -2 -1 0 1 2 4 9]] [x' y'])))]
                              
        

    ;; make like the image
    (dotimes [i 2000]
      (let [x1 (rand-int (.-width img))
            y1 (rand-int (.-height img))
            col1 (q/get-pixel img x1 y1)
            ;; x2 (rand-int (.-width img))
            x2 (int (mod (+ x1 (q/random -80 80)) (.-width img)))
            ; x2 (int (mod (+ (q/random 0 0) (q/map-range (q/cos (+ (q/random 2) (/ (q/brightness col1) 13))) -1 1 0 (.-width img))) (.-width img)))
            ; y2 (rand-int (.-height img))
            y2 (int (mod (+ y1 (q/random -80 80)) (.-height img)))
            ; y2 (int (mod (+ (q/random 0 0) (q/map-range (q/sin (+ (q/random 2) (/ (q/red col1) 9))) -1 1 0 (.-height img))) (.-height img)))
            col2 (q/get-pixel img x2 y2)]
            
        ; if swapping is less jumbled than not swapping
        (when (< (+ (image-score col1 x2 y2)
                    (image-score col2 x1 y1))
                 (+ (image-score col2 x2 y2)
                    (image-score col1 x1 y1) 0.1)) ;; slight offset to peturb the status quo
          (q/set-pixel 
            img x2 y2 col1)
          (q/set-pixel 
            img x1 y1 col2))))
          
    ;; fight grain
    (dotimes [i 1000]
      (let [x1 (rand-int (.-width img))
            y1 (rand-int (.-height img))
            col1 (q/get-pixel img x1 y1)
            ; x2 (rand-int (.-width img))
            x2 (int (mod (+ x1 (q/random -80 80)) (.-width img)))
            ; x2 (int (mod (+ (q/random 0 0) (q/map-range (q/cos (+ (q/random 2) (/ (q/brightness col1) 13))) -1 1 0 (.-width img))) (.-width img)))
            ; y2 (rand-int (.-height img))
            y2 (int (mod (+ y1 (q/random -80 80)) (.-height img)))
            ; y2 (int (mod (+ (q/random 0 0) (q/map-range (q/sin (+ (q/random 2) (/ (q/red col1) 9))) -1 1 0 (.-height img))) (.-height img)))
            col2 (q/get-pixel img x2 y2)]
            
        ; if swapping is less jumbled than not swapping
        (when (< (+ (smooth-score col1 x2 y2)
                    (smooth-score col2 x1 y1))
                 (+ (smooth-score col2 x2 y2)
                    (smooth-score col1 x1 y1)))
          (q/set-pixel 
            img x2 y2 col1)
          (q/set-pixel 
            img x1 y1 col2))))
    (q/update-pixels img)
    state))
    


(defn draw-state [state]
  (q/background 2)
  (q/image (:palette state) 0 0)
  ; (q/image (:subject state) 0 0)
  ; (q/text "test" 20 20)
  ; (q/ellipse 20 20 20 20)
  ;; (print state)
  (q/stroke 255)
  (q/fill 255)
  ;; (q/save (str (q/frame-count) ".png"))
  (q/text (str (q/frame-count)) 20 20))
  

(q/defsketch colour-sort
  :title "Test"
  :size [640 858]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
  
