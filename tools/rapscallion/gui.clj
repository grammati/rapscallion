(ns rapscallion.gui
  (:use (rapscallion core))
  (:use (clojure (pprint :only [pprint])))
  (:import (javax.swing JFrame JTextPane SwingUtilities JButton JLabel)
           (java.awt GridLayout)
           (java.awt.event KeyListener ActionListener))
  )
  
(defn button [label action]
  (doto (JButton. label)
    (.addActionListener 
      (proxy [ActionListener] []
        (actionPerformed [e]
          (action e))))))

(defn rap-gui []
  (let [frame (JFrame. "Rapscallion Debugger")
        xml-input (JTextPane.)
        code-display (JTextPane.)
        update-code-display 
          (fn []
            (let [xml-in (.getText xml-input)
                  code (with-out-str
                         (try
                           (-> xml-in to-template-fn pprint)
                           (catch Exception e
                             (.printStackTrace e (java.io.PrintWriter. *out*)))))]
              (.setText code-display code)))
        rap-reload
          (fn []
            (use 'rapscallion.core :reload-all))]
    (.addKeyListener xml-input
      (proxy [KeyListener] []
        (keyTyped [evt]
          (SwingUtilities/invokeLater update-code-display))
        (keyPressed [evt])
        (keyReleased [evt])
        ))
    (doto frame
      (.setLayout (GridLayout. 1 1 3 3)) ; rows, cols, hgap, vgap
      ;(.add (JLabel. "")) ; filler
      ;(.add (button "Reload" (fn [_] (rap-reload))))
      (.add xml-input)
      (.add code-display)
      (.setSize 800 500)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))

(rap-gui)
