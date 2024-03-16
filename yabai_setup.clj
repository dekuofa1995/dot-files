#!/usr/bin/env bb
(require '[clojure.string :as str])
(require '[babashka.process :refer [shell]])

(declare DEFUALT-GAP GAP)

;; helper region start

(def RETAIN-KEYWORDS #{:sub-layer})
(defn key->str
  [k]
  (cond (keyword? k)
          (if (k RETAIN-KEYWORDS) (name k) (str/replace (name k) "-" "_"))
        :else (str k)))
(defn val->str
  [val]
  (cond (string? val) (format "'%s'" val)
        (boolean? val) (if val "on" "off")
        (number? val) val
        (keyword? val) (name val)
        :else (throw (ex-info (format "Unexcepted value type: %s" (type val))
                              {:val val}))))
(defn args->str
  ([args] (args->str args "=" nil))
  ([args separator prefix]
   (->> (map (if prefix
               #(format "%s%s%s%s" prefix (key->str %1) separator (val->str %2))
               #(format "%s%s%s" (key->str %1) separator (val->str %2)))
          (map name (keys args))
          (vals args))
        (str/join " "))))
(comment
  (args->str {:label "test", :app "test-app", :keyword :key-test}))

;; helper region end
;; commands region
(defn yabai-conf-cmd
  [[k v]]
  (format "yabai -m config %s %s" (key->str k) (val->str v)))
(comment
  (yabai-conf-cmd [:test :a]))
(defn yabai-rule-cmd
  ([args] (yabai-rule-cmd args :add))
  ([args oper] (format "yabai -m rule --%s %s" (name oper) (args->str args))))

(defn yabai-space-cmd
  "Yabai space setting command.
  eg: yabai -m space 1 --label web --display 1"
  ([id args] (format "yabai -m space %d %s" id (args->str args " " "--"))))
(defn yabai-signal-cmd
  ([args] (yabai-signal-cmd args :add))
  ([args oper] (format "yabai -m signal --%s %s" (name oper) (args->str args))))

(defn prefix-rule
  ([name] (prefix-rule name false))
  ([name manage]
   (let [regex (format "^%s" name)]
     {:label regex, :app regex, :manage manage, :sub-layer :below})))
(defn disable-focus-rule
  [name]
  (let [regex (format "^%s" name)]
    {:label regex, :app regex, :mouse_follows_focus false}))
;; commands region end

;; config region
(def DEFUALT-GAP 11)

(def COLOR
  {:focused "0xffacfabd", :normal "0x00010101", :preselect "0xE02d74da"})

(def CONFIG
  {:external-bar "all:42:0",
   :layout :bsp,
   :top-padding 11,
   :left-padding 11,
   :right-padding 11,
   :bottom-padding 11,
   :window-gap 12,
   :mouse-follows-focus true,
   :focus-follows-mouse :autoraise,
   :window-opacity true,
   :window-shadow false,
   :normal-window-opacity 0.96,
   :auto-balance false,
   :split-ratio 0.5,
   :mouse-modifier :fn,
   :mouse-action1 :move,
   :mouse-action2 :resize})
(def ignore-apps
  ["WeChat" "Discord" "Spotify" "QQ" "Telegram" "SwitchHosts" "JetBrains"
   "Input Source Pro" "Surge" "Surge Dashboard" "Mimestream" "iStat Menus"
   "Itsycal" "百度网盘" "TencentMeeting" "Only Switch" "System Preferences"
   "App Store" "Activity Monitor" "Calculator" "Dictionary" "Software Update"
   "Podcasts" "Safari" "Finder" "Reminders" "Font Book" "Steam Helper"])

(def disable-focus-apps
  ["百度网盘" "WeChat" "Xnip" "iStat Menus" "Steam Helper" "Only Switch"])
(def special-rules
  [{:label "About This Mac",
    :app "System Information",
    :title "About This Mac",
    :manage false,
    :sub-layer :below} {:app ".*", :sub-layer :above}])
(def rule-args
  (apply concat
    [special-rules (map prefix-rule ignore-apps)
     (map disable-focus-rule disable-focus-apps)]))
(def space-args
  [{:label :web, :display 1} ;
   {:label :prog, :display 1}      ;
   {:label :minor-web, :display 2} ;
   {:label :books, :display 2}])
(def signal-args [{:event :dock_did_restart, :action "sudo yabai --load-sa"}])

(def conf-commands (map yabai-conf-cmd (seq CONFIG)))
(def rule-commands (map yabai-rule-cmd rule-args))
(def space-commands (map-indexed #(yabai-space-cmd (inc %1) %2) space-args))
(def signal-commands (map yabai-signal-cmd signal-args))
(comment
  (map yabai-rule-cmd (take 5 rule-args))
  (map yabai-conf-cmd (seq CONFIG))
  (map-indexed #(yabai-space-cmd (inc %1) %2) space-args)
  (map yabai-signal-cmd signal-args))
(let [commands (apply concat
                 [conf-commands rule-commands space-commands signal-commands])]
  (doseq [cmd commands]
    (try (-> (shell {:out :string, :err :string} cmd)
             (select-keys [:out :err]))
         (catch clojure.lang.ExceptionInfo e
           (let [{out :out, err :err} (ex-data e)] (print out err))))))
;; (doseq [cmd conf-commands] (shell cmd))
;; (doseq [cmd rule-commands] (shell cmd))
;; (doseq [cmd space-commands]
;;   (try (shell cmd) (catch clojure.lang.ExceptionInfo e (print e))))
;; (doseq [cmd signal-commands] (shell cmd))
;; config region end
