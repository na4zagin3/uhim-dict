# UHIM Dict

## TUT-Code

### 交書変換辞書の生成
追記予定

#### uim-tutcode用
```
$ stack build && stack exec uhim-dict -- skk-yomi -t Uim kana.yaml kana-ext.yaml joyo.yaml non-joyo.yaml > ~/tcode/mazegaki-trad.dic
```

#### tcvime用
```
$ stack build && stack exec uhim-dict -- skk-yomi -t Tcvime kana.yaml kana-ext.yaml joyo.yaml non-joyo.yaml > ~/tcode/mazegaki-trad.dic
```

#### tc.el用
現状、uim-tutcodeと同じ。

```
$ stack build && stack exec uhim-dict -- skk-yomi -t TcEl kana.yaml kana-ext.yaml joyo.yaml non-joyo.yaml > ~/tcode/mazegaki-trad.dic
```

### キー配列の生成
追記予定

#### tcvime への追加
追記予定
```
$ cd data
$ stack build && stack exec uhim-dict -- tcvime --name tutcodeo kana.yaml kana-ext.yaml joyo.yaml non-joyo.yaml
$ mv tutcodeo.vim ~/.vim/keymap
```

#### tc.el への追加
以下の様に tc 本体への変更が要る。
```
$ cd data
$ stack build && stack exec uhim-dict -- tcel --name tutcodeo kana.yaml kana-ext.yaml joyo.yaml non-joyo.yaml
$ mv tutcodeo-tbl.el ~/.emacs.d/elpa/tc-<version>/
```
例：
```diff
diff -u tc/tc-pre.el /Users/mrty/.emacs.d/elpa/tc-20150113.1926/tc-pre.el
--- tc/tc-pre.el	2016-10-07 21:48:18.000000000 +0900
+++ /Users/mrty/.emacs.d/elpa/tc-20150113.1926/tc-pre.el	2016-09-29 10:18:30.000000000 +0900
@@ -120,7 +120,8 @@
   '(("japanese-T-Code" . "tc-tbl")
     ("japanese-TT-Code" . "ttc-tbl")
     ("japanese-Try-Code" . "try-tbl")
-    ("japanese-TUT-Code" . "tutc-tbl"))
+    ("japanese-TUT-Code" . "tutc-tbl")
+    ("japanese-TUTO-Code" . "tutcodeo-tbl"))
   "名前とテーブル名との対応")
 
 ;;;; site information
diff -u tc/tc-setup.el /Users/mrty/.emacs.d/elpa/tc-20150113.1926/tc-setup.el
--- tc/tc-setup.el	2016-10-07 21:48:18.000000000 +0900
+++ /Users/mrty/.emacs.d/elpa/tc-20150113.1926/tc-setup.el	2016-09-29 10:18:04.000000000 +0900
@@ -42,6 +42,11 @@
       (autoload 'toggle-input-method "tc-sysdep" nil t))
   (if (not (fboundp 'register-input-method))
       (load "tc-sysdep"))
+  (register-input-method "japanese-TUTO-Code"
+			 "Japanese"
+			 'tcode-use-package
+			 "TUTO"
+			 "a kanji direct input method")
   (register-input-method "japanese-TUT-Code"
 			 "Japanese"
 			 'tcode-use-package
```

## 語一覧の出力

### LaTeX形式
追記予定
```
$ cd data
$ stack build && stack exec uhim-dict -- latex -o latex-joyo.tex kana.yaml joyo.yaml && uplatex latex-joyo && dvipdfmx latex-joyo
```
