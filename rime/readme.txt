* rime 白鹭(egret) 输入方案
核心卖点: 五笔*拼音整句混输方案 (同一句话可同时使用五笔与拼音)
| egret_wubi_py_sentence | 五笔*拼音整句混输        |
| egret_wubi_sentence    | 五笔整句输入(可拼音反查) |
| egret_wubi_py          | 五笔拼音                 |
| egret_wubi             | 五笔                     |
| egret                  | 全拼                     |
* 参考
 雾凇拼音 https://github.com/iDvel/rime-ice
 万象拼音：带声调的拼音词库 https://github.com/amzxyz/rime_wanxiang
 墨奇版 五笔整句输入方案  https://github.com/gaboolic/rime-wubi-sentence
白霜拼音: https://github.com/gaboolic/rime-frost
 
* 好文章 https://ksqsf.moe/posts/2023-06-01-rime-double-pinyin/
** 拼写运算（spelling algebra）
拼写运算做的事情很单纯，就是从已有的编码(字典中的)算出一个新编码（用户输入的）

 我们希望一个字的实际输入编码是 yyxx（音音形形）
 ，并且用 y, yy, yyx, yyxx 都可以打出这个字。为此，将
 上面的 speller 一节可以作如下修改：
 #+begin_src yaml-ts
speller:
  alphabet: abcdefghijklmnopqrstuvwxyz  # 可以删掉 ; 了
  delimiter: " '"
  algebra:
  # 假设有码表
  # 时	ui;oc	999000

  # #可以只输入 $1$2$3部分 即 "uioc" $1=ui $2=o $3=c
    - derive/^(\w*);(\w)(\w)$/$1$2$3/
  # #可以只输入 $1$2$3部分 即 "uio" $1=ui $2=o
    - derive/^(\w*);(\w)(\w)$/$1$2/
  # #可以只输入 $1$2$3部分 即 "ui" $1=ui
    - derive/^(\w*);(\w)(\w)$/$1/
 #+end_src
