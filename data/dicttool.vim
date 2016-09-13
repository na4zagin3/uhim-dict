let s:kanaTable = { "あ": "a", "い": "i", "う": "u", "え": "e", "お": "o", "か": "ka", "き": "ki", "く": "ku", "け": "ke", "こ": "ko", "が": "ga", "ぎ": "gi", "ぐ": "gu", "げ": "ge", "ご": "go", "さ": "sa", "し": "si", "す": "su", "せ": "se", "そ": "so", "ざ": "za", "じ": "zi", "ず": "zu", "ぜ": "ze", "ぞ": "zo", "た": "ta", "ち": "ti", "つ": "tu", "て": "te", "と": "to", "だ": "da", "ぢ": "di", "づ": "du", "で": "de", "ど": "do", "な": "na", "に": "ni", "ぬ": "nu", "ね": "ne", "の": "no", "は": "ha", "ひ": "hi", "ふ": "hu", "へ": "he", "ほ": "ho", "ば": "ba", "び": "bi", "ぶ": "bu", "べ": "be", "ぼ": "bo", "ぱ": "pa", "ぴ": "pi", "ぷ": "pu", "ぺ": "pe", "ぽ": "po", "ま": "ma", "み": "mi", "む": "mu", "め": "me", "も": "mo", "や": "ya", "ゆ": "yu", "よ": "yo", "ら": "ra", "り": "ri", "る": "ru", "れ": "re", "ろ": "ro", "わ": "wa", "を": "wo", "ん": "n", }

function! GetCurrentLine()
  echom expand("<cWORD>")
endfunction

function! Adjective(type)
  return [ "- 日形容詞:", "    類: [ " . a:type . " ]" ]
endfunction

function! Verb(type)
  return [ "- 日動詞:", "    類: [ " . a:type . " ]" ]
endfunction

function! Nown()
  return [ "- 語:" ]
endfunction

function! WordType(str)
  if a:str =~ '-.*しい$'
	return Adjective("シイ, シク")
  elseif a:str =~ '-.*やか$'
	return Adjective("ナ, ナリ")
  elseif a:str =~ '-.*い$'
	return Adjective("イ, ク")

  elseif a:str =~ '-.*う$'
	return Verb("?")
  elseif a:str =~ '-.*いる$'
	return Verb("?")
  elseif a:str =~ '-.*える$'
	return Verb("?")

  elseif a:str =~ '-.*く$'
	return Verb("カ五, 文カ四")
  elseif a:str =~ '-.*きる$'
	return Verb("カ上一, 文カ上二")
  elseif a:str =~ '-.*ける$'
	return Verb("カ下一, 文カ下二")

  elseif a:str =~ '-.*ぐ$'
	return Verb("ガ五, 文ガ四")
  elseif a:str =~ '-.*ぎる$'
	return Verb("ガ上一, 文ガ上二")
  elseif a:str =~ '-.*げる$'
	return Verb("ガ下一, 文ガ下二")

  elseif a:str =~ '-.*す$'
	return Verb("サ五, 文サ四")
  elseif a:str =~ '-.*しる$'
	return Verb("サ上一, 文サ上二")
  elseif a:str =~ '-.*せる$'
	return Verb("サ下一, 文サ下二")

  elseif a:str =~ '-.*ず$'
	return Verb("?")
  elseif a:str =~ '-.*じる$'
	return Verb("?")
  elseif a:str =~ '-.*ぜる$'
	return Verb("ザ下一, 文ザ下二")

  elseif a:str =~ '-.*つ$'
	return Verb("タ五, 文タ四")
  elseif a:str =~ '-.*ちる$'
	return Verb("タ上一, 文タ上二")
  elseif a:str =~ '-.*てる$'
	return Verb("タ下一, 文タ下二")

  elseif a:str =~ '-.*でる$'
	return Verb("ダ下一, 文ダ下二")

  elseif a:str =~ '-.*ぬ$'
	return Verb("ナ五, 文ナ四")
  elseif a:str =~ '-.*にる$'
	return Verb("ナ上一, 文ナ上二")
  elseif a:str =~ '-.*ねる$'
	return Verb("ナ下一, 文ナ下二")

  elseif a:str =~ '-.*ぶ$'
	return Verb("バ五, 文バ四")
  elseif a:str =~ '-.*びる$'
	return Verb("バ上一, 文バ上二")
  elseif a:str =~ '-.*べる$'
	return Verb("バ下一, 文バ下二")

  elseif a:str =~ '-.*む$'
	return Verb("マ五, 文マ四")
  elseif a:str =~ '-.*みる$'
	return Verb("マ上一, 文マ上二")
  elseif a:str =~ '-.*める$'
	return Verb("マ下一, 文マ下二")
  elseif a:str =~ '-.*ゆ$'
	return Verb("ヤ五, 文ヤ四")

  elseif a:str =~ '-.*りる$'
	return Verb("ラ上一, 文ラ上二")
  elseif a:str =~ '-.*れる$'
	return Verb("ラ下一, 文ラ下二")
  elseif a:str =~ '-.*る$'
	return Verb("ラ五, 文ラ四")

  else
	return Nown()
  endif
endfunction

function! KanaChanged(str)
  if a:str =~ '^[いえお]'
	return 1
  elseif a:str =~ '^..*[わいうえお]'
	return 1
  elseif a:str =~ '[じず]'
	return 1
  endif

  return 0
endfunction

function! Okurigana(kana)
  return "      - *" . s:kanaTable[a:kana]
endfunction

function! Okuriganas(kana)
  let str = []

  for s:kana in split(a:kana, '\zs')
	let str += [ Okurigana(s:kana) ]
  endfor

  return str
endfunction

function! Kanji(kanji, yomi)
  let kanjiDef = [ "      - 字:", "          日: " . a:kanji ]
  if KanaChanged(a:yomi)
	let yomiDef = [ "        讀: { 日訓: { 舊: , 新: " . a:yomi . " } }" ]
  else
	let yomiDef = [ "        讀: { 日訓: " . a:yomi . " }" ]
  endif

  return kanjiDef + yomiDef
endfunction

function! WordDefinition(kanji, yomi)
  let comment = [ "# " . a:yomi ]
  let kanjiYomi = matchstr(a:yomi, '^[^-]*', 0)
  let okurigana = matchstr(a:yomi, '-\zs.*', 0)
  let typeDef = WordType(a:yomi)
  let yomiDef = Kanji(a:kanji, kanjiYomi) + Okuriganas(okurigana)
  let tagDef = [ "    簽:", "      - 日-常用" ]
  return comment + typeDef + [ "    聯:" ] + yomiDef + tagDef
endfunction

function! ExpandWordDefinition()
  let s:definitions = split(expand("<cWORD>"), '、')
  let s:ans = []
  for s:yomi in s:definitions[1:]
	let s:ans += WordDefinition(s:definitions[0], s:yomi)
  endfor

  call append(line('.'), s:ans)
  normal dd
endfunction
" s/、\([^、]*\)/- 語:\r    聯:\r      - 字:\r          日: \r        讀: { 日訓: \1 }\r    簽:\r      - 日-常用\r/g
