module Graphics.Declarative.SDL.Keys where

import Data.Int

data Key
  = KeyUnkown
  | KeyBackspace
  | KeyTab
  | KeyReturn
  | KeyEscape
  | KeySpace
  | KeyExclaim
  | KeyQuotedbl
  | KeyHash
  | KeyDollar
  | KeyPercent
  | KeyAmpersand
  | KeyQuote
  | KeyLeftparen
  | KeyRightParen
  | KeyAsterisk
  | KeyPlus
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash
  | KeyN0
  | KeyN1
  | KeyN2
  | KeyN3
  | KeyN4
  | KeyN5
  | KeyN6
  | KeyN7
  | KeyN8
  | KeyN9
  | KeyColon
  | KeySemiColon
  | KeyLess
  | KeyEquals
  | KeyGreater
  | KeyQuestion
  | KeyAt
  | KeyLeftBracket
  | KeyBackSlash
  | KeyRightBracket
  | KeyCaret
  | KeyUnderscore
  | KeyBackquote
  | KeyCharA
  | KeyCharB
  | KeyCharC
  | KeyCharD
  | KeyCharE
  | KeyCharF
  | KeyCharG
  | KeyCharH
  | KeyCharI
  | KeyCharJ
  | KeyCharK
  | KeyCharL
  | KeyCharM
  | KeyCharN
  | KeyCharO
  | KeyCharP
  | KeyCharQ
  | KeyCharR
  | KeyCharS
  | KeyCharT
  | KeyCharU
  | KeyCharV
  | KeyCharW
  | KeyCharX
  | KeyCharY
  | KeyCharZ
  | KeyDelete
  | KeyCAPSLOCK
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyPrintScreen
  | KeyScrollLock
  | KeyPause
  | KeyInsert
  | KeyHome
  | KeyPageUp
  | KeyEnd
  | KeyPageDown
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | KeyNumLockClear
  | KeyKeypadDivide
  | KeyKeypadMultiply
  | KeyKeypadMinus
  | KeyKeypadPlus
  | KeyKeypadEnter
  | KeyKeypad1
  | KeyKeypad2
  | KeyKeypad3
  | KeyKeypad4
  | KeyKeypad5
  | KeyKeypad6
  | KeyKeypad7
  | KeyKeypad8
  | KeyKeypad9
  | KeyKeypad0
  | KeyKeypadPeriod
  | KeyApplication
  | KeyPower
  | KeyKeypadEquals
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyExecute
  | KeyHelp
  | KeyMenu
  | KeySelect
  | KeyStop
  | KeyAgain
  | KeyUndo
  | KeyCut
  | KeyCopy
  | KeyPaste
  | KeyFind
  | KeyMute
  | KeyVolumeUp
  | KeyVolumeDown
  | KeyKeypadComma
  | KeyKeypadEqualsAS400
  | KeyAlterASE
  | KeySysReq
  | KeyCancel
  | KeyClear
  | KeyPrior
  | KeyReturn2
  | KeySeperator
  | KeyOut
  | KeyOper
  | KeyClearAgain
  | KeyCRSel
  | KeyEXSel
  | KeyKeypad00
  | KeyKeypad000
  | KeyThousandsSeperator
  | KeyDecimalsSeperator
  | KeyCurrencyUnit
  | KeyCurrencySubunit
  | KeyKeypadLeftParen
  | KeyKeypadRightParen
  | KeyKeypadLeftBrace
  | KeyKeypadRightBrace
  | KeyKeypadTab
  | KeyKeypadBackSpace
  | KeyKeypadA
  | KeyKeypadB
  | KeyKeypadC
  | KeyKeypadD
  | KeyKeypadE
  | KeyKeypadF
  | KeyKeypadXor
  | KeyKeypadPower
  | KeyKeypadPercent
  | KeyKeypadLess
  | KeyKeypadGreater
  | KeyKeypadAmpersand
  | KeyKeypadDBLAmpersand
  | KeyKeypadVerticalBar
  | KeyKeypadDBLVerticalBar
  | KeyKeypadColon
  | KeyKeypadHash
  | KeyKeypadSpace
  | KeyKeypadAt
  | KeyKeypadExclaim
  | KeyKeypadMemstore
  | KeyKeypadMemRecall
  | KeyKeypadMemClear
  | KeyKeypadMemAdd
  | KeyKeypadMemSubtract
  | KeyKeypadMemMultiply
  | KeyKeypadMemDivide
  | KeyKeypadPlusMinus
  | KeyKeypadClear
  | KeyKeypadClearEntry
  | KeyKeypadBinary
  | KeyKeypadOctal
  | KeyKeypadDecimal
  | KeyKeypadHexaDecimal
  | KeyLCtrl
  | KeyLShift
  | KeyLAlt
  | KeyLGui
  | KeyRCtrl
  | KeyRShift
  | KeyRAlt
  | KeyRGui
  | KeyMode
  | KeyAudioNext
  | KeyAudioPrev
  | KeyAudioStop
  | KeyAudioPlay
  | KeyAudioMute
  | KeyMediaSelect
  | KeyWWW
  | KeyMail
  | KeyCalculator
  | KeyComputer
  | KeyAppControlSearch
  | KeyAppControlHome
  | KeyAppControlBack
  | KeyAppControlForward
  | KeyAppControlStop
  | KeyAppControlRefresh
  | KeyAppControlBookmarks
  | KeyBrightnessDown
  | KeyBrightnessUp
  | KeyDisplaySwitch
  | KeyKBDIllumToogle
  | KeyKBDIllumDown
  | KeyKBDIllumUp
  | KeyEject
  | KeySleep
  deriving (Show, Eq, Ord, Read)

fromKeycode :: Int32 -> Maybe Key
fromKeycode 0 = Just KeyUnkown
fromKeycode 8 = Just KeyBackspace
fromKeycode 9 = Just KeyTab
fromKeycode 13 = Just KeyReturn
fromKeycode 27 = Just KeyEscape
fromKeycode 32 = Just KeySpace
fromKeycode 33 = Just KeyExclaim
fromKeycode 34 = Just KeyQuotedbl
fromKeycode 35 = Just KeyHash
fromKeycode 36 = Just KeyDollar
fromKeycode 37 = Just KeyPercent
fromKeycode 38 = Just KeyAmpersand
fromKeycode 39 = Just KeyQuote
fromKeycode 40 = Just KeyLeftparen
fromKeycode 41 = Just KeyRightParen
fromKeycode 42 = Just KeyAsterisk
fromKeycode 43 = Just KeyPlus
fromKeycode 44 = Just KeyComma
fromKeycode 45 = Just KeyMinus
fromKeycode 46 = Just KeyPeriod
fromKeycode 47 = Just KeySlash
fromKeycode 48 = Just KeyN0
fromKeycode 49 = Just KeyN1
fromKeycode 50 = Just KeyN2
fromKeycode 51 = Just KeyN3
fromKeycode 52 = Just KeyN4
fromKeycode 53 = Just KeyN5
fromKeycode 54 = Just KeyN6
fromKeycode 55 = Just KeyN7
fromKeycode 56 = Just KeyN8
fromKeycode 57 = Just KeyN9
fromKeycode 58 = Just KeyColon
fromKeycode 59 = Just KeySemiColon
fromKeycode 60 = Just KeyLess
fromKeycode 61 = Just KeyEquals
fromKeycode 62 = Just KeyGreater
fromKeycode 63 = Just KeyQuestion
fromKeycode 64 = Just KeyAt
fromKeycode 91 = Just KeyLeftBracket
fromKeycode 92 = Just KeyBackSlash
fromKeycode 93 = Just KeyRightBracket
fromKeycode 94 = Just KeyCaret
fromKeycode 95 = Just KeyUnderscore
fromKeycode 96 = Just KeyBackquote
fromKeycode 97 = Just KeyCharA
fromKeycode 98 = Just KeyCharB
fromKeycode 99 = Just KeyCharC
fromKeycode 100 = Just KeyCharD
fromKeycode 101 = Just KeyCharE
fromKeycode 102 = Just KeyCharF
fromKeycode 103 = Just KeyCharG
fromKeycode 104 = Just KeyCharH
fromKeycode 105 = Just KeyCharI
fromKeycode 106 = Just KeyCharJ
fromKeycode 107 = Just KeyCharK
fromKeycode 108 = Just KeyCharL
fromKeycode 109 = Just KeyCharM
fromKeycode 110 = Just KeyCharN
fromKeycode 111 = Just KeyCharO
fromKeycode 112 = Just KeyCharP
fromKeycode 113 = Just KeyCharQ
fromKeycode 114 = Just KeyCharR
fromKeycode 115 = Just KeyCharS
fromKeycode 116 = Just KeyCharT
fromKeycode 117 = Just KeyCharU
fromKeycode 118 = Just KeyCharV
fromKeycode 119 = Just KeyCharW
fromKeycode 120 = Just KeyCharX
fromKeycode 121 = Just KeyCharY
fromKeycode 122 = Just KeyCharZ
fromKeycode 127 = Just KeyDelete
fromKeycode 1073741881 = Just KeyCAPSLOCK
fromKeycode 1073741882 = Just KeyF1
fromKeycode 1073741883 = Just KeyF2
fromKeycode 1073741884 = Just KeyF3
fromKeycode 1073741885 = Just KeyF4
fromKeycode 1073741886 = Just KeyF5
fromKeycode 1073741887 = Just KeyF6
fromKeycode 1073741888 = Just KeyF7
fromKeycode 1073741889 = Just KeyF8
fromKeycode 1073741890 = Just KeyF9
fromKeycode 1073741891 = Just KeyF10
fromKeycode 1073741892 = Just KeyF11
fromKeycode 1073741893 = Just KeyF12
fromKeycode 1073741894 = Just KeyPrintScreen
fromKeycode 1073741895 = Just KeyScrollLock
fromKeycode 1073741896 = Just KeyPause
fromKeycode 1073741897 = Just KeyInsert
fromKeycode 1073741898 = Just KeyHome
fromKeycode 1073741899 = Just KeyPageUp
fromKeycode 1073741901 = Just KeyEnd
fromKeycode 1073741902 = Just KeyPageDown
fromKeycode 1073741903 = Just KeyRight
fromKeycode 1073741904 = Just KeyLeft
fromKeycode 1073741905 = Just KeyDown
fromKeycode 1073741906 = Just KeyUp
fromKeycode 1073741907 = Just KeyNumLockClear
fromKeycode 1073741908 = Just KeyKeypadDivide
fromKeycode 1073741909 = Just KeyKeypadMultiply
fromKeycode 1073741910 = Just KeyKeypadMinus
fromKeycode 1073741911 = Just KeyKeypadPlus
fromKeycode 1073741912 = Just KeyKeypadEnter
fromKeycode 1073741913 = Just KeyKeypad1
fromKeycode 1073741914 = Just KeyKeypad2
fromKeycode 1073741915 = Just KeyKeypad3
fromKeycode 1073741916 = Just KeyKeypad4
fromKeycode 1073741917 = Just KeyKeypad5
fromKeycode 1073741918 = Just KeyKeypad6
fromKeycode 1073741919 = Just KeyKeypad7
fromKeycode 1073741920 = Just KeyKeypad8
fromKeycode 1073741921 = Just KeyKeypad9
fromKeycode 1073741922 = Just KeyKeypad0
fromKeycode 1073741923 = Just KeyKeypadPeriod
fromKeycode 1073741925 = Just KeyApplication
fromKeycode 1073741926 = Just KeyPower
fromKeycode 1073741927 = Just KeyKeypadEquals
fromKeycode 1073741928 = Just KeyF13
fromKeycode 1073741929 = Just KeyF14
fromKeycode 1073741930 = Just KeyF15
fromKeycode 1073741931 = Just KeyF16
fromKeycode 1073741932 = Just KeyF17
fromKeycode 1073741933 = Just KeyF18
fromKeycode 1073741934 = Just KeyF19
fromKeycode 1073741935 = Just KeyF20
fromKeycode 1073741936 = Just KeyF21
fromKeycode 1073741937 = Just KeyF22
fromKeycode 1073741938 = Just KeyF23
fromKeycode 1073741939 = Just KeyF24
fromKeycode 1073741940 = Just KeyExecute
fromKeycode 1073741941 = Just KeyHelp
fromKeycode 1073741942 = Just KeyMenu
fromKeycode 1073741943 = Just KeySelect
fromKeycode 1073741944 = Just KeyStop
fromKeycode 1073741945 = Just KeyAgain
fromKeycode 1073741946 = Just KeyUndo
fromKeycode 1073741947 = Just KeyCut
fromKeycode 1073741948 = Just KeyCopy
fromKeycode 1073741949 = Just KeyPaste
fromKeycode 1073741950 = Just KeyFind
fromKeycode 1073741951 = Just KeyMute
fromKeycode 1073741952 = Just KeyVolumeUp
fromKeycode 1073741953 = Just KeyVolumeDown
fromKeycode 1073741957 = Just KeyKeypadComma
fromKeycode 1073741958 = Just KeyKeypadEqualsAS400
fromKeycode 1073741977 = Just KeyAlterASE
fromKeycode 1073741978 = Just KeySysReq
fromKeycode 1073741979 = Just KeyCancel
fromKeycode 1073741980 = Just KeyClear
fromKeycode 1073741981 = Just KeyPrior
fromKeycode 1073741982 = Just KeyReturn2
fromKeycode 1073741983 = Just KeySeperator
fromKeycode 1073741984 = Just KeyOut
fromKeycode 1073741985 = Just KeyOper
fromKeycode 1073741986 = Just KeyClearAgain
fromKeycode 1073741987 = Just KeyCRSel
fromKeycode 1073741988 = Just KeyEXSel
fromKeycode 1073742000 = Just KeyKeypad00
fromKeycode 1073742001 = Just KeyKeypad000
fromKeycode 1073742002 = Just KeyThousandsSeperator
fromKeycode 1073742003 = Just KeyDecimalsSeperator
fromKeycode 1073742004 = Just KeyCurrencyUnit
fromKeycode 1073742005 = Just KeyCurrencySubunit
fromKeycode 1073742006 = Just KeyKeypadLeftParen
fromKeycode 1073742007 = Just KeyKeypadRightParen
fromKeycode 1073742008 = Just KeyKeypadLeftBrace
fromKeycode 1073742009 = Just KeyKeypadRightBrace
fromKeycode 1073742010 = Just KeyKeypadTab
fromKeycode 1073742011 = Just KeyKeypadBackSpace
fromKeycode 1073742012 = Just KeyKeypadA
fromKeycode 1073742013 = Just KeyKeypadB
fromKeycode 1073742014 = Just KeyKeypadC
fromKeycode 1073742015 = Just KeyKeypadD
fromKeycode 1073742016 = Just KeyKeypadE
fromKeycode 1073742017 = Just KeyKeypadF
fromKeycode 1073742018 = Just KeyKeypadXor
fromKeycode 1073742019 = Just KeyKeypadPower
fromKeycode 1073742020 = Just KeyKeypadPercent
fromKeycode 1073742021 = Just KeyKeypadLess
fromKeycode 1073742022 = Just KeyKeypadGreater
fromKeycode 1073742023 = Just KeyKeypadAmpersand
fromKeycode 1073742024 = Just KeyKeypadDBLAmpersand
fromKeycode 1073742025 = Just KeyKeypadVerticalBar
fromKeycode 1073742026 = Just KeyKeypadDBLVerticalBar
fromKeycode 1073742027 = Just KeyKeypadColon
fromKeycode 1073742028 = Just KeyKeypadHash
fromKeycode 1073742029 = Just KeyKeypadSpace
fromKeycode 1073742030 = Just KeyKeypadAt
fromKeycode 1073742031 = Just KeyKeypadExclaim
fromKeycode 1073742032 = Just KeyKeypadMemstore
fromKeycode 1073742033 = Just KeyKeypadMemRecall
fromKeycode 1073742034 = Just KeyKeypadMemClear
fromKeycode 1073742035 = Just KeyKeypadMemAdd
fromKeycode 1073742036 = Just KeyKeypadMemSubtract
fromKeycode 1073742037 = Just KeyKeypadMemMultiply
fromKeycode 1073742038 = Just KeyKeypadMemDivide
fromKeycode 1073742039 = Just KeyKeypadPlusMinus
fromKeycode 1073742040 = Just KeyKeypadClear
fromKeycode 1073742041 = Just KeyKeypadClearEntry
fromKeycode 1073742042 = Just KeyKeypadBinary
fromKeycode 1073742043 = Just KeyKeypadOctal
fromKeycode 1073742044 = Just KeyKeypadDecimal
fromKeycode 1073742045 = Just KeyKeypadHexaDecimal
fromKeycode 1073742048 = Just KeyLCtrl
fromKeycode 1073742049 = Just KeyLShift
fromKeycode 1073742050 = Just KeyLAlt
fromKeycode 1073742051 = Just KeyLGui
fromKeycode 1073742052 = Just KeyRCtrl
fromKeycode 1073742053 = Just KeyRShift
fromKeycode 1073742054 = Just KeyRAlt
fromKeycode 1073742055 = Just KeyRGui
fromKeycode 1073742081 = Just KeyMode
fromKeycode 1073742082 = Just KeyAudioNext
fromKeycode 1073742083 = Just KeyAudioPrev
fromKeycode 1073742084 = Just KeyAudioStop
fromKeycode 1073742085 = Just KeyAudioPlay
fromKeycode 1073742086 = Just KeyAudioMute
fromKeycode 1073742087 = Just KeyMediaSelect
fromKeycode 1073742088 = Just KeyWWW
fromKeycode 1073742089 = Just KeyMail
fromKeycode 1073742090 = Just KeyCalculator
fromKeycode 1073742091 = Just KeyComputer
fromKeycode 1073742092 = Just KeyAppControlSearch
fromKeycode 1073742093 = Just KeyAppControlHome
fromKeycode 1073742094 = Just KeyAppControlBack
fromKeycode 1073742095 = Just KeyAppControlForward
fromKeycode 1073742096 = Just KeyAppControlStop
fromKeycode 1073742097 = Just KeyAppControlRefresh
fromKeycode 1073742098 = Just KeyAppControlBookmarks
fromKeycode 1073742099 = Just KeyBrightnessDown
fromKeycode 1073742100 = Just KeyBrightnessUp
fromKeycode 1073742101 = Just KeyDisplaySwitch
fromKeycode 1073742102 = Just KeyKBDIllumToogle
fromKeycode 1073742103 = Just KeyKBDIllumDown
fromKeycode 1073742104 = Just KeyKBDIllumUp
fromKeycode 1073742105 = Just KeyEject
fromKeycode 1073742106 = Just KeySleep
fromKeycode _ = Nothing
