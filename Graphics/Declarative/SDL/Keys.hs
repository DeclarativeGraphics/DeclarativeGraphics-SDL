module Graphics.Declarative.SDL.Keys where

import Data.Int

data Key
  = Unkown
  | Backspace
  | Tab
  | Return
  | Escape
  | Space
  | Exclaim
  | Quotedbl
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | Quote
  | Leftparen
  | RightParen
  | Asterisk
  | Plus
  | Comma
  | Minus
  | Period
  | Slash
  | N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | Colon
  | SemiColon
  | Less
  | Equals
  | Greater
  | Question
  | At
  | LeftBracket
  | BackSlash
  | RightBracket
  | Caret
  | Underscore
  | Backquote
  | CharA
  | CharB
  | CharC
  | CharD
  | CharE
  | CharF
  | CharG
  | CharH
  | CharI
  | CharJ
  | CharK
  | CharL
  | CharM
  | CharN
  | CharO
  | CharP
  | CharQ
  | CharR
  | CharS
  | CharT
  | CharU
  | CharV
  | CharW
  | CharX
  | CharY
  | CharZ
  | Delete
  | CAPSLOCK
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | PrintScreen
  | ScrollLock
  | Pause
  | Insert
  | Home
  | PageUp
  | End
  | PageDown
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | NumLockClear
  | KeypadDivide
  | KeypadMultiply
  | KeypadMinus
  | KeypadPlus
  | KeypadEnter
  | Keypad1
  | Keypad2
  | Keypad3
  | Keypad4
  | Keypad5
  | Keypad6
  | Keypad7
  | Keypad8
  | Keypad9
  | Keypad0
  | KeypadPeriod
  | Application
  | Power
  | KeypadEquals
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | F21
  | F22
  | F23
  | F24
  | Execute
  | Help
  | Menu
  | Select
  | Stop
  | Again
  | Undo
  | Cut
  | Copy
  | Paste
  | Find
  | Mute
  | VolumeUp
  | VolumeDown
  | KeypadComma
  | KeypadEqualsAS400
  | AlterASE
  | SysReq
  | Cancel
  | Clear
  | Prior
  | Return2
  | Seperator
  | Out
  | Oper
  | ClearAgain
  | CRSel
  | EXSel
  | Keypad00
  | Keypad000
  | ThousandsSeperator
  | DecimalsSeperator
  | CurrencyUnit
  | CurrencySubunit
  | KeypadLeftParen
  | KeypadRightParen
  | KeypadLeftBrace
  | KeypadRightBrace
  | KeypadTab
  | KeypadBackSpace
  | KeypadA
  | KeypadB
  | KeypadC
  | KeypadD
  | KeypadE
  | KeypadF
  | KeypadXor
  | KeypadPower
  | KeypadPercent
  | KeypadLess
  | KeypadGreater
  | KeypadAmpersand
  | KeypadDBLAmpersand
  | KeypadVerticalBar
  | KeypadDBLVerticalBar
  | KeypadColon
  | KeypadHash
  | KeypadSpace
  | KeypadAt
  | KeypadExclaim
  | KeypadMemstore
  | KeypadMemRecall
  | KeypadMemClear
  | KeypadMemAdd
  | KeypadMemSubtract
  | KeypadMemMultiply
  | KeypadMemDivide
  | KeypadPlusMinus
  | KeypadClear
  | KeypadClearEntry
  | KeypadBinary
  | KeypadOctal
  | KeypadDecimal
  | KeypadHexaDecimal
  | LCtrl
  | LShift
  | LAlt
  | LGui
  | RCtrl
  | RShift
  | RAlt
  | RGui
  | Mode
  | AudioNext
  | AudioPrev
  | AudioStop
  | AudioPlay
  | AudioMute
  | MediaSelect
  | WWW
  | Mail
  | Calculator
  | Computer
  | AppControlSearch
  | AppControlHome
  | AppControlBack
  | AppControlForward
  | AppControlStop
  | AppControlRefresh
  | AppControlBookmarks
  | BrightnessDown
  | BrightnessUp
  | DisplaySwitch
  | KBDIllumToogle
  | KBDIllumDown
  | KBDIllumUp
  | Eject
  | Sleep

fromKeycode :: Int32 -> Maybe Key
fromKeycode 0 = Just Unkown
fromKeycode 8 = Just Backspace
fromKeycode 9 = Just Tab
fromKeycode 13 = Just Return
fromKeycode 27 = Just Escape
fromKeycode 32 = Just Space
fromKeycode 33 = Just Exclaim
fromKeycode 34 = Just Quotedbl
fromKeycode 35 = Just Hash
fromKeycode 36 = Just Dollar
fromKeycode 37 = Just Percent
fromKeycode 38 = Just Ampersand
fromKeycode 39 = Just Quote
fromKeycode 40 = Just Leftparen
fromKeycode 41 = Just RightParen
fromKeycode 42 = Just Asterisk
fromKeycode 43 = Just Plus
fromKeycode 44 = Just Comma
fromKeycode 45 = Just Minus
fromKeycode 46 = Just Period
fromKeycode 47 = Just Slash
fromKeycode 48 = Just N0
fromKeycode 49 = Just N1
fromKeycode 50 = Just N2
fromKeycode 51 = Just N3
fromKeycode 52 = Just N4
fromKeycode 53 = Just N5
fromKeycode 54 = Just N6
fromKeycode 55 = Just N7
fromKeycode 56 = Just N8
fromKeycode 57 = Just N9
fromKeycode 58 = Just Colon
fromKeycode 59 = Just SemiColon
fromKeycode 60 = Just Less
fromKeycode 61 = Just Equals
fromKeycode 62 = Just Greater
fromKeycode 63 = Just Question
fromKeycode 64 = Just At
fromKeycode 91 = Just LeftBracket
fromKeycode 92 = Just BackSlash
fromKeycode 93 = Just RightBracket
fromKeycode 94 = Just Caret
fromKeycode 95 = Just Underscore
fromKeycode 96 = Just Backquote
fromKeycode 97 = Just CharA
fromKeycode 98 = Just CharB
fromKeycode 99 = Just CharC
fromKeycode 100 = Just CharD
fromKeycode 101 = Just CharE
fromKeycode 102 = Just CharF
fromKeycode 103 = Just CharG
fromKeycode 104 = Just CharH
fromKeycode 105 = Just CharI
fromKeycode 106 = Just CharJ
fromKeycode 107 = Just CharK
fromKeycode 108 = Just CharL
fromKeycode 109 = Just CharM
fromKeycode 110 = Just CharN
fromKeycode 111 = Just CharO
fromKeycode 112 = Just CharP
fromKeycode 113 = Just CharQ
fromKeycode 114 = Just CharR
fromKeycode 115 = Just CharS
fromKeycode 116 = Just CharT
fromKeycode 117 = Just CharU
fromKeycode 118 = Just CharV
fromKeycode 119 = Just CharW
fromKeycode 120 = Just CharX
fromKeycode 121 = Just CharY
fromKeycode 122 = Just CharZ
fromKeycode 127 = Just Delete
fromKeycode 1073741881 = Just CAPSLOCK
fromKeycode 1073741882 = Just F1
fromKeycode 1073741883 = Just F2
fromKeycode 1073741884 = Just F3
fromKeycode 1073741885 = Just F4
fromKeycode 1073741886 = Just F5
fromKeycode 1073741887 = Just F6
fromKeycode 1073741888 = Just F7
fromKeycode 1073741889 = Just F8
fromKeycode 1073741890 = Just F9
fromKeycode 1073741891 = Just F10
fromKeycode 1073741892 = Just F11
fromKeycode 1073741893 = Just F12
fromKeycode 1073741894 = Just PrintScreen
fromKeycode 1073741895 = Just ScrollLock
fromKeycode 1073741896 = Just Pause
fromKeycode 1073741897 = Just Insert
fromKeycode 1073741898 = Just Home
fromKeycode 1073741899 = Just PageUp
fromKeycode 1073741901 = Just End
fromKeycode 1073741902 = Just PageDown
fromKeycode 1073741903 = Just KeyRight
fromKeycode 1073741904 = Just KeyLeft
fromKeycode 1073741905 = Just KeyDown
fromKeycode 1073741906 = Just KeyUp
fromKeycode 1073741907 = Just NumLockClear
fromKeycode 1073741908 = Just KeypadDivide
fromKeycode 1073741909 = Just KeypadMultiply
fromKeycode 1073741910 = Just KeypadMinus
fromKeycode 1073741911 = Just KeypadPlus
fromKeycode 1073741912 = Just KeypadEnter
fromKeycode 1073741913 = Just Keypad1
fromKeycode 1073741914 = Just Keypad2
fromKeycode 1073741915 = Just Keypad3
fromKeycode 1073741916 = Just Keypad4
fromKeycode 1073741917 = Just Keypad5
fromKeycode 1073741918 = Just Keypad6
fromKeycode 1073741919 = Just Keypad7
fromKeycode 1073741920 = Just Keypad8
fromKeycode 1073741921 = Just Keypad9
fromKeycode 1073741922 = Just Keypad0
fromKeycode 1073741923 = Just KeypadPeriod
fromKeycode 1073741925 = Just Application
fromKeycode 1073741926 = Just Power
fromKeycode 1073741927 = Just KeypadEquals
fromKeycode 1073741928 = Just F13
fromKeycode 1073741929 = Just F14
fromKeycode 1073741930 = Just F15
fromKeycode 1073741931 = Just F16
fromKeycode 1073741932 = Just F17
fromKeycode 1073741933 = Just F18
fromKeycode 1073741934 = Just F19
fromKeycode 1073741935 = Just F20
fromKeycode 1073741936 = Just F21
fromKeycode 1073741937 = Just F22
fromKeycode 1073741938 = Just F23
fromKeycode 1073741939 = Just F24
fromKeycode 1073741940 = Just Execute
fromKeycode 1073741941 = Just Help
fromKeycode 1073741942 = Just Menu
fromKeycode 1073741943 = Just Select
fromKeycode 1073741944 = Just Stop
fromKeycode 1073741945 = Just Again
fromKeycode 1073741946 = Just Undo
fromKeycode 1073741947 = Just Cut
fromKeycode 1073741948 = Just Copy
fromKeycode 1073741949 = Just Paste
fromKeycode 1073741950 = Just Find
fromKeycode 1073741951 = Just Mute
fromKeycode 1073741952 = Just VolumeUp
fromKeycode 1073741953 = Just VolumeDown
fromKeycode 1073741957 = Just KeypadComma
fromKeycode 1073741958 = Just KeypadEqualsAS400
fromKeycode 1073741977 = Just AlterASE
fromKeycode 1073741978 = Just SysReq
fromKeycode 1073741979 = Just Cancel
fromKeycode 1073741980 = Just Clear
fromKeycode 1073741981 = Just Prior
fromKeycode 1073741982 = Just Return2
fromKeycode 1073741983 = Just Seperator
fromKeycode 1073741984 = Just Out
fromKeycode 1073741985 = Just Oper
fromKeycode 1073741986 = Just ClearAgain
fromKeycode 1073741987 = Just CRSel
fromKeycode 1073741988 = Just EXSel
fromKeycode 1073742000 = Just Keypad00
fromKeycode 1073742001 = Just Keypad000
fromKeycode 1073742002 = Just ThousandsSeperator
fromKeycode 1073742003 = Just DecimalsSeperator
fromKeycode 1073742004 = Just CurrencyUnit
fromKeycode 1073742005 = Just CurrencySubunit
fromKeycode 1073742006 = Just KeypadLeftParen
fromKeycode 1073742007 = Just KeypadRightParen
fromKeycode 1073742008 = Just KeypadLeftBrace
fromKeycode 1073742009 = Just KeypadRightBrace
fromKeycode 1073742010 = Just KeypadTab
fromKeycode 1073742011 = Just KeypadBackSpace
fromKeycode 1073742012 = Just KeypadA
fromKeycode 1073742013 = Just KeypadB
fromKeycode 1073742014 = Just KeypadC
fromKeycode 1073742015 = Just KeypadD
fromKeycode 1073742016 = Just KeypadE
fromKeycode 1073742017 = Just KeypadF
fromKeycode 1073742018 = Just KeypadXor
fromKeycode 1073742019 = Just KeypadPower
fromKeycode 1073742020 = Just KeypadPercent
fromKeycode 1073742021 = Just KeypadLess
fromKeycode 1073742022 = Just KeypadGreater
fromKeycode 1073742023 = Just KeypadAmpersand
fromKeycode 1073742024 = Just KeypadDBLAmpersand
fromKeycode 1073742025 = Just KeypadVerticalBar
fromKeycode 1073742026 = Just KeypadDBLVerticalBar
fromKeycode 1073742027 = Just KeypadColon
fromKeycode 1073742028 = Just KeypadHash
fromKeycode 1073742029 = Just KeypadSpace
fromKeycode 1073742030 = Just KeypadAt
fromKeycode 1073742031 = Just KeypadExclaim
fromKeycode 1073742032 = Just KeypadMemstore
fromKeycode 1073742033 = Just KeypadMemRecall
fromKeycode 1073742034 = Just KeypadMemClear
fromKeycode 1073742035 = Just KeypadMemAdd
fromKeycode 1073742036 = Just KeypadMemSubtract
fromKeycode 1073742037 = Just KeypadMemMultiply
fromKeycode 1073742038 = Just KeypadMemDivide
fromKeycode 1073742039 = Just KeypadPlusMinus
fromKeycode 1073742040 = Just KeypadClear
fromKeycode 1073742041 = Just KeypadClearEntry
fromKeycode 1073742042 = Just KeypadBinary
fromKeycode 1073742043 = Just KeypadOctal
fromKeycode 1073742044 = Just KeypadDecimal
fromKeycode 1073742045 = Just KeypadHexaDecimal
fromKeycode 1073742048 = Just LCtrl
fromKeycode 1073742049 = Just LShift
fromKeycode 1073742050 = Just LAlt
fromKeycode 1073742051 = Just LGui
fromKeycode 1073742052 = Just RCtrl
fromKeycode 1073742053 = Just RShift
fromKeycode 1073742054 = Just RAlt
fromKeycode 1073742055 = Just RGui
fromKeycode 1073742081 = Just Mode
fromKeycode 1073742082 = Just AudioNext
fromKeycode 1073742083 = Just AudioPrev
fromKeycode 1073742084 = Just AudioStop
fromKeycode 1073742085 = Just AudioPlay
fromKeycode 1073742086 = Just AudioMute
fromKeycode 1073742087 = Just MediaSelect
fromKeycode 1073742088 = Just WWW
fromKeycode 1073742089 = Just Mail
fromKeycode 1073742090 = Just Calculator
fromKeycode 1073742091 = Just Computer
fromKeycode 1073742092 = Just AppControlSearch
fromKeycode 1073742093 = Just AppControlHome
fromKeycode 1073742094 = Just AppControlBack
fromKeycode 1073742095 = Just AppControlForward
fromKeycode 1073742096 = Just AppControlStop
fromKeycode 1073742097 = Just AppControlRefresh
fromKeycode 1073742098 = Just AppControlBookmarks
fromKeycode 1073742099 = Just BrightnessDown
fromKeycode 1073742100 = Just BrightnessUp
fromKeycode 1073742101 = Just DisplaySwitch
fromKeycode 1073742102 = Just KBDIllumToogle
fromKeycode 1073742103 = Just KBDIllumDown
fromKeycode 1073742104 = Just KBDIllumUp
fromKeycode 1073742105 = Just Eject
fromKeycode 1073742106 = Just Sleep
fromKeycode _ = Nothing

keyToChar :: Key -> Maybe Char
keyToChar Tab = Just '\t'
keyToChar Return = Just '\n'
keyToChar Space = Just ' '
keyToChar Exclaim = Just '!'
keyToChar Quotedbl = Just '"'
keyToChar Hash = Just '#'
keyToChar Dollar = Just '$'
keyToChar Percent = Just '%'
keyToChar Ampersand = Just '&'
keyToChar Quote = Just '\''
keyToChar Leftparen = Just '('
keyToChar RightParen = Just ')'
keyToChar Asterisk = Just '*'
keyToChar Plus = Just '+'
keyToChar Comma = Just ','
keyToChar Minus = Just '-'
keyToChar Period = Just '.'
keyToChar Slash = Just '/'
keyToChar N0 = Just '0'
keyToChar N1 = Just '1'
keyToChar N2 = Just '2'
keyToChar N3 = Just '3'
keyToChar N4 = Just '4'
keyToChar N5 = Just '5'
keyToChar N6 = Just '6'
keyToChar N7 = Just '7'
keyToChar N8 = Just '8'
keyToChar N9 = Just '9'
keyToChar Colon = Just ':'
keyToChar SemiColon = Just ';'
keyToChar Less = Just '<'
keyToChar Equals = Just '='
keyToChar Greater = Just '>'
keyToChar Question = Just '?'
keyToChar At = Just '@'
keyToChar LeftBracket = Just '['
keyToChar BackSlash = Just '\\'
keyToChar RightBracket = Just ']'
keyToChar Caret = Just '^'
keyToChar Underscore = Just '_'
keyToChar Backquote = Just '`'
keyToChar CharA = Just 'a'
keyToChar CharB = Just 'b'
keyToChar CharC = Just 'c'
keyToChar CharD = Just 'd'
keyToChar CharE = Just 'e'
keyToChar CharF = Just 'f'
keyToChar CharG = Just 'g'
keyToChar CharH = Just 'h'
keyToChar CharI = Just 'i'
keyToChar CharJ = Just 'j'
keyToChar CharK = Just 'k'
keyToChar CharL = Just 'l'
keyToChar CharM = Just 'm'
keyToChar CharN = Just 'n'
keyToChar CharO = Just 'o'
keyToChar CharP = Just 'p'
keyToChar CharQ = Just 'q'
keyToChar CharR = Just 'r'
keyToChar CharS = Just 's'
keyToChar CharT = Just 't'
keyToChar CharU = Just 'u'
keyToChar CharV = Just 'v'
keyToChar CharW = Just 'w'
keyToChar CharX = Just 'x'
keyToChar CharY = Just 'y'
keyToChar CharZ = Just 'z'
keyToChar Delete = Just '\177'
keyToChar _ = Nothing
