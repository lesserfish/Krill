module Frontend.KeyASCII where

import SDL
import Data.Word

data KeyMod = KM_ALONE | KM_CTRL | KM_SHIFT | KM_BOTH

keyMod :: (Bool, Bool) -> KeyMod
keyMod (False, False) = KM_ALONE
keyMod (True, False) = KM_CTRL
keyMod (False, True) = KM_SHIFT
keyMod (True, True) = KM_BOTH

keyASCII :: KeyMod -> Keycode -> Maybe Word8
keyASCII _ KeycodeSpace     = Just 0xA0
keyASCII _ Keycode0         = Just 0xB0

keyASCII KM_ALONE Keycode1  = Just 0xB1
keyASCII KM_CTRL Keycode1   = Just 0xB1
keyASCII KM_SHIFT Keycode1  = Just 0xA1
keyASCII KM_BOTH Keycode1   = Just 0xA1

keyASCII KM_ALONE Keycode2  = Just 0xB2
keyASCII KM_CTRL Keycode2   = Just 0xB2
keyASCII KM_SHIFT Keycode2  = Just 0xA2
keyASCII KM_BOTH Keycode2   = Just 0xA2

keyASCII KM_ALONE Keycode3  = Just 0xB3
keyASCII KM_CTRL Keycode3   = Just 0xB3
keyASCII KM_SHIFT Keycode3  = Just 0xA3
keyASCII KM_BOTH Keycode3   = Just 0xA3

keyASCII KM_ALONE Keycode4  = Just 0xB4
keyASCII KM_CTRL Keycode4   = Just 0xB4
keyASCII KM_SHIFT Keycode4  = Just 0xA4
keyASCII KM_BOTH Keycode4   = Just 0xA4

keyASCII KM_ALONE Keycode5  = Just 0xB5
keyASCII KM_CTRL Keycode5   = Just 0xB5
keyASCII KM_SHIFT Keycode5  = Just 0xA5
keyASCII KM_BOTH Keycode5   = Just 0xA5

keyASCII KM_ALONE Keycode6  = Just 0xB6
keyASCII KM_CTRL Keycode6   = Just 0xB6
keyASCII KM_SHIFT Keycode6  = Just 0xA6
keyASCII KM_BOTH Keycode6   = Just 0xA6

keyASCII KM_ALONE Keycode7  = Just 0xB7
keyASCII KM_CTRL Keycode7   = Just 0xB7
keyASCII KM_SHIFT Keycode7  = Just 0xA7
keyASCII KM_BOTH Keycode7   = Just 0xA7

keyASCII KM_ALONE Keycode8  = Just 0xB8
keyASCII KM_CTRL Keycode8   = Just 0xB8
keyASCII KM_SHIFT Keycode8  = Just 0xA8
keyASCII KM_BOTH Keycode8   = Just 0xA8

keyASCII KM_ALONE Keycode9  = Just 0xB9
keyASCII KM_CTRL Keycode9   = Just 0xB9
keyASCII KM_SHIFT Keycode9  = Just 0xA9
keyASCII KM_BOTH Keycode9   = Just 0xA9

keyASCII KM_ALONE KeycodeColon  = Just 0xBA
keyASCII KM_CTRL KeycodeColon   = Just 0xBA
keyASCII KM_SHIFT KeycodeColon  = Just 0xAA
keyASCII KM_BOTH KeycodeColon   = Just 0xAA

keyASCII KM_ALONE KeycodeSemicolon  = Just 0xBB
keyASCII KM_CTRL KeycodeSemicolon   = Just 0xBB
keyASCII KM_SHIFT KeycodeSemicolon  = Just 0xAB
keyASCII KM_BOTH KeycodeSemicolon   = Just 0xAB

keyASCII KM_ALONE KeycodeComma  = Just 0xBC
keyASCII KM_CTRL KeycodeComma   = Just 0xBC
keyASCII KM_SHIFT KeycodeComma  = Just 0xAC
keyASCII KM_BOTH KeycodeComma   = Just 0xAC

keyASCII KM_ALONE KeycodeMinus  = Just 0xBD
keyASCII KM_CTRL KeycodeMinus   = Just 0xBD
keyASCII KM_SHIFT KeycodeMinus  = Just 0xAD
keyASCII KM_BOTH KeycodeMinus   = Just 0xAD

keyASCII KM_ALONE KeycodePeriod  = Just 0xBC
keyASCII KM_CTRL KeycodePeriod   = Just 0xBC
keyASCII KM_SHIFT KeycodePeriod  = Just 0xAC
keyASCII KM_BOTH KeycodePeriod   = Just 0xAC

keyASCII KM_ALONE KeycodeSlash  = Just 0xBD
keyASCII KM_CTRL KeycodeSlash   = Just 0xBD
keyASCII KM_SHIFT KeycodeSlash  = Just 0xAD
keyASCII KM_BOTH KeycodeSlash   = Just 0xAD

keyASCII KM_ALONE KeycodeA = Just 0xC1
keyASCII KM_CTRL KeycodeA  = Just 0x81
keyASCII KM_SHIFT KeycodeA = Just 0xC1
keyASCII KM_BOTH KeycodeA  = Just 0x81

keyASCII KM_ALONE KeycodeB = Just 0xC2
keyASCII KM_CTRL KeycodeB  = Just 0x82
keyASCII KM_SHIFT KeycodeB = Just 0xC2
keyASCII KM_BOTH KeycodeB  = Just 0x82

keyASCII KM_ALONE KeycodeC = Just 0xC3
keyASCII KM_CTRL KeycodeC  = Just 0x83
keyASCII KM_SHIFT KeycodeC = Just 0xC3
keyASCII KM_BOTH KeycodeC  = Just 0x83

keyASCII KM_ALONE KeycodeD = Just 0xC4
keyASCII KM_CTRL KeycodeD  = Just 0x84
keyASCII KM_SHIFT KeycodeD = Just 0xC4
keyASCII KM_BOTH KeycodeD  = Just 0x84

keyASCII KM_ALONE KeycodeE = Just 0xC5
keyASCII KM_CTRL KeycodeE  = Just 0x85
keyASCII KM_SHIFT KeycodeE = Just 0xC5
keyASCII KM_BOTH KeycodeE  = Just 0x85

keyASCII KM_ALONE KeycodeF = Just 0xC6
keyASCII KM_CTRL KeycodeF  = Just 0x86
keyASCII KM_SHIFT KeycodeF = Just 0xC6
keyASCII KM_BOTH KeycodeF  = Just 0x86

keyASCII _ KeycodeReturn = Just 0x8D

keyASCII KM_ALONE KeycodeG = Just 0xC7
keyASCII KM_CTRL KeycodeG  = Just 0x87
keyASCII KM_SHIFT KeycodeG = Just 0xC7
keyASCII KM_BOTH KeycodeG  = Just 0x87

keyASCII KM_ALONE KeycodeH = Just 0xC8
keyASCII KM_CTRL KeycodeH  = Just 0x88
keyASCII KM_SHIFT KeycodeH = Just 0xC8
keyASCII KM_BOTH KeycodeH  = Just 0x88

keyASCII KM_ALONE KeycodeI = Just 0xC9
keyASCII KM_CTRL KeycodeI  = Just 0x89
keyASCII KM_SHIFT KeycodeI = Just 0xC9
keyASCII KM_BOTH KeycodeI  = Just 0x89

keyASCII KM_ALONE KeycodeJ = Just 0xCA
keyASCII KM_CTRL KeycodeJ  = Just 0x8A
keyASCII KM_SHIFT KeycodeJ = Just 0xCA
keyASCII KM_BOTH KeycodeJ  = Just 0x8A

keyASCII KM_ALONE KeycodeK = Just 0xCB
keyASCII KM_CTRL KeycodeK  = Just 0x8B
keyASCII KM_SHIFT KeycodeK = Just 0xCB
keyASCII KM_BOTH KeycodeK  = Just 0x8B

keyASCII KM_ALONE KeycodeL = Just 0xCC
keyASCII KM_CTRL KeycodeL  = Just 0x8C
keyASCII KM_SHIFT KeycodeL = Just 0xCC
keyASCII KM_BOTH KeycodeL  = Just 0x8C

keyASCII KM_ALONE KeycodeM = Just 0xCD
keyASCII KM_CTRL KeycodeM  = Just 0x8D
keyASCII KM_SHIFT KeycodeM = Just 0xCD
keyASCII KM_BOTH KeycodeM  = Just 0x8D

keyASCII KM_ALONE KeycodeN = Just 0xCE
keyASCII KM_CTRL KeycodeN  = Just 0x8E
keyASCII KM_SHIFT KeycodeN = Just 0xCE
keyASCII KM_BOTH KeycodeN  = Just 0x8E

keyASCII KM_ALONE KeycodeO = Just 0xCF
keyASCII KM_CTRL KeycodeO  = Just 0x8F
keyASCII KM_SHIFT KeycodeO = Just 0xCF
keyASCII KM_BOTH KeycodeO  = Just 0x8F

keyASCII KM_ALONE KeycodeP = Just 0xD0
keyASCII KM_CTRL KeycodeP  = Just 0x90
keyASCII KM_SHIFT KeycodeP = Just 0xC0
keyASCII KM_BOTH KeycodeP  = Just 0x80

keyASCII KM_ALONE KeycodeQ = Just 0xD1
keyASCII KM_CTRL KeycodeQ  = Just 0x91
keyASCII KM_SHIFT KeycodeQ = Just 0xD1
keyASCII KM_BOTH KeycodeQ  = Just 0x91

keyASCII KM_ALONE KeycodeR = Just 0xD2
keyASCII KM_CTRL KeycodeR  = Just 0x92
keyASCII KM_SHIFT KeycodeR = Just 0xD2
keyASCII KM_BOTH KeycodeR  = Just 0x92

keyASCII KM_ALONE KeycodeS = Just 0xD3
keyASCII KM_CTRL KeycodeS  = Just 0x93
keyASCII KM_SHIFT KeycodeS = Just 0xD3
keyASCII KM_BOTH KeycodeS  = Just 0x93

keyASCII KM_ALONE KeycodeT = Just 0xD4
keyASCII KM_CTRL KeycodeT  = Just 0x94
keyASCII KM_SHIFT KeycodeT = Just 0xD4
keyASCII KM_BOTH KeycodeT  = Just 0x94

keyASCII KM_ALONE KeycodeU = Just 0xD5
keyASCII KM_CTRL KeycodeU  = Just 0x95
keyASCII KM_SHIFT KeycodeU = Just 0xD5
keyASCII KM_BOTH KeycodeU  = Just 0x95

keyASCII KM_ALONE KeycodeV = Just 0xD6
keyASCII KM_CTRL KeycodeV  = Just 0x96
keyASCII KM_SHIFT KeycodeV = Just 0xD6
keyASCII KM_BOTH KeycodeV  = Just 0x96

keyASCII KM_ALONE KeycodeW = Just 0xD7
keyASCII KM_CTRL KeycodeW  = Just 0x97
keyASCII KM_SHIFT KeycodeW = Just 0xD7
keyASCII KM_BOTH KeycodeW  = Just 0x97

keyASCII KM_ALONE KeycodeX = Just 0xD8
keyASCII KM_CTRL KeycodeX  = Just 0x98
keyASCII KM_SHIFT KeycodeX = Just 0xD8
keyASCII KM_BOTH KeycodeX  = Just 0x98

keyASCII KM_ALONE KeycodeY = Just 0xD9
keyASCII KM_CTRL KeycodeY  = Just 0x99
keyASCII KM_SHIFT KeycodeY = Just 0xD9
keyASCII KM_BOTH KeycodeY  = Just 0x99

keyASCII KM_ALONE KeycodeZ = Just 0xDa
keyASCII KM_CTRL KeycodeZ  = Just 0x9A
keyASCII KM_SHIFT KeycodeZ = Just 0xDA
keyASCII KM_BOTH KeycodeZ  = Just 0x9A

keyASCII KM_BOTH KeycodeLeft  = Just 0x88

keyASCII KM_BOTH KeycodeRight  = Just 0x95

keyASCII KM_BOTH KeycodeEscape  = Just 0x9B

keyASCII _ _ = Nothing
