module DitsAndDahs.MorseCodeSpec (spec) where

import DitsAndDahs.MorseCode
import Test.Hspec

spec :: Spec
spec = describe "DitsAndDahs" $ do
  describe "Character to Morse Code" $ do
    it "A -> .-" $ do
      textToMorseCode "A" `shouldBe` ".-"
    it "B -> -..." $ do
      textToMorseCode "B" `shouldBe` "-..."
    it "C -> -.-." $ do
      textToMorseCode "C" `shouldBe` "-.-."
    it "D -> -.." $ do
      textToMorseCode "D" `shouldBe` "-.."
    it "E -> ." $ do
      textToMorseCode "E" `shouldBe` "."
    it "F -> ..-." $ do
      textToMorseCode "F" `shouldBe` "..-."
    it "G -> --." $ do
      textToMorseCode "G" `shouldBe` "--."
    it "H -> ...." $ do
      textToMorseCode "H" `shouldBe` "...."
    it "I -> .." $ do
      textToMorseCode "I" `shouldBe` ".."
    it "J -> .---" $ do
      textToMorseCode "J" `shouldBe` ".---"
    it "K -> -.-" $ do
      textToMorseCode "K" `shouldBe` "-.-"
    it "L -> .-.." $ do
      textToMorseCode "L" `shouldBe` ".-.."
    it "M -> --" $ do
      textToMorseCode "M" `shouldBe` "--"
    it "N -> -." $ do
      textToMorseCode "N" `shouldBe` "-."
    it "O -> ---" $ do
      textToMorseCode "O" `shouldBe` "---"
    it "P -> .--." $ do
      textToMorseCode "P" `shouldBe` ".--."
    it "Q -> --.-" $ do
      textToMorseCode "Q" `shouldBe` "--.-"
    it "R -> .-." $ do
      textToMorseCode "R" `shouldBe` ".-."
    it "S -> ..." $ do
      textToMorseCode "S" `shouldBe` "..."
    it "T -> -" $ do
      textToMorseCode "T" `shouldBe` "-"
    it "U -> ..-" $ do
      textToMorseCode "U" `shouldBe` "..-"
    it "V -> ...-" $ do
      textToMorseCode "V" `shouldBe` "...-"
    it "W -> .--" $ do
      textToMorseCode "W" `shouldBe` ".--"
    it "X -> -..-" $ do
      textToMorseCode "X" `shouldBe` "-..-"
    it "Y -> -.--" $ do
      textToMorseCode "Y" `shouldBe` "-.--"
    it "Z -> --.." $ do
      textToMorseCode "Z" `shouldBe` "--.."
    it "0 -> -----" $ do
      textToMorseCode "0" `shouldBe` "-----"
    it "1 -> .----" $ do
      textToMorseCode "1" `shouldBe` ".----"
    it "2 -> ..---" $ do
      textToMorseCode "2" `shouldBe` "..---"
    it "3 -> ...--" $ do
      textToMorseCode "3" `shouldBe` "...--"
    it "4 -> ....-" $ do
      textToMorseCode "4" `shouldBe` "....-"
    it "5 -> ....." $ do
      textToMorseCode "5" `shouldBe` "....."
    it "6 -> -...." $ do
      textToMorseCode "6" `shouldBe` "-...."
    it "7 -> --..." $ do
      textToMorseCode "7" `shouldBe` "--..."
    it "8 -> ---.." $ do
      textToMorseCode "8" `shouldBe` "---.."
    it "9 -> ----." $ do
      textToMorseCode "9" `shouldBe` "----."
    it "space -> /" $ do
      textToMorseCode " " `shouldBe` "/"
    it "unknown -> \0" $ do
      textToMorseCode "!" `shouldBe` "\0"

  describe "Text to Morse Code" $ do
    it "Hello -> .... . .-.. .-.. ---" $ do
      textToMorseCode "Hello" `shouldBe` ".... . .-.. .-.. ---"
    it "Hello World -> .... . .-.. .-.. --- / .-- --- .-. .-.. -.." $ do
      textToMorseCode "Hello World" `shouldBe` ".... . .-.. .-.. --- / .-- --- .-. .-.. -.."

  describe "Morse Code to Character" $ do
    it ".- -> A" $ do
      morseCodeToText ".-" `shouldBe` "A"
    it "-... -> B" $ do
      morseCodeToText "-..." `shouldBe` "B"
    it "-.-. -> C" $ do
      morseCodeToText "-.-." `shouldBe` "C"
    it "-.. -> D" $ do
      morseCodeToText "-.." `shouldBe` "D"
    it ". -> E" $ do
      morseCodeToText "." `shouldBe` "E"
    it "..-. -> F" $ do
      morseCodeToText "..-." `shouldBe` "F"
    it "--. -> G" $ do
      morseCodeToText "--." `shouldBe` "G"
    it ".... -> H" $ do
      morseCodeToText "...." `shouldBe` "H"
    it ".. -> I" $ do
      morseCodeToText ".." `shouldBe` "I"
    it ".--- -> J" $ do
      morseCodeToText ".---" `shouldBe` "J"
    it "-.- -> K" $ do
      morseCodeToText "-.-" `shouldBe` "K"
    it ".-.. -> L" $ do
      morseCodeToText ".-.." `shouldBe` "L"
    it "-- -> M" $ do
      morseCodeToText "--" `shouldBe` "M"
    it "-. -> N" $ do
      morseCodeToText "-." `shouldBe` "N"
    it "--- -> O" $ do
      morseCodeToText "---" `shouldBe` "O"
    it ".--. -> P" $ do
      morseCodeToText ".--." `shouldBe` "P"
    it "--.- -> Q" $ do
      morseCodeToText "--.-" `shouldBe` "Q"
    it ".-. -> R" $ do
      morseCodeToText ".-." `shouldBe` "R"
    it "... -> S" $ do
      morseCodeToText "..." `shouldBe` "S"
    it "- -> T" $ do
      morseCodeToText "-" `shouldBe` "T"
    it "..- -> U" $ do
      morseCodeToText "..-" `shouldBe` "U"
    it "...- -> V" $ do
      morseCodeToText "...-" `shouldBe` "V"
    it ".-- -> W" $ do
      morseCodeToText ".--" `shouldBe` "W"
    it "-..- -> X" $ do
      morseCodeToText "-..-" `shouldBe` "X"
    it "-.-- -> Y" $ do
      morseCodeToText "-.--" `shouldBe` "Y"
    it "--.. -> Z" $ do
      morseCodeToText "--.." `shouldBe` "Z"
    it "----- -> 0" $ do
      morseCodeToText "-----" `shouldBe` "0"
    it ".---- -> 1" $ do
      morseCodeToText ".----" `shouldBe` "1"
    it "..--- -> 2" $ do
      morseCodeToText "..---" `shouldBe` "2"
    it "...-- -> 3" $ do
      morseCodeToText "...--" `shouldBe` "3"
    it "....- -> 4" $ do
      morseCodeToText "....-" `shouldBe` "4"
    it "..... -> 5" $ do
      morseCodeToText "....." `shouldBe` "5"
    it "-.... -> 6" $ do
      morseCodeToText "-...." `shouldBe` "6"
    it "--... -> 7" $ do
      morseCodeToText "--..." `shouldBe` "7"
    it "---.. -> 8" $ do
      morseCodeToText "---.." `shouldBe` "8"
    it "----. -> 9" $ do
      morseCodeToText "----." `shouldBe` "9"
    it "/ -> space" $ do
      morseCodeToText "/" `shouldBe` " "

  describe "Morse Code to Text" $ do
    it ".... . .-.. .-.. --- -> HELLO" $ do
      morseCodeToText ".... . .-.. .-.. ---" `shouldBe` "HELLO"
    it ".... . .-.. .-.. --- / .-- --- .-. .-.. -.. -> HELLO WORLD" $ do
      morseCodeToText ".... . .-.. .-.. --- / .-- --- .-. .-.. -.." `shouldBe` "HELLO WORLD"
