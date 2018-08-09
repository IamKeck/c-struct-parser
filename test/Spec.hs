import Test.Hspec
import Text.Parsec (parse)
import Lib
import Data.Either (isLeft)

main :: IO ()
main = hspec $ do
    describe "name parser" $ do
        it "normal" $
            parse name "" "LPTSTR" `shouldBe` Right "LPTSTR"
        it "normal2" $
            parse name "" "hoge_fuga" `shouldBe` Right "hoge_fuga"
        it "normal3" $
            parse name "" "hoge_fuga2" `shouldBe` Right "hoge_fuga2"
        it "with space" $
            parse name "" "hoge fuga2" `shouldBe` Right "hoge"
    describe "line test" $ do
        it "normal" $
            parse line "" "LPTSTR hoge;" `shouldBe` Right (Member "LPTSTR" "hoge" 0)
        it "with space" $
            parse line "" "   LPTSTR   hoge; " `shouldBe` Right (Member "LPTSTR" "hoge" 0)
        it "array" $
            parse line "" "WORD hoge[23]; " `shouldBe` Right (Member "WORD" "hoge" 23)
    describe "typedef test" $ do
        it "normal" $
            let
                raw = "typedef struct _GUID {\n\
                    \      DWORD DATA1;\n\
                    \      WORD  DATA2;\n\
                    \      WORD  DATA3;\n\
                    \      WORD  DATA4[8];\n\
                    \       } GUID;"
                ans = Struct "GUID" [ Member "DWORD" "DATA1" 0
                                    , Member "WORD" "DATA2" 0
                                    , Member "WORD" "DATA3" 0
                                    , Member "WORD" "DATA4" 8 ]
            in
                parse typedefStruct "" raw `shouldBe` Right ans



        
