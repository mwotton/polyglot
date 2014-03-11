{-# LANGUAGE OverloadedStrings #-}
module PolyglotSpec where
import           Test.Hspec

import           Language.Polyglot

spec = describe "Language.Polyglot" $ do
  it "should distinguish english and vietnamese" $ do
    let model = build [("en", "A cat sat on the mat. Many hands make light work. Elephants are small and partial to cheese"),
           ("vn", "Đứa bé cũng không biết là mình sẽ trở thành món đồ chơi của thằng anh họ Dudley, bi nó tha hồ ngắt véo trong vài tuần lễ sau đó. Đứa bé không hề biết gì về những điều đó trong lúc này, cái lúc này mà khắp nơi trên cả nước, tiệc tùng linh đình đang diễn ra, người người đều \nnâng ly chúc tụng: “Uống mừng Harry Potter! Đứa bé vẫn sống!")]
    predict model "Fasten seat belt while seated, life vest under centre armrest" `shouldBe` "en"
    predict model "Tới góc đường, cụ dừng bước, lấy trong áo trùm ra cái tắt - lửa bằng bạ" `shouldBe` "vn"
    predict model "angela likes to eat pies" `shouldBe` "en"
    predict model "Cụ giơ lên bấm nó một cái, rồi mười hai cái, lập tức mười hai cái bóng đèn trên đường Privet Drive bật sáng, nhưng cũng" `shouldBe` "vn"
