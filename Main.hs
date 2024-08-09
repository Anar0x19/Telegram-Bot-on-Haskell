{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (command)

-- Responses for the how_are_you_bucefal command
howAreYouResponses :: [Text]
howAreYouResponses =
  [ "Спасибо, сэр! сегодня все хорошо"
  , "Спасибо, сэр! сегодня все нормально"
  , "Спасибо, сэр! сегодня слава Богу все не плохо"
  , "Спасибо, сэр! живем еще"
  ]

-- Responses for the anekdot command
anekdotResponses :: [Text]
anekdotResponses =
  [ "Колобок повесился"
  , "Оптимист: стакан на половину заполнен водой, Пессимист: стакан на половину пуст, Программист: этот стакан помечен как дубликат"
  , "Россия в скором времени установит ракеты в Никарагуа и Венесуэле, которые ни в коем случае не будут направлены против США, они предназначены для борьбы с морским пиратством в Монголии"
  ]

data BotModel = BotModel
  deriving (Show)

data Action
  = GoodJobBucefal
  | BadJobBucefal
  | HowAreYouBucefal
  | WhoYourCreator
  | Bucefal
  | YourName
  | Anekdot
  deriving (Show)

botApp :: BotApp BotModel Action
botApp = BotApp
  { botInitialModel = BotModel
  , botAction = \update _model -> handleUpdate update
  , botHandler = handleAction
  , botJobs = []
  }

handleUpdate :: Update -> Action
handleUpdate = parseUpdate
  $ GoodJobBucefal <$ command "good_job_bucefal"
 <|> BadJobBucefal <$ command "bad_job_bucefal"
 <|> HowAreYouBucefal <$ command "how_are_you_bucefal"
 <|> WhoYourCreator <$ command "who_your_creator"
 <|> Bucefal <$ command "bucefal"
 <|> YourName <$ command "your_name"
 <|> Anekdot <$ command "anekdot"

handleAction :: Action -> BotModel -> Eff Action BotModel
handleAction GoodJobBucefal model = model <# do
  replyText "Спасибо, сэр! всегда к вашим услугам."
  pure model
handleAction BadJobBucefal model = model <# do
  replyText "Извините сэр! я буду стараться лучше."
  pure model
handleAction HowAreYouBucefal model = model <# do
  response <- liftIO $ randomChoice howAreYouResponses
  replyText response
  pure model
handleAction WhoYourCreator model = model <# do
  replyText "Мой создатель - сэр @super_icosahedron !!!"
  pure model
handleAction Bucefal model = model <# do
  replyText "Буцефал готов к вашим услугам, сэр!"
  pure model
handleAction YourName model = model <# do
  replyText "Сэр! мое имя посвящено коню Сашки Македонского"
  pure model
handleAction Anekdot model = model <# do
  response <- liftIO $ randomChoice anekdotResponses
  replyText response
  pure model

randomChoice :: [a] -> IO a
randomChoice xs = do
  index <- randomRIO (0, length xs - 1)
  return (xs !! index)

main :: IO ()
main = do
  let token = Token ""
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId botApp) env
