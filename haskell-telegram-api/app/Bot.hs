{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import           Network.HTTP.Client      (newManager, managerSetProxy)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Control.Concurrent
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Char8 as Char8 (pack, unpack, dropWhile, reverse)
import           Data.Text as T (pack, unpack)
import           Database.PostgreSQL.Simple
import           GHC.Int
import           Data.List.Split
import           Data.String
import           Network.Wreq
import           Control.Lens
import           Data.Char (isSpace)
import           Data.Time.Clock



myToken :: Token
myToken = Token "bot789825659:AAFrTqMAKl3zmK_x05cOm66KmRpiTREWETU"

postgresConnection = connectPostgreSQL "host='localhost' port=5432 dbname='postgres' user='ilya' password='qwerty'"

myThreadDelay = 1000000

adminId = 129563384

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings

  conn <- postgresConnection

  print "start!"

  myResult <- runTelegramClient myToken manager $ do
  
    let messageRequest = sendMessageRequest (ChatId adminId) "Bot started!"
  
    let myGetUpdatesRequest = getUpdatesRequest
  
    updates <- getUpdatesM myGetUpdatesRequest
    
    sendMessageM messageRequest

  case myResult of 
    Left error -> print "Request failed"
    Right response -> print ((chat_id (chat (result response))))

  isContinuing <- myCycle 0
  
  print myResult
  
  print "finish!"



myCycle :: Int -> IO ()
myCycle x = do

  delay <- threadDelay myThreadDelay

  manager <- newManager tlsManagerSettings

  myResult <- runTelegramClient myToken manager $ do
    let getUpdatedRequest = GetUpdatesRequest (Just x) Nothing Nothing Nothing
    
    getUpdatesM getUpdatedRequest

  case myResult of
    Right response -> do
      let updates = result response
  
      let updatesSize = length updates

      if updatesSize > 0 then do
        let update = (head updates)

        isUpdated <- telegramUpdate update
  
        myCycle ((update_id update) + 1)

      else (myCycle x)

    _ -> print "No updates"

  print "Finish!"


telegramUpdate update = do
  manager <- newManager tlsManagerSettings

  let mes = (message update)

  case mes of
    Just m -> do
      let id = chat_id (chat m)

      let myText = text m

      case myText of
        Just text -> do
          res <- botBody (fromIntegral id) (T.unpack text)
          
          print "Message sent!"
        Nothing -> print "No message!"
    
    Nothing -> print "No message!"


botBody id text = do
    conn <- postgresConnection

    lastmes <- getLastmes id

    print lastmes

    if text == "/start" then do
      res <- inDataBase id

      username <- getUsername id
      
      if not res then do
        execute conn "insert into users (name, lastmes, user_id) values (?, ?, ?)" (username :: String, text :: String, id :: Int) :: IO GHC.Int.Int64

        print (show "Not in bd!")
      else print "In bd!"

      mySendMessageWithButtons (fromIntegral id) "Приветствую тебя, друг!👋\n\n⚡️Я бот HiddenLinks и я единственный в природе бот, который прячет ссылки с таких файлобменников как:\n• 📀YandexDisk\n• ☁️Облако Mail  \n• 🛰Google Drive\n\n💁♂️Для чего я это делаю? Все чаще и чаще рабочие ссылки с инфопродуктами блокируются правообладателями, и это сильно затрудняет обучение. \n\n😎Именно поэтому приходится прятать ссылки от злодеев, которые находятся всегда на страже и радовать вас бесперебойным доступом к любимым материалам.\n\nУзнать как мной пользоваться вы сможете нажав в навигации кнопку ❓FAQ" startButtons
    
    else do
      case text of
        "🙏Получить ID" -> do
          updateLastmes "get_id" id
          
          print "get_id"
          
          mySendMessageHTML (fromIntegral id) "🕹 Отправьте мне ссылку на <b>Yandex.Disc либо Cloud.Mail</b>, чтобы я смог добавить ее в базу данных ссылок.\n\nЯ выдам вам <b>персональный ID</b> для быстрого доступа к этой ссылке"
        "☁️Скачать по ID" -> do
          updateLastmes "get_link" id
          
          print "get_link"
          
          mySendMessageHTML (fromIntegral id) "🕹 Отправьте мне ID в формате: <b>#540</b> чтобы получить <b>приватную ссылку</b> для скачивания соответствующего материала."
        "❓FAQ" -> do
          print "faq"
          
          mySendMessageHTML (fromIntegral id) "❓<b>FAQ (Как пользоваться ботом)</b>\n\n❗️Теперь вы можете делиться или скачивать курсы через меня.\n\nДля того, чтобы скачать курс, необходимо нажать на кнопку ☁️<b>Скачать по ID</b>, написать мне номер <b>(Пример: #540)</b>, и загрузка курса начнется в вашем браузере автоматически.\n\nЕсли вы хотите самостоятельно преобразовать ссылку (которая есть только у вас) в ID, чтобы делиться ей с друзьями, нажмите на кнопку\n🙏Получить ID и отправьте мне вашу ссылку (Пример: https://disk.yandex.ru/d/gggggggggg)\nЯ выдам вам уникальный id номер, по которому можно скрытно скачать любой файл с облака, при этом ссылка останется в тайне."
        "☎️Тех.Поддержка" -> do
          print "support"
          
          mySendMessageHTML (fromIntegral id) "<b> 📌 По всем интересующим вас вопросам, обращайтесь к нашему менеджеру</b> ➡️ @MarshMallowBitch"
        "/add" -> do
          print "adding title"

          updateLastmes "/add" id

          mySendMessageHTML (fromIntegral id) "Введите номер курса в формате #540 и далее через пробел название, которое хотите присвоить данному курсу."
        "📕 Библиотека" -> do
          print "library"

          titled_links <- getTitledLinks

          let ans = createLibraryReply titled_links
          
          mySendMessageHTML (fromIntegral id) (T.pack ans)
          
          print (show ans)          
         
        "📊 Статистика" -> do
          print "stats"

          reply <- getStats 

          mySendMessageHTML (fromIntegral id) (T.pack reply)

        _ -> do
          case lastmes of 
            "get_id" -> do
          
              let pos = findStr "cloud.mail.ru" text
          
              case pos of
                Just p -> do
          
                  print "found"
          
                  js <- getJSON text
          
                  curTime <- getCurrentTime
          
                  let timed = floor $ utctDayTime curTime :: Int
          
                  execute conn "insert into links (link, json_result, time) values (?, ?, ?)" (text :: String, js :: String, timed :: Int) :: IO GHC.Int.Int64
                  
                  xs <- query_ conn "select id from links order by id desc limit 1" :: IO [Only Int]
                  
                  let last_id = (show (fromOnly (head xs)) :: String)
                  
                  let reply = "Ваша ссылка на материал на облачном хранилище успешно добавлена в базу!\n\nПерсональный ID для данной ссылки: #" ++ last_id
                  
                  mySendMessageHTML (fromIntegral id) (T.pack reply)
                  
                  updateLastmes text id
                  
                  print "success"
                Nothing -> do
                  print "not found"
                  
                  mySendMessageHTML (fromIntegral id) "❗️ Неверный формат ссылки! Попробуйте еще раз!"
            
            "get_link" -> do
              if substring 0 1 text == "#" then do
                
                let link_id = substring 1 (length text) text
            
                link <- getLinkById link_id
                
                let opts = defaults & param "link" .~ [T.pack link]
            
                request <- getWith opts "https://bestproger.ru/abot/linker.php"
                
                let names = request ^.. responseBody . values . key "name" . _String
                let links = request ^.. responseBody . values . key "link" . _String
                let sizes = request ^.. responseBody . values . key "size" . _String
                
                let glowed_links = sumStringList links

                let params = defaults & param "links" .~ [T.pack glowed_links]
                                      & param "id" .~ [T.pack (link_id)]
            
                response <- getWith params "https://bestproger.ru/abot/get_links.php"

                let reply = sumList names sizes links link_id 0

                mySendMessageHTML (fromIntegral id) (T.pack reply)

                updateLastmes text id

                print (response)
            
              else do
                mySendMessageHTML (fromIntegral id) "❗️ Неверный формат ID ссылки! Попробуйте еще раз!"
            
                print "Wrong ID"
            
            "/add" -> do
              
              let splited_text = mysplit text
              
              let link_id_full = getByIndex splited_text 0
              
              let title = getByIndex splited_text 1
              
              let link_id = (substring 1 (length link_id_full) link_id_full)
              
              rows <- execute conn "update links set title=? where id=?" (text :: String, link_id :: String) :: IO GHC.Int.Int64

              updateLastmes text id

              let reply = "Описание к материалу с ID " ++ link_id_full ++ " успешно добавлено!"

              mySendMessageHTML (fromIntegral id) (T.pack reply)

              print title
            _ -> print "Not in case"


getByIndex (x : xs) index = if index == 0 then x else getByIndex xs (index - 1)

myslice from to xs = take (to - from + 1) (drop from xs)

mysplit :: String -> [String]
mysplit [] = [""]
mysplit (c:cs) | c == ' '  = "" : rest
               | otherwise = (c : head rest) : tail rest
    where rest = mysplit cs


sumStringList [] = ""
sumStringList (x : xs) = (T.unpack x) ++ ":::" ++ sumStringList xs


sumList [] [] [] link_id count = ""
sumList (x : xs) (z : zs) (y: ys) link_id count = "<b>" ++ (T.unpack x) ++ " (" ++ (T.unpack z) ++ ")</b>\n" ++ "https://bestproger.ru/abot/downloader.php?id=" ++ (link_id) ++ "_" ++ (show count) ++ "\n\n" ++ sumList xs zs ys link_id (count + 1)


substring :: Int -> Int -> String -> String
substring start end text = take (end - start) (drop start text)


getJSON link = do
    let text = link
    
    let opts = defaults & param "link" .~ [T.pack text]
    
    request <- getWith opts "https://bestproger.ru/abot/linker.php"
    
    return $ Char8.unpack $ trim (request ^. responseBody)
  where trim = Char8.reverse . (Char8.dropWhile isSpace) . Char8.reverse . (Char8.dropWhile isSpace)


findStr :: String -> String -> Maybe Int
findStr sub s
 | length sub > length s = Nothing
 | take (length sub) s == sub = Just 0
 | otherwise = fmap (+1) $ findStr sub $ drop 1 s


getTitledLinks = do
  conn <- postgresConnection
  query_ conn "select * from links where title != ''" :: IO [(Int, String, String, Int, String)]


createLibraryReply :: [(Int, String, String, Int, String)] -> String
createLibraryReply ar = foldr (\(x, y, z, c, d) b -> "<b>" ++ d ++ "</b>\n\n" ++ b) mempty ar


getUsername id = do
  manager <- newManager tlsManagerSettings

  ret <- runTelegramClient myToken manager $ do
    getChatM (T.pack (show id))
  
  case ret of
    Right response -> do
      
      let username = (chat_username (result response))
      
      case username of
        
        Just u -> return ("@" ++ (T.unpack u))
        
        Nothing -> return "@"
    
    _ -> return ""


updateLastmes lastmes id = do
  
  conn <- postgresConnection
  
  rows <- execute conn "update users set lastmes=? where user_id=?" (lastmes :: String, id :: Int) :: IO GHC.Int.Int64
  
  return True


getLastmes id = do
  
  conn <- postgresConnection
  
  xs <- query conn "select lastmes from users where user_id=?" (Only id) :: IO [Only String]
  
  let lastmes = fromOnly (head xs)
  
  return lastmes


getLinkById link_id = do

  conn <- postgresConnection

  xs <- query conn "select link from links where id=?" (Only link_id) :: IO [Only String]

  let link = fromOnly (head xs)

  return link


mySendMessageHTML id text = do
  manager <- newManager tlsManagerSettings

  ret <- runTelegramClient myToken manager $ do
    let message_request = SendMessageRequest (ChatId id) text (Just HTML) Nothing Nothing Nothing Nothing

    sendMessageM message_request

  print "HTML message sent!"


mySendMessageWithButtons id text kb = do
  manager <- newManager tlsManagerSettings

  ret <- runTelegramClient myToken manager $ do
    let message_request = SendMessageRequest (ChatId id) text (Just HTML) Nothing Nothing Nothing (Just kb)
    
    sendMessageM message_request

  print "Message with buttons Sent!"


startButtons = myReplyKeyboardMarkup [[keyboardButton "🙏Получить ID", keyboardButton "☁️Скачать по ID"], [keyboardButton "📕 Библиотека", keyboardButton "📊 Статистика"], [keyboardButton "❓FAQ", keyboardButton "☎️Тех.Поддержка"]]

myReplyKeyboardMarkup keyboard = ReplyKeyboardMarkup keyboard (Just True) (Just False) (Just True)


inDataBase id = do
  conn <- postgresConnection

  xs <- query conn "select count(*) as count from users where user_id=?" (Only id) :: IO [Only Int]

  let count = fromOnly (head xs)

  print count

  if count > 0 then return True else return False


getStats = do
  conn <- postgresConnection

  xs <- query_ conn "select count(*) as count from users" :: IO [Only Int]

  let countUsers = fromOnly (head xs)

  ys <- query_ conn "select count(*) as count from links where title != ''" :: IO [Only Int]

  let countTitledLinks = fromOnly (head ys)

  print countUsers

  let res = "Количество пользователей бота: " ++ (show countUsers) ++ "\n\nКоличество курсов в библиотеке: " ++ (show countTitledLinks)
  return res
