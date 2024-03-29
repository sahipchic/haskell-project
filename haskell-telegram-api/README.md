# HiddenLinks Telegram Bot

Я - бот HiddenLinks и я единственный в природе бот, который прячет ссылки с таких файлобменников как:  
• YandexDisk  
• Облако Mail    

Для чего я это делаю? Все чаще и чаще рабочие ссылки с инфопродуктами блокируются правообладателями, и это сильно затрудняет обучение. 

Именно поэтому приходится прятать ссылки от злодеев, которые находятся всегда на страже и радовать вас бесперебойным доступом к любимым материалам.


**Support of [Bot-3.5 API](https://api.telegram.org/bots)**

## Usage

Данная реализация использования `Telegram API` построена на использовании `TelegramClient` и работе с локальной базой данных `PostgreSQL`

Также реализована пара вспомагательных файлов, которые помогают формировать скрытые ссылки, используя мой рабочий домен `https://bestproger.ru/abot` для данного бота

## Принцип работы бота

Допустим мы являетесь владельцем Telegram канала с инфопродуктами, ежедневно выкладываете посты с обучающими материалами, которые размещены на облаках Mail и Yandex.  
Данные материалы подвержены постоянным блокировкам, в среднем публичная ссылка вида `cloud.mail.ru` или `disk.yandex.ru` живут не больше 3-5 дней.  

Вы отправляете боту ссылку, которую хотите скрыть, он, при помощи библиотеки `Network.Wreq` формирует `GET-запрос`, перенапрвляет запрос на сервер, тот её парсит, шарит по коду страницы, смотрит на исходящие запросы при эмулировании нажатия на кнопку `Скачать`, вытаскивает так называемые _прямые ссылки на скачивание_ - ссылки вида `cloclo21.mail.ru/d/....`, при переходе на которые автоматически начинается скачивание необходимого вам файла.  
Скрипт на сервере возвращает `JSON` вида ```[{name: text, size: text, link: text}, ...]``` , далее , при помощи библиотеки `Aeson` парсится `JSON` ответ и кладется в базу данных - это так называемое временное хранилище ссылок вместе с временной меткой - это нужно затем, что ссылки, которые были вытащены, живут в среднем всего около 4 часов, и если между запросами прошло меньше времени, то гораздо выгоднее с точки зрения нагрузки будет достать их из базы, а не отправлять каждый раз новый запрос на сервер.

Также есть возможность добавить скрытый курс в **библиотеку** - место, где хранятся ID скрытых ссылок с описанием от автора.

Пример:  
![alt tag](https://sun9-38.userapi.com/c853524/v853524606/1021a9/9KpTlUBaomg.jpg "Библиотека")​

Чтобы получить необходимый инфокурс, достаточно нажать кнопку **Скачать по ID** , ввести ID и бот либо достанет `JSON` ответ из базы, если ссылки еще валидные, либо отправит запрос на сервер, получит актуальный ответ, спарсит его при помощи той же `Aeson` , сделает апдейт в базе, сформирует новые ссылки, используя рабочий домен, и вернет пользователю ответ в таком виде:

![alt tag](https://sun9-8.userapi.com/c853524/v853524606/1021d2/Ko1dFhlu5SY.jpg "Скачать по ID")​

При переходе по любой из сгенерированных ссылок начнется автоматическая загрузка файла на устройство, ссылка сразу же закроется, не давая отследить исходящий запрос.



По графическому интерфейсу - практически полностью все от Telegram, за исключением моих созданных через API кнопок:

![alt tag](https://sun9-47.userapi.com/c853524/v853524606/10227e/3WGB0MWlYGU.jpg "Buttons")​


Кнопка **Статистика** показывает статистику по пользователям и загруженным курсам, **FAQ** - описание принципа работы бота, и **Тех. Поддержка** - Just ссылка на разработчика.


## Основные использованные библиотеки

• [Web.Telegram.API.Bot](https://github.com/klappvisor/haskell-telegram-api) - соответственно основная библиотека для взаимодействия с `Telegram API`  
• Database.PostgreSQL.Simple - библиотека для взаимодействия с PostgreSQL  
• Network.Wreq - либа для отправки `GET` и `POST` запросов на сервер моему обработчику ссылок  
• Data.Aeson  
• Control.Concurrent



**Также ВАЖНО** перед запуском бота поставить на устройство какую-либо VPN программу, так как иначе в России из-за блокировки либа попросту работать не будет :(

Такие дела =)
