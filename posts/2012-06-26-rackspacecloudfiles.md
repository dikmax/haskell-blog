---
title: Загрузка файлов на Rackspace
date: 2012-06-26T10:44:38+03:00
tags: haskell, html, javascript, rackspace, кроссбраузерность, программирование
---

Я все-таки дописал этот функционал, хотя он отнял приличное количество времени. Попытаюсь поделиться своим опытом. Не буду рассказывать как реализовать получение списка файлов, т.к. в большей части это перекликается с самой загрузкой.

_Oсторожно, сейчас будет простыня._

Во-первых, нам нужна форма загрузки:

~~~~~html
<form class="well form-inline upload-form" action="/vault/fileupload" enctype="multipart/form-data" method="post">
  <strong>Загрузить: </strong>
  <input type="hidden" name="container" id="file-container" />
  <input type="file" name="file" id="file-upload"
    style="position: absolute; top: -100px; left: -100px;" />
  <button class="btn" type="button" id="file-button"> Выбрать </button>
  <input type="text" name="name" id="file-name" placeholder="Под именем" />
  <button class="btn" type="submit"> Отправить </button>
</form>
~~~~~

Мне захотелось кастомную кнопку выбора файлов, для этого оригинальную кнопку мы прячем, а клик по ней эмулируем:

~~~~~javascript
$('#file-button').click(function () {
    $('#file-upload').click();
    return false;
});
~~~~~

Стоит обратить внимание на способ, которым кнопка спрятана. Если просто указать `display: none`, то опера перестанет воспринимать поддельные клики и окошко с выбором файла перестанет появляться. А если указать `visibility: hidden`, то старое поле будет занимать положенное ему место и получится дырка на форме.

Итак, форму нарисовали, можно писать обработчик:

~~~~~haskell
vaultFileUpload :: AppHandler ()
vaultFileUpload = do
  tmpDir <- liftIO getTemporaryDirectory
  response <- handleFileUploads tmpDir uploadPolicy partUploadPolicy processForm
  writeBS $ pack $ encode response
~~~~~

Тут все просто: получаем имя временной папки, обрабатываем форму и выводим результат, поэтому посмотрим внимательнее на функцию обработки:

~~~~~haskell
response <- handleFileUploads tmpDir uploadPolicy partUploadPolicy processForm
~~~~~

Функция `handleFileUploads` принимает на вход 4 параметра: временная папка, глобальная политика загрузки, функция, возвращающая по описанию части локальную политику загрузки, и, наконец, функция для обработки полученных данных. Т.е. мы загружаем все или только некоторые файлы во временную папку и вызываем обработчик:

~~~~~haskell
processForm ((_, Left uploadError) : []) = 
  return $ ServiceError $ show uploadError

processForm ((_, Right path) : []) = do
  container <- getPostParam "container"
  name <- getPostParam "name"
  if container == Nothing || name == Nothing
    then return $ ServiceError "Container or name not defined"
    else liftIO $ curlDo $ UploadFile path (unpack $ fromMaybe "" container) (unpack $ fromMaybe "" name)

processForm [] = return $ ServiceError "No files were transmitted"
processForm _ = return $ ServiceError "Too many files"
~~~~~

Интересен только второй случай, когда успешно загружен только один файл. Мы получаем два POST-парамента (имя контейнера Rackspace CloudFiles и имя, под которым файл должен быть загружен) и вызываем нашу функцию `curlDo` с описанием нужной операции.  `UploadFile` --- это конструктор моего типа `ServiceAction`, где просто перечислены все необходимые операции:

~~~~~haskell
data ServiceAction
  = GetContainers
  | GetContainerItems String
  | UploadFile FilePath String String
~~~~~

Переходим к `curlDo`.

~~~~~haskell
curlDo :: ServiceAction -> IO ServiceResponse
curlDo action = withCurlDo $ do
    h <- initialize
    response <- curl h "https://auth.api.rackspacecloud.com/v1.0"
      [CurlHttpHeaders 
        [ "X-Auth-Key: "  ++ rackspaceAuthKey
        , "X-Auth-User: " ++ rackspaceAuthUser
        ]
      ]
    
    let headers = M.map (dropWhile (== ' ')) $ M.fromList $ respHeaders response
    case respStatus response of
      204 -> processAction action 
        (headers M.! "X-Storage-Url") (headers M.! "X-CDN-Management-Url") 
        (headers M.! "X-Auth-Token")
      _ -> return $ ServiceError "Can't authenticate"
~~~~~

Мы отправляем аутентификационные данные на Cloud-сервер и проверяем их правильность. В случае удачи вызывается `processAction` с действием, которое нужно выполнить. Ну и кроме того передаются некоторые заголовки из ответа сервера --- там указано, к каким серверам дальше обращаться.

В функции `processAction` несколько паттернов, по одному на каждую выполняемую операцию из типа `ServiceAction`. Вот тот, что используется для операции загрузки файла:

~~~~~haskell
processAction (UploadFile filePath container name) url _ =
  uploadFile filePath container name url
~~~~~

Как видите, ничего особенного, просто вызов. Переходим к самому интересному --- `uploadFile`. Каркас этой функции выглядит так:

~~~~~haskell
uploadFile filePath container name url token = do
  h <- initialize
  response <- withBinaryFile filePath ReadMode processFile
  case respStatus response of
    201 -> return ServiceSuccess
    _ -> return $ ServiceDebug $ show $ respStatus response
~~~~~

Сначала мы инициализируем handle от curl c помощью `initialize`, затем оборачиваем в `withBinaryFile` работу с файлом (отправку) и отдаем результат в зависимости от кода, возвращенного сервером. Отправка файла реализуется через `processFile`:

~~~~~haskell
processFile fh = do
  fileSize <- hFileSize fh
  curl h (url ++ "/" ++ container ++ "/" ++ name)
    [ CurlPut True
    , CurlHttpHeaders ["X-Auth-Token: " ++ token]
    , CurlReadFunction readFunction
    , CurlInFileSize $ fromInteger fileSize
    ]
~~~~~

Тут задаются опции для вызова curl. Самое интересное и сложное --- это функция `readFunction`, которая отвечает за чтение файла и передачу данных в curl. Т.к. функция будет вызываться из libcurl, написана она весьма специфически с использованием библиотеки `Foreign`:

~~~~~haskell
readFunction :: Ptr CChar -> CInt -> CInt -> Ptr () -> IO (Maybe CInt)
readFunction ptr size nmemb _ = do
  actualSize <- hGetBuf fh ptr $ fromInteger $ toInteger (size * nmemb)
  return $ if (actualSize > 0) then Just $ fromInteger $ toInteger actualSize else Nothing
~~~~~

`hGetBuf` читает из файла `fh` в область по указателю `pts` `(size * nmemb)` байт и возвращает количество действительно прочитанных байт. Ну и сама функция должна вернуть `Maybe CInt`, причем `Nothing` возвращается в случае, если ничего не прочитано и читать дальше не надо.

Если собрать весь код функции в одно место, получится как-то так:

~~~~~haskell
uploadFile filePath container name url token = do
  h <- initialize
  let 
    processFile fh = do
      let 
        readFunction :: Ptr CChar -> CInt -> CInt -> Ptr () -> IO (Maybe CInt)
        readFunction ptr size nmemb _ = do
          actualSize <- hGetBuf fh ptr $ fromInteger $ toInteger (size * nmemb)
          return $ if (actualSize > 0) then Just $ fromInteger $ toInteger actualSize else Nothing

      fileSize <- hFileSize fh
      curl h (url ++ "/" ++ container ++ "/" ++ name)
        [ CurlPut True
        , CurlHttpHeaders ["X-Auth-Token: " ++ token]
        , CurlReadFunction readFunction
        , CurlInFileSize $ fromInteger fileSize
        ]

  response <- withBinaryFile filePath ReadMode processFile
  case respStatus response of
    201 -> return ServiceSuccess
    _ -> return $ ServiceDebug $ show $ respStatus response
~~~~~

Вот такими нехитрыми действиями можно пересылать файлы с клиента через промежуточный сервер на сервера Rackspace CloudFiles. Все исходники [лежат на GitHub](https://github.com/dikmax/haskell-blog), где и подлежат пристальному и вдумчивому изучению (trollface).